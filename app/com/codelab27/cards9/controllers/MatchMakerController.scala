package com.codelab27.cards9.controllers

import com.codelab27.cards9.game.engines
import com.codelab27.cards9.models.common.Common.Color
import com.codelab27.cards9.models.matches.Match._
import com.codelab27.cards9.models.matches.{Match, MatchRoomEvent}
import com.codelab27.cards9.models.players.Player
import com.codelab27.cards9.repos.matches.MatchRepository

import akka.stream.Materializer
import akka.stream.scaladsl.{Flow, Keep, Sink, Source}
import io.kanaka.monadic.dsl._

import cats.Bimonad
import cats.arrow.FunctionK
import cats.data.OptionT

import play.api.libs.json.Json
import play.api.mvc.{AbstractController, ControllerComponents, WebSocket}

import scala.concurrent.Future

class MatchMakerController[F[_] : Bimonad](
    cc: ControllerComponents,
    matchRepo: MatchRepository[F]
)(implicit fshandler: FunctionK[F, Future], materializer: Materializer) extends AbstractController(cc) {

  implicit val ec = cc.executionContext

  import com.codelab27.cards9.serdes.json.DefaultFormats._
  import com.codelab27.cards9.utils.DefaultStepOps._

  import cats.syntax.comonad._
  import cats.syntax.functor._

  val bufferSize = 100

  val overflowStrategy = akka.stream.OverflowStrategy.dropHead

  val (roomEventsQueue, roomEventsPub) = Source.queue[MatchRoomEvent](bufferSize, overflowStrategy)
    .toMat(Sink.asPublisher(true))(Keep.both).run()

  val roomEventsFlow = Flow.fromSinkAndSource(Sink.ignore, Source.fromPublisher(roomEventsPub))

  private def playingOrWaitingMatches(playerId: Player.Id): F[Seq[Match]] = for {
    foundMatches <- matchRepo.findMatchesForPlayer(playerId)
  } yield {
    foundMatches.filter(engines.matches.isPlayingOrWaiting)
  }

  def createMatch() = Action.async { implicit request =>

    for {
      matchId <- OptionT(matchRepo.storeMatch(engines.matches.freshMatch)).step ?| (_ => Conflict("Could not create a new match"))
      _       <- roomEventsQueue.offer(MatchRoomEvent.MatchCreated(matchId))    ?| (_ => Conflict(s"Cannot publish event match creation to room"))
    } yield {
      Ok(Json.toJson(matchId))
    }

  }

  def retrieveMatch(matchId: Match.Id) = Action.async { implicit request =>

    for {
      theMatch <- OptionT(matchRepo.findMatch(matchId)).step ?| (_ => NotFound(s"Could not find match with id ${matchId.value}"))
    } yield {
      Ok(Json.toJson(theMatch))
    }

  }

  def getMatchesForState(stateOrError: Either[String, MatchState]) = Action.async {

    for {
      state   <- stateOrError ?| (err => BadRequest(err))
    } yield {
      val foundMatches = matchRepo.findMatches(state)

      Ok(Json.toJson(foundMatches.extract))
    }

  }

  private def updateMatch(id: Match.Id, event: MatchRoomEvent)(performUpdate: Match => Option[Match]) = {
    Action.async { implicit request =>

      for {
        theMatch      <- OptionT(matchRepo.findMatch(id)).step            ?| (_ => NotFound(s"Match with identifier ${id.value} not found"))
        updatedMatch  <- OptionT.fromOption(performUpdate(theMatch)).step ?| (_ => Conflict(s"Could not perform action on match ${id.value}"))
        _             <- OptionT(matchRepo.storeMatch(updatedMatch)).step ?| (_ => Conflict(s"Could not update the match"))
        _             <- roomEventsQueue.offer(event)                     ?| (_ => Conflict(s"Cannot publish event ${event} to room"))
      } yield {
        Ok(Json.toJson(updatedMatch))
      }

    }
  }

  def addPlayer(id: Match.Id, color: Color, playerId: Player.Id) = {
    val event = MatchRoomEvent.PlayerJoin(id, color, playerId)

    updateMatch(id, event)(theMatch => engines.matches.addPlayerToMatch(theMatch, color, playerId))
  }

  def removePlayer(id: Match.Id, color: Color) = {
    val event = MatchRoomEvent.PlayerLeave(id, color)

    updateMatch(id, event)(theMatch => engines.matches.removePlayerFromMatch(theMatch, color))
  }

  def setReadiness(id: Match.Id, color: Color, isReady: IsReady) = {
    val event = MatchRoomEvent.PlayerIsReady(id, color, isReady)

    updateMatch(id, event)(theMatch => engines.matches.switchPlayerReadiness(theMatch, color, isReady))
  }

  private def roomEventsTransformer = WebSocket.MessageFlowTransformer.jsonMessageFlowTransformer[String, MatchRoomEvent]

  def subscribeRoomEvents = WebSocket.accept(_ => roomEventsFlow)(roomEventsTransformer)

}
