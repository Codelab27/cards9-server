package com.codelab27.cards9.controllers

import com.codelab27.cards9.game.engines
import com.codelab27.cards9.models.common.Common.Color
import com.codelab27.cards9.models.matches.Match
import com.codelab27.cards9.models.matches.Match._
import com.codelab27.cards9.models.players.Player
import com.codelab27.cards9.repos.matches.MatchRepository

import io.kanaka.monadic.dsl._

import cats.Bimonad
import cats.arrow.FunctionK
import cats.data.OptionT

import play.api.libs.json.Json
import play.api.mvc.{AbstractController, ControllerComponents}

import scala.concurrent.Future

class MatchMakerController[F[_] : Bimonad](
    cc: ControllerComponents,
    matchRepo: MatchRepository[F]
)(implicit fshandler: FunctionK[F, Future]) extends AbstractController(cc) {

  implicit val ec = cc.executionContext

  import com.codelab27.cards9.serdes.json.DefaultFormats._
  import com.codelab27.cards9.utils.DefaultStepOps._

  import cats.syntax.comonad._
  import cats.syntax.functor._

  private def playingOrWaitingMatches(playerId: Player.Id): F[Seq[Match]] = for {
    foundMatches <- matchRepo.findMatchesForPlayer(playerId)
  } yield {
    foundMatches.filter(engines.matches.isPlayingOrWaiting)
  }

  def createMatch() = Action.async { implicit request =>

    for {
      matchId <- OptionT(matchRepo.storeMatch(engines.matches.freshMatch)).step ?| (_ => Conflict("Could not create a new match"))
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

  private def updateMatch(id: Match.Id)(performUpdate: Match => Option[Match]) = {
    Action.async { implicit request =>

      for {
        theMatch      <- OptionT(matchRepo.findMatch(id)).step            ?| (_ => NotFound(s"Match with identifier ${id.value} not found"))
        updatedMatch  <- OptionT.fromOption(performUpdate(theMatch)).step ?| (_ => Conflict(s"Could not perform action on match ${id.value}"))
        _             <- OptionT(matchRepo.storeMatch(updatedMatch)).step ?| (_ => Conflict(s"Could not update the match"))
      } yield {
        Ok(Json.toJson(updatedMatch))
      }

    }
  }

  def addPlayer(id: Match.Id, color: Color, playerId: Player.Id) = {
    updateMatch(id)(theMatch => engines.matches.addPlayerToMatch(theMatch, color, playerId))
  }

  def removePlayer(id: Match.Id, color: Color) = {
    updateMatch(id)(theMatch => engines.matches.removePlayerFromMatch(theMatch, color))
  }

  def setReadiness(id: Match.Id, color: Color, isReady: IsReady) = {
    updateMatch(id)(theMatch => engines.matches.switchPlayerReadiness(theMatch, color, isReady))
  }

}
