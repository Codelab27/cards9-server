package com.codelab27.cards9.controllers

import com.codelab27.cards9.models.matches.Match
import com.codelab27.cards9.models.matches.Match.MatchState
import com.codelab27.cards9.models.matches.Match.MatchState.SettingUp
import com.codelab27.cards9.models.players.Player
import com.codelab27.cards9.services.matchmaking.MatchMaker

import io.kanaka.monadic.dsl._

import cats.Bimonad
import cats.arrow.FunctionK
import cats.data.OptionT

import play.api.libs.json.{JsValue, Json}
import play.api.mvc.{AbstractController, ControllerComponents, Request}

import scala.concurrent.Future

class MatchMakerController[F[_] : Bimonad](
    cc: ControllerComponents,
    matchMaker: MatchMaker[F]
)(implicit fshandler: FunctionK[F, Future]) extends AbstractController(cc) {

  implicit val ec = cc.executionContext

  import com.codelab27.cards9.serdes.json.DefaultFormats._

  import cats.syntax.comonad._
  import cats.syntax.functor._

  import com.codelab27.cards9.utils.DefaultStepOps._

  ///////////////////////
  /// Body readers
  private def readPlayer(request: Request[JsValue]) = (request.body \ "playerId").validate[Player.Id]

  def getMatchesForState(state: MatchState) = Action {

    val foundMatches = matchMaker.findMatches(state)

    Ok(Json.toJson(foundMatches.extract))

  }

  private def playingOrWaitingMatches(playerId: Player.Id): F[Seq[Match]] = for {
    matches <- matchMaker.findMatchesForPlayer(playerId)
  } yield {
    matches.filter(m => MatchState.isPlayingOrWaiting(m.state))
  }

  def createMatch() = Action.async(parse.json) { implicit request =>

    def createMatchForPlayer(playerId: Player.Id) = {

      val theMatch = Match(Some(playerId), None, MatchState.Waiting, None, None)

      OptionT(matchMaker.storeMatch(theMatch))

    }

    for {
      playerId  <- readPlayer(request)                  ?| (jserrs  => BadRequest(s"Error parsing player id: ${jserrs.seq}"))
      // TODO validate player existence
      matchId   <- createMatchForPlayer(playerId).step  ?| (_       => Conflict(s"Could not create a match"))
    } yield {
      Ok(Json.toJson(matchId))
    }

  }

  def joinMatch(id: Match.Id) = Action.async(parse.json) { implicit request =>

    def addPlayerToMatch(playerId: Player.Id, theMatch: Match) = {

      val (red, blue) = (theMatch.red, theMatch.blue) match {
        case (None, None)                             => (Some(playerId), None) // Probably never reaching this case
        case (r @ Some(rid), None) if rid != playerId => (r, Some(playerId))    // Normal
        case (None, b @ Some(rid)) if rid != playerId => (Some(playerId), b)    // Normal
        case (_, _)                                   => (None, None)           // Full or repeated player id :(
      }

      lazy val matchUpdated = theMatch.copy(red = red, blue = blue, state = SettingUp)

      OptionT.fromOption(
        Option((red.isEmpty && blue.isEmpty) || theMatch.state != MatchState.Waiting).collect {
          case false => matchUpdated
        }
      )

    }

    for {
      playerId      <- readPlayer(request)                                ?| (jserrs  => BadRequest(s"Error parsing player id: ${jserrs.seq}"))
      theMatch      <- OptionT(matchMaker.findMatch(id)).step             ?| (_       => NotFound(s"Match with identifier ${id.value} not found"))
      updatedMatch  <- addPlayerToMatch(playerId, theMatch).step          ?| (_ => Conflict(s"Could not add player to match ${id.value}"))
      _             <- OptionT(matchMaker.storeMatch(updatedMatch)).step  ?| (_  => Conflict(s"Could not update the match"))
    } yield {
      Ok(Json.toJson(updatedMatch))
    }

  }

}
