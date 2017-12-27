package com.codelab27.cards9.controllers

import com.codelab27.cards9.models.matches.Match
import com.codelab27.cards9.models.matches.Match.MatchState
import com.codelab27.cards9.models.matches.Match.MatchState.SettingUp
import com.codelab27.cards9.models.players.Player
import com.codelab27.cards9.services.matchmaking.MatchMaker

import cats.data.OptionT
import cats.syntax.option._
import cats.{Comonad, Monad}
import io.kanaka.monadic.dsl._

import play.api.libs.json.{JsValue, Json}
import play.api.mvc.{AbstractController, ControllerComponents, Request}

class MatchMakerController[F[_] : Monad](
    cc: ControllerComponents,
    matchMaker: MatchMaker[F]
)(implicit evf: Comonad[F]) extends AbstractController(cc) {

  import com.codelab27.cards9.serdes.json.DefaultFormats._

  implicit val ec = cc.executionContext

  import com.codelab27.cards9.utils.DefaultStepOps._

  def readPlayer(request: Request[JsValue]) = (request.body \ "playerId").validate[Player.Id]

  def getMatchesForState(state: MatchState) = Action {

    val attemptMatchesRetrieval: F[Seq[Match]] = matchMaker.findMatches(state)

    Ok(Json.toJson(evf.extract(attemptMatchesRetrieval)))

  }

  def createMatch() = Action.async(parse.json) { implicit request =>

    def storeMatchForPlayer(playerId: Player.Id) = {
      val theMatch = Match(Some(playerId), None, MatchState.Waiting, None, None)
      OptionT(matchMaker.storeMatch(theMatch))
    }

    for {
      playerId  <- readPlayer(request)            ?| (jserrs  => BadRequest(s"Error parsing player id: ${jserrs.seq}"))
      // TODO validate player existence
      matchId   <- storeMatchForPlayer(playerId)  ?| (_       => Conflict(s"Could not create a match"))
    } yield {
      Ok(Json.toJson(matchId))
    }

  }

  def joinMatch(id: Match.Id) = Action.async(parse.json) { implicit request =>

    def findMatch(matchId: Match.Id) = {
      val theMatch = matchMaker.findMatch(Some(matchId))
      OptionT(theMatch)
    }

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
      playerId      <- readPlayer(request) ?| (jserrs  => BadRequest(s"Error parsing player id: ${jserrs.seq}"))
      theMatch      <- findMatch(id)       ?| (_       => NotFound(s"Match with identifier ${id.value} not found"))
      updatedMatch  <- addPlayerToMatch(playerId, theMatch) ?| (_ => Conflict(s"Could not add player to match ${id.value}"))
      _             <- OptionT(matchMaker.storeMatch(updatedMatch))  ?| (_  => Conflict(s"Could not update the match"))
    } yield {
      Ok(Json.toJson(updatedMatch))
    }

  }

}
