package com.codelab27.cards9.controllers

import com.codelab27.cards9.game.engines
import com.codelab27.cards9.models.matches.Match
import com.codelab27.cards9.models.matches.Match.PlayerAction.{Join, Leave, Ready, Start}
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

  def getMatchesForState(state: MatchState) = Action {

    val foundMatches = matchRepo.findMatches(state)

    Ok(Json.toJson(foundMatches.extract))

  }

  private def playingOrWaitingMatches(playerId: Player.Id): F[Seq[Match]] = for {
    foundMatches <- matchRepo.findMatchesForPlayer(playerId)
  } yield {
    foundMatches.filter(engines.matches.isPlayingOrWaiting)
  }

  def createMatch(playerId: Player.Id) = Action.async { implicit request =>

    val theMatch = engines.matches.createMatchForPlayer(playerId)

    for {
      // TODO validate player existence
      matchId <- OptionT(matchRepo.storeMatch(theMatch)).step ?| (_ => Conflict(s"Could not create a match"))
    } yield {
      Ok(Json.toJson(matchId))
    }

  }

  private def updateMatchWithPlayer(id: Match.Id, playerId: Player.Id)(update: (Match, Player.Id) => Option[Match]) = {
    Action.async { implicit request =>

      def performUpdate(theMatch: Match) = OptionT.fromOption(update(theMatch, playerId))

      for {
        theMatch      <- OptionT(matchRepo.findMatch(id)).step            ?| (_ => NotFound(s"Match with identifier ${id.value} not found"))
        updatedMatch  <- performUpdate(theMatch).step                     ?| (_ => Conflict(s"Could not perform action on player ${playerId.value} of match ${id.value}"))
        _             <- OptionT(matchRepo.storeMatch(updatedMatch)).step ?| (_ => Conflict(s"Could not update the match"))
      } yield {
        Ok(Json.toJson(updatedMatch))
      }

    }
  }

  def playerActionOnMatch(id: Match.Id, playerId: Player.Id, action: PlayerAction) = {

    val actionOnMatchAndPlayer = updateMatchWithPlayer(id, playerId) _

    action match {
      case Join   => actionOnMatchAndPlayer(engines.matches.addPlayerToMatch)
      case Leave  => actionOnMatchAndPlayer(engines.matches.removePlayerFromMatch)
      case Ready  => actionOnMatchAndPlayer(engines.matches.switchPlayerReadiness)
      case Start  => ???
    }

  }

}
