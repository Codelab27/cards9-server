package com.codelab27.cards9.controllers

import com.codelab27.cards9.models.common.Common.Color.{Blue, Red}
import com.codelab27.cards9.models.matches.Match
import com.codelab27.cards9.models.matches.Match.PlayerAction.{Join, Leave, Ready, Start}
import com.codelab27.cards9.models.matches.Match._
import com.codelab27.cards9.models.players.Player
import com.codelab27.cards9.services.matchmaking.MatchMaker

import io.kanaka.monadic.dsl._

import cats.Bimonad
import cats.arrow.FunctionK
import cats.data.OptionT

import play.api.libs.json.Json
import play.api.mvc.{AbstractController, ControllerComponents}

import scala.concurrent.Future

class MatchMakerController[F[_] : Bimonad](
    cc: ControllerComponents,
    matchMaker: MatchMaker[F]
)(implicit fshandler: FunctionK[F, Future]) extends AbstractController(cc) {

  implicit val ec = cc.executionContext

  import com.codelab27.cards9.serdes.json.DefaultFormats._
  import com.codelab27.cards9.utils.DefaultStepOps._

  import cats.syntax.comonad._
  import cats.syntax.functor._

  def getMatchesForState(state: MatchState) = Action {

    val foundMatches = matchMaker.findMatches(state)

    Ok(Json.toJson(foundMatches.extract))

  }

  private def playingOrWaitingMatches(playerId: Player.Id): F[Seq[Match]] = for {
    matches <- matchMaker.findMatchesForPlayer(playerId)
  } yield {
    matches.filter(m => MatchState.isPlayingOrWaiting(m.state))
  }

  def createMatch(playerId: Player.Id) = Action.async { implicit request =>

    def createMatchForPlayer(playerId: Player.Id) = {

      val theMatch = Match(Some(RedPlayer(playerId, IsReady(false))), None, MatchState.Waiting, None, None)

      OptionT(matchMaker.storeMatch(theMatch))

    }

    for {
      // TODO validate player existence
      matchId <- createMatchForPlayer(playerId).step  ?| (_ => Conflict(s"Could not create a match"))
    } yield {
      Ok(Json.toJson(matchId))
    }

  }

  def playerActionOnMatch(id: Match.Id, playerId: Player.Id, action: PlayerAction) = action match {

    case Join   => joinMatch(id, playerId)
    case Leave  => leaveMatch(id, playerId)
    case Ready  => playerReady(id, playerId)
    case Start  => ???

  }

  private def joinMatch(id: Match.Id, playerId: Player.Id) = Action.async { implicit request =>

    def addPlayerToMatch(playerId: Player.Id, theMatch: Match): OptionT[F, Match] = {

      lazy val alreadyInThisMatch = Match.isPlayerInMatch(theMatch, playerId)

      val updatedMatch = for {
        _     <- Option(theMatch.state == MatchState.Waiting && alreadyInThisMatch.isEmpty).filter(identity)
        color <- Match.emptySlot(theMatch)
      } yield {

        val matchWithNewPlayer = color match {
          case Red  => theMatch.copy(red = Some(RedPlayer(playerId, IsReady(false))))
          case Blue => theMatch.copy(blue = Some(BluePlayer(playerId, IsReady(false))))
        }

        matchWithNewPlayer.copy(state = MatchState.SettingUp)
      }

      OptionT.fromOption(updatedMatch)
    }

    for {
      theMatch      <- OptionT(matchMaker.findMatch(id)).step             ?| (_ => NotFound(s"Match with identifier ${id.value} not found"))
      updatedMatch  <- addPlayerToMatch(playerId, theMatch).step          ?| (_ => Conflict(s"Could not add player ${playerId.value} to match ${id.value}"))
      _             <- OptionT(matchMaker.storeMatch(updatedMatch)).step  ?| (_ => Conflict(s"Could not update the match"))
    } yield {
      Ok(Json.toJson(updatedMatch))
    }

  }

  private def leaveMatch(id: Match.Id, playerId: Player.Id) = Action.async { implicit request =>

    def removePlayerFromMatch(playerId: Player.Id, theMatch: Match): OptionT[F, Match] = {

      val removedPlayer = for {
        _       <- Option(theMatch.state == MatchState.SettingUp || theMatch.state == MatchState.Waiting).filter(identity)
        player  <- Match.isPlayerInMatch(theMatch, playerId)
      } yield {

        val matchWithPlayers = player match {
          case _: RedPlayer   => theMatch.copy(red = None)
          case _: BluePlayer  => theMatch.copy(blue = None)
        }

        if (matchWithPlayers.red.isEmpty && matchWithPlayers.blue.isEmpty) {
          matchWithPlayers.copy(state = MatchState.Aborted)
        } else {
          matchWithPlayers.copy(state = MatchState.Waiting)
        }

      }

      OptionT.fromOption(removedPlayer)
    }

    for {
      theMatch      <- OptionT(matchMaker.findMatch(id)).step             ?| (_ => NotFound(s"Match with identifier ${id.value} not found"))
      updatedMatch  <- removePlayerFromMatch(playerId, theMatch).step     ?| (_ => Conflict(s"Could not remove player ${playerId.value} from match ${id.value}"))
      _             <- OptionT(matchMaker.storeMatch(updatedMatch)).step  ?| (_ => Conflict(s"Could not update the match"))
    } yield {
      Ok(Json.toJson(updatedMatch))
    }

  }

  private def playerReady(id: Match.Id, playerId: Player.Id) = Action.async { implicit request =>

    def switchPlayerReady(playerId: Player.Id, theMatch: Match): OptionT[F, Match] = {

      val readyToPlay = for {
        _       <- Option(theMatch.state == MatchState.SettingUp || theMatch.state == MatchState.Waiting).filter(identity)
        player  <- Match.isPlayerInMatch(theMatch, playerId)
      } yield {

        player match {
          case redPlayer: RedPlayer   => theMatch.copy(red = Some(redPlayer.copy(ready = IsReady(!redPlayer.ready.value))))
          case bluePlayer: BluePlayer => theMatch.copy(blue = Some(bluePlayer.copy(ready = IsReady(!bluePlayer.ready.value))))
        }

      }

      OptionT.fromOption(readyToPlay)
    }

    for {
      theMatch      <- OptionT(matchMaker.findMatch(id)).step             ?| (_ => NotFound(s"Match with identifier ${id.value} not found"))
      updatedMatch  <- switchPlayerReady(playerId, theMatch).step         ?| (_ => Conflict(s"Could not switch player ${playerId.value} ready from match ${id.value}"))
      _             <- OptionT(matchMaker.storeMatch(updatedMatch)).step  ?| (_ => Conflict(s"Could not update the match"))
    } yield {
      Ok(Json.toJson(updatedMatch))
    }

  }

}
