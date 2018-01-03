package com.codelab27.cards9.game.engines

import com.codelab27.cards9.game.engines.board._
import com.codelab27.cards9.models.cards.Fight
import com.codelab27.cards9.models.common.Common.Color
import com.codelab27.cards9.models.common.Common.Color.{Blue, Red}
import com.codelab27.cards9.models.matches.Match
import com.codelab27.cards9.models.matches.Match.MatchState._
import com.codelab27.cards9.models.matches.Match._
import com.codelab27.cards9.models.players.Player

trait MatchEngine {

  /**
    * Adds a new fight to the match.
    *
    * @param theMatch the match that receives the new fight
    * @param fight    the new fight
    * @return match with the fight added
    */
  def addFight(theMatch: Match, fight: Fight): Match = {

    val attemptAddingFight = for {
      snapshot <- theMatch.snapshot
    } yield {
      theMatch.copy(snapshot = Some(snapshot.copy(fights = snapshot.fights :+ fight)))
    }

    attemptAddingFight.getOrElse(theMatch)

  }

  /**
    * Get the current score of the match.
    *
    * @param theMatch match from where the score is extracted
    * @return the current score
    */
  def score(theMatch: Match): Score = {

    val attemptScoreRetrieval = for {
      snapshot <- theMatch.snapshot
    } yield {
      val redScore = RedScore(cardsOf(snapshot.board, Red).length)
      val blueScore = BlueScore(cardsOf(snapshot.board, Blue).length)

      Score(redScore, blueScore)
    }

    attemptScoreRetrieval.getOrElse(Score(RedScore(0), BlueScore(0)))

  }

  /**
    * Checks whether the player is involved in the match, and retrieves the red or blue slot accordingly.
    *
    * @param theMatch match to be checked
    * @param playerId player identifier
    * @return red/blue slot, or none
    */
  def playerInMatch(theMatch: Match, playerId: Player.Id): Option[Color] = {
    val redSlot       = for (redPlayer <- theMatch.red if redPlayer.id == playerId) yield Red
    lazy val blueSlot = for (bluePlayer <- theMatch.blue if bluePlayer.id == playerId) yield Blue

    redSlot.orElse(blueSlot)
  }

  /**
    * Retrieves the color of the first empty slot for a player.
    *
    * @param theMatch match to be checked
    * @return red/blue, or no color at all
    */
  def emptySlot(theMatch: Match): Option[Color] = {
    val redSlot       = for (_ <- Option(theMatch.red.isEmpty).filter(identity)) yield Red
    lazy val blueSlot = for (_ <- Option(theMatch.blue.isEmpty).filter(identity)) yield Blue

    redSlot.orElse(blueSlot)
  }

  /**
    * Checks if the match is currently in a playing or waiting state.
    *
    * @param theMatch
    * @return
    */
  def isPlayingOrWaiting(theMatch: Match): Boolean = theMatch.state match {
    case Paused | Aborted | Finished              => false
    case Waiting | SettingUp | Starting | Ongoing => true
  }

  /**
    * Fresh match without players, waiting.
    */
  val freshMatch: Match = Match(None, None, MatchState.Waiting, None, None)

  /**
    * Creates a new match for the player.
    *
    * @param playerId the player identifier
    * @return a fresh match without identifier
    */
  def createMatchForPlayer(playerId: Player.Id): Match = {
    Match(Some(RedPlayer(playerId, IsReady(false))), None, MatchState.Waiting, None, None)
  }

  /**
    * Switches the readiness of the player on the match.
    *
    * @param theMatch the match to be updated
    * @param color the player of the color switching its readiness
    * @param isReady the new readiness of the player
    * @return match updated with the player changed, if possible
    */
  def switchPlayerReadiness(theMatch: Match, color: Color, isReady: IsReady): Option[Match] = {

    for {
      _       <- Option(theMatch.state == MatchState.SettingUp || theMatch.state == MatchState.Waiting).filter(identity)
      player  <- color match {
        case Red  => theMatch.red
        case Blue => theMatch.blue
      }
    } yield {

      // TODO use a lens here
      player match {
        case redPlayer: RedPlayer   => theMatch.copy(red = Some(redPlayer.copy(ready = isReady)))
        case bluePlayer: BluePlayer => theMatch.copy(blue = Some(bluePlayer.copy(ready = isReady)))
      }

    }

  }

  /**
    * Removes the player from the red/blue slot and transitions to a properly match state.
    *
    * @param theMatch the match to be updated
    * @param color the colored slot to be removed
    * @return match updated with players and state changed, if possible
    */
  def removePlayerFromMatch(theMatch: Match, color: Color): Option[Match] = {

    for {
      _       <- Option(theMatch.state == MatchState.SettingUp || theMatch.state == MatchState.Waiting).filter(identity)
    } yield {

      val matchWithPlayers = color match {
        case Red  => theMatch.copy(red = None)
        case Blue => theMatch.copy(blue = None)
      }

      if (matchWithPlayers.red.isEmpty && matchWithPlayers.blue.isEmpty) {
        matchWithPlayers.copy(state = MatchState.Aborted)
      } else {
        matchWithPlayers.copy(state = MatchState.Waiting)
      }

    }
  }

  /**
    * Adds the player to the red/blue slot and transitions to a properly match state.
    *
    * @param theMatch the match to where we want to add the player
    * @param playerId the player identifier
    * @return match updated withs players and state changed, if possible
    */
  def addPlayerToMatch(theMatch: Match, color: Color, playerId: Player.Id): Option[Match] = {

    lazy val alreadyInThisMatch = playerInMatch(theMatch, playerId)

    val updatedMatch = for {
      _     <- Option(theMatch.state == MatchState.Waiting).filter(identity)
      _     <- Option(alreadyInThisMatch.forall(_ == color)).filter(identity)
    } yield {

      val matchWithNewPlayer = color match {
        case Red  => theMatch.copy(red = Some(RedPlayer(playerId, IsReady(false))))
        case Blue => theMatch.copy(blue = Some(BluePlayer(playerId, IsReady(false))))
      }

      if (matchWithNewPlayer.red.isDefined && matchWithNewPlayer.blue.isDefined) {
        matchWithNewPlayer.copy(state = MatchState.SettingUp)
      } else {
        matchWithNewPlayer
      }
    }

    updatedMatch
  }

}
