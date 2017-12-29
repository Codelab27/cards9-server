package com.codelab27.cards9.models.matches

import com.codelab27.cards9.models.common.Common.Color
import com.codelab27.cards9.models.common.Common.Color.{Blue, Red}
import com.codelab27.cards9.models.matches.Match.{BluePlayer, MatchState, RedPlayer}
import com.codelab27.cards9.models.players.Player

import enumeratum._

/**
  * A cards match.
  *
  * @param red player one identifier
  * @param blue player two identifier
  * @param state current state of the match
  * @param snapshot snapshot including the board and fights
  * @param id unique identifier of the match
  */
final case class Match(
    red: Option[RedPlayer],
    blue: Option[BluePlayer],
    state: MatchState,
    snapshot: Option[MatchSnapshot],
    id: Option[Match.Id]
)

object Match {

  case class Id(value: String) extends AnyVal

  case class RedScore(value: Int) extends AnyVal

  case class BlueScore(value: Int) extends AnyVal

  case class Score(red: RedScore, blue: BlueScore)

  sealed trait ColoredPlayer

  case class RedPlayer(id: Player.Id) extends ColoredPlayer

  case class BluePlayer(id: Player.Id) extends ColoredPlayer

  def isPlayerInMatch(theMatch: Match, player: Player.Id): Option[ColoredPlayer] = {
    val redSlot = for (redPlayer <- theMatch.red if redPlayer.id == player) yield redPlayer
    lazy val blueSlot = for (bluePlayer <- theMatch.blue if bluePlayer.id == player) yield bluePlayer

    redSlot.orElse(blueSlot)
  }

  def emptySlot(theMatch: Match): Option[Color] = {
    val redSlot = for (_ <- Option(theMatch.red.isEmpty).filter(identity)) yield Red
    lazy val blueSlot = for (_ <- Option(theMatch.blue.isEmpty).filter(identity)) yield Blue

    redSlot.orElse(blueSlot)
  }

  sealed trait MatchState extends EnumEntry

  object MatchState extends Enum[MatchState] {

    val values = findValues

    // Waiting for an opponent
    case object Waiting extends MatchState

    // Setting up the decks, settings, etc.
    case object SettingUp extends MatchState

    // Allocating resources (from setting up or paused)
    case object Starting extends MatchState

    // Ongoing
    case object Ongoing extends MatchState

    // Currently not being played
    case object Paused extends MatchState

    // Finished without completion (possible penalties)
    case object Aborted extends MatchState

    // Finished and completed with score and cards exchange
    case object Finished extends MatchState

    def isPlayingOrWaiting(state: MatchState): Boolean = state match {
      case Paused | Aborted | Finished              => false
      case Waiting | SettingUp | Starting | Ongoing => true
    }

  }

}
