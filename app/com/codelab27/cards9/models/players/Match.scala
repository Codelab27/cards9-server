package com.codelab27.cards9.models.players

import com.codelab27.cards9.models.boards.Board
import com.codelab27.cards9.models.cards.Fight
import com.codelab27.cards9.models.players.Match.MatchState

/**
  * A cards match.
  *
  * @param red player one identifier
  * @param blue player two identifier
  * @param board the board
  * @param state the current state of the match
  * @param fights list of already computed fights
  */
final case class Match(
  red: Option[Player.Id],
  blue: Option[Player.Id],
  board: Board,
  state: MatchState,
  fights: List[Fight],
  id: Option[Match.Id])

object Match {

  case class Id(value: String) extends AnyVal

  case class RedScore(value: Int) extends AnyVal

  case class BlueScore(value: Int) extends AnyVal

  case class Score(red: RedScore, blue: BlueScore)

  sealed trait MatchState

  object MatchState {

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

  }

}
