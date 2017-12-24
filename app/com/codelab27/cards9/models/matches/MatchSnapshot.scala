package com.codelab27.cards9.models.matches

import com.codelab27.cards9.models.boards.Board
import com.codelab27.cards9.models.cards.Fight

/**
  * Snapshot of a match.
  *
  * @param board distribution of cards and blocks
  * @param fights list of already computed fights
  */
final case class MatchSnapshot(
    board: Board,
    fights: List[Fight],
    id: Option[MatchSnapshot.Id]
)

object MatchSnapshot {

  case class Id(value: String) extends AnyVal

}
