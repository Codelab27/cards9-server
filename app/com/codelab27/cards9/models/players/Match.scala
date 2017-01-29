package com.codelab27.cards9.models.players

import com.codelab27.cards9.models.boards.{ Board, Red, Blue }
import com.codelab27.cards9.models.boards.Board._
import com.codelab27.cards9.models.cards.Fight
import com.codelab27.cards9.game.GameTypes._

case class Match(
  player1: PlayerId, // Red player
  player2: PlayerId, // Blue player
  board: Board,
  fights: List[Fight] = Nil) {

  /**
   * Adds a new fight to the match.
   *
   * @param fight the new fight
   *
   * @return match with the fight added
   */
  def addFight(fight: Fight): Match = this.copy(fights = fights :+ fight)

  /**
   * Get the current score of the match.
   *
   * @return a tuple with scores (Red, Blue)
   */
  def score: Score = (cardsOf(board,Red).length, cardsOf(board,Blue).length)
}
