package com.codelab27.cards9.models.players

import com.codelab27.cards9.models.boards.{ Board, Red, Blue }
import com.codelab27.cards9.models.boards.Board._
import com.codelab27.cards9.models.cards.Fight

case class Match(
  player1: Player.Id, // Red player
  player2: Player.Id, // Blue player
  board: Board,
  fights: List[Fight] = Nil) {
}

object Match {
  case class Id(value: String) extends AnyVal

  case class RedScore(value: Int) extends AnyVal

  case class BlueScore(value: Int) extends AnyVal

  case class Score(red: RedScore, blue: BlueScore)

  /**
    * Adds a new fight to the match.
    *
    * @param theMatch the match that receives the new fight
    * @param fight the new fight
    *
    * @return match with the fight added
    */
  def addFight(theMatch: Match, fight: Fight): Match = theMatch.copy(fights = theMatch.fights :+ fight)

  /**
    * Get the current score of the match.
    *
    * @param theMatch match from where the score is extracted
    *
    * @return the current score
    */
  def score(theMatch: Match): Score = {
    val redScore = RedScore(cardsOf(theMatch.board, Red).length)
    val blueScore = BlueScore(cardsOf(theMatch.board, Blue).length)

    Score(redScore, blueScore)
  }
}
