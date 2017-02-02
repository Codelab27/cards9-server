package com.codelab27.cards9.game

import GameTypes._

object GameMessages {
  sealed trait GameMessage
  case class AvailableMatches(playerId: PlayerId) extends GameMessage // Matches suspended
  case class AvailableCards(playerId: PlayerId) extends GameMessage
  case class SelectedHand(hand: Seq[CardId]) extends GameMessage // Player selected hand
  case class JoinMatch(matchId: MatchId) extends GameMessage // Player joins match
  case class DeleteMatch(matchId: MatchId) extends GameMessage
  case class NewMatch(playerId: PlayerId) extends GameMessage // Player creates a new match
  case object StartMatch extends GameMessage // Player is ready to start/restart the match
  case object LeaveMatch extends GameMessage // Player leaves current match

  sealed trait MatchMessage
  case class PlayerJoined(playerId: PlayerId) extends MatchMessage
  case class PutCard(playerId: PlayerId, cardId: CardId, i: Int, j: Int) extends MatchMessage
  case class ChooseOpponent(playerId: PlayerId, i: Int, j: Int) extends MatchMessage
}
