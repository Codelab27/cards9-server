package com.codelab27.cards9.game

import com.codelab27.cards9.models.boards.Coordinates
import com.codelab27.cards9.models.cards.Card
import com.codelab27.cards9.models.players.{Match, Player}

object GameMessages {
  sealed trait GameMessage
  case class AvailableMatches(playerId: Player.Id) extends GameMessage // Matches suspended
  case class AvailableCards(playerId: Player.Id) extends GameMessage
  case class SelectedHand(hand: Seq[Card.Id]) extends GameMessage // Player selected hand
  case class JoinMatch(matchId: Match.Id) extends GameMessage // Player joins match
  case class DeleteMatch(matchId: Match.Id) extends GameMessage
  case class NewMatch(playerId: Player.Id) extends GameMessage // Player creates a new match
  case object StartMatch extends GameMessage // Player is ready to start/restart the match
  case object LeaveMatch extends GameMessage // Player leaves current match

  sealed trait MatchMessage
  case class PlayerJoined(playerId: Player.Id) extends MatchMessage
  case class PutCard(playerId: Player.Id, cardId: Card.Id, coords: Coordinates) extends MatchMessage
  case class ChooseOpponent(playerId: Player.Id, coords: Coordinates) extends MatchMessage
}
