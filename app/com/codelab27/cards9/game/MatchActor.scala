package com.codelab27.cards9.game

import com.codelab27.cards9.models.players.{Match, Player}
import com.codelab27.cards9.models.boards.Color
import akka.actor.Actor

class MatchActor(matchId: Match.Id, creator: Player.Id) extends Actor {
  import context._

  var cards9Match: Option[Match] = None
  var currentPlayer: Player.Id = creator
  var colors: Map[Player.Id, Color] = Map.empty

  def receive: Receive = ???
}
