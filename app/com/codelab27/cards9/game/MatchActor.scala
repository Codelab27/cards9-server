package com.codelab27.cards9.game

import com.codelab27.cards9.models.players.Match
import com.codelab27.cards9.models.boards.Color
import GameTypes._
import akka.actor.Actor

class MatchActor(matchId: MatchId, creator: PlayerId) extends Actor {
  import context._

  var cards9Match: Option[Match] = None
  var currentPlayer: PlayerId = creator
  var colors: Map[PlayerId, Color] = Map.empty

  def receive: Receive = ???
}
