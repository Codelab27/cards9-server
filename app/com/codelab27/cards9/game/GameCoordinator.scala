package com.codelab27.cards9.game

import akka.actor.Actor
import GameMessages._
import com.codelab27.cards9.models.players.{Match, Player}

class GameCoordinator extends Actor {
  import context._

  // Players matches
  var playerMatches: Map[Player.Id, Match.Id] = Map.empty

  def receive = ???
}
