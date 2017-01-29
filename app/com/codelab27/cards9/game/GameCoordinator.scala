package com.codelab27.cards9.game

import akka.actor.Actor
import GameMessages._
import GameTypes._

class GameCoordinator extends Actor {
  import context._

  // Players matches
  var playerMatches: Map[PlayerId, MatchId] = Map.empty

  def receive = ???
}
