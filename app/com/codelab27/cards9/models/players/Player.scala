package com.codelab27.cards9.models.players

import com.codelab27.cards9.game.GameTypes._

case class Player(
  id: PlayerId,
  cards: List[CardId])
