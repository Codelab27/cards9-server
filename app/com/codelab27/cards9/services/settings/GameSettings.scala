package com.codelab27.cards9.services.settings

trait GameSettings {


  /**
   * Maximum level for card stats.
   */
  def CARD_MAX_LEVEL: Int

  /**
   * Maximum number for cards in hand.
   */
  def MAX_HAND_CARDS: Int
}
