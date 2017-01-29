package com.codelab27.cards9.services.settings

trait GameSettings {
  /**
   * Size of the squared board side.
   */
  def BOARD_SIZE: Int

  /**
   * Maximum number of blocks on the board.
   */
  def BOARD_MAX_BLOCKS: Int

  /**
   * Maximum level for card stats.
   */
  def CARD_MAX_LEVEL: Int

  /**
   * Maximum number for cards in hand.
   */
  def MAX_HAND_CARDS: Int
}
