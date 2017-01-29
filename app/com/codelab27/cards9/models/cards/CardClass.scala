package com.codelab27.cards9.models.cards

import java.net.URL
import com.codelab27.cards9.game.GameTypes._

/**
 * Card class.
 *
 * @param id unique identifier of this card class
 * @param name card name
 * @param img url of the image for this card
 */
case class CardClass(
  id: CardClassId,
  name: String,
  img: URL)
