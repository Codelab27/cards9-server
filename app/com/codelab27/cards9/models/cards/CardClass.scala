package com.codelab27.cards9.models.cards

import java.net.URL

/**
 * Card class.
 *
 * @param id unique identifier of this card class
 * @param name card name
 * @param img url of the image for this card
 */
case class CardClass(
  id: CardClass.Id,
  name: CardClass.Name,
  img: URL)

object CardClass {
  case class Id(value: Int) extends AnyVal

  case class Name(value: String) extends AnyVal
}
