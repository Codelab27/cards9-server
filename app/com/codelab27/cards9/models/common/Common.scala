package com.codelab27.cards9.models.common

import enumeratum.{Enum, EnumEntry}

object Common {

  /**
    * Possible colors inside game.
    */
  sealed trait Color extends EnumEntry { def flip: Color }

  object Color extends Enum[Color] {

    val values = findValues

    case object Red extends Color { def flip: Color = Blue }

    case object Blue extends Color { def flip: Color = Red }

  }

}
