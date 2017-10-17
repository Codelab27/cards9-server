package com.codelab27.cards9.models.cards

import com.codelab27.cards9.models.boards.{Coordinates, XAxis, YAxis}
import enumeratum._

/**
 * Card arrows.
 *
 * Packed representation (bits):
 *
 *   N   NE  E   SE  S   SW  W   NW
 * ---------------------------------
 * | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 |
 * ---------------------------------
 *
 */
sealed trait Arrow extends EnumEntry {
  def hex: Byte
  def opposite: Arrow
}

object Arrow extends Enum[Arrow] {
  val values = findValues

  case object N extends Arrow { val hex: Byte = 0x80.toByte; val opposite: Arrow = S }
  case object NE extends Arrow { val hex: Byte = 0x40; val opposite: Arrow = SW }
  case object E extends Arrow { val hex: Byte = 0x20; val opposite: Arrow = W }
  case object SE extends Arrow { val hex: Byte = 0x10; val opposite: Arrow = NW }
  case object S extends Arrow { val hex: Byte = 0x08; val opposite: Arrow = N }
  case object SW extends Arrow { val hex: Byte = 0x04; val opposite: Arrow = NE }
  case object W extends Arrow { val hex: Byte = 0x02; val opposite: Arrow = E }
  case object NW extends Arrow { val hex: Byte = 0x01; val opposite: Arrow = SE }

  val MAX_ARROWS = values.size

  /**
   * Get the coordinates of the arrow, given the center.
   *
   * @param arrow get coords for this arrow
   * @param coords center of coordinates
   * @return the coordinates of the arrow
   */
  def arrowCoords(arrow: Arrow, coords: Coordinates): Coordinates = {
    val i = coords.x.value
    val j = coords.y.value

    val (newX, newY) = arrow match {
      case N  => (i - 1, j)
      case NE => (i - 1, j + 1)
      case E  => (i, j + 1)
      case SE => (i - 1, j + 1)
      case S  => (i - 1, j)
      case SW => (i - 1, j - 1)
      case W  => (i, j - 1)
      case NW => (i - 1, j - 1)
    }

    Coordinates(XAxis(newX), YAxis(newY))
  }

  /**
   * Extract a list of arrows from a packed byte.
   *
   * @param packed a byte with packed arrows
   * @return a list with the arrows contained into the packed byte
   */
  def extract(packed: Byte): List[Arrow] = values.toList.filterNot(arrow => (arrow.hex & packed) == 0)

  /**
   * Compresses a list of arrows into a packed byte.
   *
   * @param arrows list of arrows
   * <b>Precondition:</b>
   * arrows must be a list of distinct arrows, with a max size of [[MAX_ARROWS]]
   * @return some byte with the arrows compressed when preconditions are true
   */
  def compress(arrows: List[Arrow]): Option[Byte] = {
    // Do not repeat arrows and do not exceed max arrows of card
    val allArrowsAreDistinct = arrows.distinct.size == arrows.size
    val correctNumberOfArrows = arrows.size < MAX_ARROWS + 1

    allArrowsAreDistinct && correctNumberOfArrows match {
      case false  => None
      case true   => {
        val compressed: Byte = {
          if (arrows.isEmpty) 0x00 // Card with no arrows...
          else arrows.foldLeft[Byte](0x00)((total, next) => (total | next.hex).toByte)
        }

        Some(compressed)
      }
    }
  }
}
