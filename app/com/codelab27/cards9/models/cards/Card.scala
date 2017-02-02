package com.codelab27.cards9.models.cards

import scala.util.Random
import scala.math.{ max, min }
import enumeratum._
import com.codelab27.cards9.game.GameTypes._
import com.codelab27.cards9.services.settings.GameSettings

/**
 * Battle class of the card.
 *
 * - Physical attacks physical def stat
 * - Magical attacks magical def stat
 * - Flexible attacks lowest def stat
 * - Assault attacks the lowest stat
 *
 * Reference: [[http://finalfantasy.wikia.com/wiki/Tetra_Master_(Minigame)#Battle_class_stat Final Fantasy Wiki]]
 */
sealed trait BattleClass extends EnumEntry { def uiChar: Char }

object BattleClass extends Enum[BattleClass] {
  val values = findValues

  case object Physical extends BattleClass { val uiChar: Char = 'P' }
  case object Magical extends BattleClass { val uiChar: Char = 'M' }
  case object Flexible extends BattleClass { val uiChar: Char = 'X' }
  case object Assault extends BattleClass { val uiChar: Char = 'A' }
}

/**
 * Unique card instance.
 *
 * @param id unique identifier
 * @param ownerId player identifier
 * @param cardType type of card
 * @param power offensive stat
 * @param bclass battle class
 * @param pdef physical defense stat
 * @param mdef magical defense stat
 * @param arrows list of atk/def arrows
 */
case class Card(
  id: CardId,
  ownerId: PlayerId,
  cardType: CardClassId,
  power: Int,
  bclass: BattleClass,
  pdef: Int,
  mdef: Int,
  arrows: List[Arrow])(implicit gameSettings: GameSettings) {

  import BattleClass._

  require(power < gameSettings.CARD_MAX_LEVEL)
  require(pdef < gameSettings.CARD_MAX_LEVEL)
  require(mdef < gameSettings.CARD_MAX_LEVEL)
  require(arrows.distinct.size == arrows.size && arrows.size <= Arrow.MAX_ARROWS)

  /**
   * Challenge another card.
   *
   * @param other enemy card
   * @param side location of the enemy card
   *
   * @return a fight result
   */
  def fight(other: Card, side: Arrow): Fight = {
    // We need an arrow pointing to the other card
    require(arrows.contains(side))

    // Fight!!
    if (other.arrows.contains(side.opposite)) {
      val (atkStat, defStat) = bclass match {
        case Physical => (power, other.pdef)
        case Magical  => (power, other.mdef)
        case Flexible => (power, min(other.pdef, other.mdef))
        case Assault => (max(max(power, pdef), mdef),
          min(min(other.power, other.pdef), other.mdef))
      }

      lazy val (atkScore, defScore) = statVs(atkStat, defStat)

      def hitPoints(stat: Int): Int = stat * gameSettings.CARD_MAX_LEVEL

      // Battle maths
      def statVs(atkStat: Int, defStat: Int): (Int, Int) = {
        val p1atk = hitPoints(atkStat) + Random.nextInt(gameSettings.CARD_MAX_LEVEL)
        val p2def = hitPoints(defStat) + Random.nextInt(gameSettings.CARD_MAX_LEVEL)
        (p1atk - Random.nextInt(p1atk + 1), p2def - Random.nextInt(p2def + 1))
      }

      Fight(this.id, other.id, atkScore, defScore, atkScore > defScore)
    } else {
      // Instant win 
      Fight(this.id, other.id, 0, 0, true)
    }
  }
}
