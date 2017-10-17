package com.codelab27.cards9.models.cards

import com.codelab27.cards9.models.players.Player

import scala.util.Random
import scala.math.{max, min}
import enumeratum._
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
  id: Card.Id,
  ownerId: Player.Id,
  cardType: CardClass.Id,
  power: Int,
  bclass: BattleClass,
  pdef: Int,
  mdef: Int,
  arrows: List[Arrow])(implicit gameSettings: GameSettings) {

  require(power < gameSettings.CARD_MAX_LEVEL)
  require(pdef < gameSettings.CARD_MAX_LEVEL)
  require(mdef < gameSettings.CARD_MAX_LEVEL)
  require(arrows.distinct.size == arrows.size && arrows.size <= Arrow.MAX_ARROWS)

}

object Card {

  case class Id(value: Int) extends AnyVal

  /**
    * Challenge another card.
    *
    * @param attacker attacking card
    * @param defender enemy card
    * @param side location of the enemy card
    *
    * @return a fight result
    */
  def fight(attacker: Card, defender: Card, side: Arrow)(implicit gameSettings: GameSettings): Fight = {
    import BattleClass._

    // We need an arrow pointing to the other card
    require(attacker.arrows.contains(side))

    // Fight!!
    if (defender.arrows.contains(side.opposite)) {
      val (atkStat, defStat) = attacker.bclass match {
        case Physical => (attacker.power, defender.pdef)
        case Magical  => (attacker.power, defender.mdef)
        case Flexible => (attacker.power, min(defender.pdef, defender.mdef))
        case Assault => (max(max(attacker.power, attacker.pdef), attacker.mdef),
          min(min(defender.power, defender.pdef), defender.mdef))
      }

      lazy val (atkScore, defScore) = statVs(atkStat, defStat)

      def hitPoints(stat: Int): Int = stat * gameSettings.CARD_MAX_LEVEL

      // Battle maths
      def statVs(atkStat: Int, defStat: Int): (Int, Int) = {
        val p1atk = hitPoints(atkStat) + Random.nextInt(gameSettings.CARD_MAX_LEVEL)
        val p2def = hitPoints(defStat) + Random.nextInt(gameSettings.CARD_MAX_LEVEL)
        (p1atk - Random.nextInt(p1atk + 1), p2def - Random.nextInt(p2def + 1))
      }

      Fight(attacker.id, defender.id, atkScore, defScore, atkScore > defScore)
    } else {
      // Instant win
      Fight(attacker.id, defender.id, 0, 0, true)
    }
  }

}
