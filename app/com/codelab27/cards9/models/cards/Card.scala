package com.codelab27.cards9.models.cards

import com.codelab27.cards9.models.players.Player

import enumeratum._

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
 * @param ownerId player identifier
 * @param cardType type of card
 * @param power offensive stat
 * @param bclass battle class
 * @param pdef physical defense stat
 * @param mdef magical defense stat
 * @param arrows list of atk/def arrows
 * @param id unique identifier
 */
final case class Card(
  ownerId: Player.Id,
  cardType: CardClass.Id,
  power: Int,
  bclass: BattleClass,
  pdef: Int,
  mdef: Int,
  arrows: List[Arrow],
  id: Option[Card.Id] = None) {

//  require(power < gameSettings.CARD_MAX_LEVEL)
//  require(pdef < gameSettings.CARD_MAX_LEVEL)
//  require(mdef < gameSettings.CARD_MAX_LEVEL)
  require(arrows.distinct.size == arrows.size && arrows.size <= Arrow.MAX_ARROWS)

}

object Card {

  case class Id(value: Int) extends AnyVal

}
