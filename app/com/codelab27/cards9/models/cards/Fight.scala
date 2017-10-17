package com.codelab27.cards9.models.cards

import org.joda.time.DateTime

/**
 * Result of a card fight.
 *
 * @param attacker card attacking
 * @param defender card defending
 * @param atkPoints points of the attack
 * @param defPoints points of the defense
 * @param atkWinner true if attacker was the winner of the fight
 */
case class Fight(
  attacker: Card.Id,
  defender: Card.Id,
  atkPoints: Int,
  defPoints: Int,
  atkWinner: Boolean,
  dateTime: DateTime = DateTime.now)

object Fight {
  case class Id(value: String) extends AnyVal
}
