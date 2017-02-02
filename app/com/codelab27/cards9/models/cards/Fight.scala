package com.codelab27.cards9.models.cards

import org.joda.time.DateTime
import com.codelab27.cards9.game.GameTypes._

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
  attacker: CardId,
  defender: CardId,
  atkPoints: Int,
  defPoints: Int,
  atkWinner: Boolean,
  dateTime: DateTime = DateTime.now)
