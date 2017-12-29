package com.codelab27.cards9.game.engines

import com.codelab27.cards9.models.cards.{Arrow, BattleClass, Card, Fight}
import com.codelab27.cards9.services.settings.GameSettings
import com.codelab27.cards9.utils.FightError

import cats.syntax.either._

import scala.math.{max, min}
import scala.util.Random

trait FightEngine {

  /**
    * Challenge another card.
    *
    * @param attacker attacking card
    * @param defender enemy card
    * @param side     location of the enemy card
    * @return a fight result
    */
  def fight(
      attacker: Card,
      defender: Card,
      side: Arrow
  )(implicit gameSettings: GameSettings): Either[FightError, Fight] = {
    import BattleClass._

    // Fight!!
    lazy val possibleFight = for {
      attackerId <- attacker.id
      defenderId <- defender.id
    } yield {

      if (defender.arrows.contains(side.opposite)) {
        val (atkStat, defStat) = attacker.bclass match {
          case Physical => (attacker.power, defender.pdef)
          case Magical  => (attacker.power, defender.mdef)
          case Flexible => (attacker.power, min(defender.pdef, defender.mdef))
          case Assault  => (max(max(attacker.power, attacker.pdef), attacker.mdef),
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

        Fight(attackerId, defenderId, atkScore, defScore, atkScore > defScore)
      } else {
        // Instant win
        Fight(attackerId, defenderId, 0, 0, atkWinner = true)
      }
    }

    for {
      // We need an arrow pointing to the other card
      _ <- Either.cond(attacker.arrows.contains(side), {}, FightError(s"Attacker does not contain $side arrow "))
      fight <- Either.fromOption(possibleFight, FightError(s"AttackerId=${attacker.id}, DefenderId=${defender.id}"))
    } yield {
      fight
    }
  }

}
