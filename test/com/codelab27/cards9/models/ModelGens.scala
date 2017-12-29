package com.codelab27.cards9.models

import java.net.URL

import com.codelab27.cards9.game.engines._
import com.codelab27.cards9.models.boards.{Board, BoardSettings}
import com.codelab27.cards9.models.cards.BattleClass._
import com.codelab27.cards9.models.cards._
import com.codelab27.cards9.models.players.Player
import com.codelab27.cards9.services.settings.GameSettings

import org.scalacheck.{Arbitrary, Gen}

object ModelGens {
  private val urlProtocol = "http://"

  private val CardClassGenerator: Gen[CardClass] = for {
    id    <- Gen.choose(0, Int.MaxValue)
    name  <- Gen.alphaStr
    img   <- Gen.alphaStr
  } yield {
    CardClass(CardClass.Name(name), new URL(urlProtocol + img), Some(CardClass.Id(id)))
  }

  private val BattleClassGenerator: Gen[BattleClass] = Gen.oneOf(Physical, Magical, Flexible, Assault)

  private val ArrowsGenerator: Gen[Seq[Arrow]] = Gen.someOf(Arrow.values)
  val InvalidArrowsGenerator: Gen[List[Arrow]] = Gen.choose(1, Arrow.MAX_ARROWS + 1) flatMap { size =>
    Gen.listOfN(size, Gen.oneOf(Arrow.values))
  }

  implicit val arrows: Arbitrary[List[Arrow]] = Arbitrary(ArrowsGenerator.map(_.toList))

  private def CardGenerator(implicit gameSettings: GameSettings): Gen[Card] = for {
    id          <- Gen.choose(0, Int.MaxValue)
    ownerId     <- Gen.choose(0, Int.MaxValue)
    cardClass   <- CardClassGenerator
    power       <- Gen.choose(0, gameSettings.CARD_MAX_LEVEL - 1)
    battleClass <- BattleClassGenerator
    pdef        <- Gen.choose(0, gameSettings.CARD_MAX_LEVEL - 1)
    mdef        <- Gen.choose(0, gameSettings.CARD_MAX_LEVEL - 1)
    arrows      <- ArrowsGenerator
  } yield {
    Card(Player.Id(ownerId), cardClass.id.get, power, battleClass, pdef, mdef, arrows.toList, Some(Card.Id(id)))
  }

  implicit def cards(implicit gameSettings: GameSettings): Arbitrary[Card] = Arbitrary(CardGenerator)

  private def HandGenerator(implicit gameSettings: GameSettings): Gen[Set[Card]] =
    Gen.containerOfN[Set, Card](gameSettings.MAX_HAND_CARDS, CardGenerator)

  private def BoardGenerator(implicit boardSettings: BoardSettings, gameSettings: GameSettings): Gen[Board] =
    for {
      redHand   <- HandGenerator
      blueHand  <- HandGenerator
    } yield {
      board.random(redHand, blueHand, boardSettings)
    }

  implicit def boards(implicit boardSettings: BoardSettings, gameSettings: GameSettings): Arbitrary[Board] = {
    Arbitrary(BoardGenerator)
  }
}
