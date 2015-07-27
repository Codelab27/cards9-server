package models

import models.boards.Board
import models.cards._
import models.cards.BattleClass._
import services.settings.GameSettings
import org.scalacheck.Gen
import java.net.URL
import scala.util.Random
import org.scalacheck.Arbitrary

object ModelGens {
  private val urlProtocol = "http://"

  private val CardClassGenerator: Gen[CardClass] = for {
    id <- Gen.choose(0, Int.MaxValue)
    name <- Gen.alphaStr
    img <- Gen.alphaStr
  } yield {
    CardClass(id, name, new URL(urlProtocol + img))
  }

  private val BattleClassGenerator: Gen[BattleClass] = Gen.oneOf(Physical, Magical, Flexible, Assault)

  private val ArrowsGenerator: Gen[Seq[Arrow]] = Gen.someOf(Arrow.values)
  val InvalidArrowsGenerator: Gen[List[Arrow]] = Gen.choose(1, Arrow.MAX_ARROWS + 1) flatMap { size =>
    Gen.listOfN(size, Gen.oneOf(Arrow.values))
  }

  implicit val arrows: Arbitrary[List[Arrow]] = Arbitrary(ArrowsGenerator.map(_.toList))

  private def CardGenerator(implicit gameSettings: GameSettings): Gen[Card] = for {
    id <- Gen.choose(0, Int.MaxValue)
    ownerId <- Gen.choose(0, Int.MaxValue)
    cardClassId <- Gen.choose(0, Int.MaxValue)
    power <- Gen.choose(0, gameSettings.CARD_MAX_LEVEL - 1)
    battleClass <- BattleClassGenerator
    pdef <- Gen.choose(0, gameSettings.CARD_MAX_LEVEL - 1)
    mdef <- Gen.choose(0, gameSettings.CARD_MAX_LEVEL - 1)
    arrows <- ArrowsGenerator
  } yield {
    Card(id, ownerId, cardClassId, power, battleClass, pdef, mdef, arrows.toList)
  }

  implicit def cards(implicit gameSettings: GameSettings): Arbitrary[Card] = Arbitrary(CardGenerator)

  private def HandGenerator(implicit gameSettings: GameSettings): Gen[Set[Card]] =
    Gen.containerOfN[Set, Card](gameSettings.MAX_HAND_CARDS, CardGenerator)

  private def BoardGenerator(implicit gameSettings: GameSettings): Gen[Board] =
    for {
      redHand <- HandGenerator
      blueHand <- HandGenerator
    } yield {
      Board.random(redHand, blueHand)
    }

  implicit def boards(implicit gameSettings: GameSettings): Arbitrary[Board] = Arbitrary(BoardGenerator)
}
