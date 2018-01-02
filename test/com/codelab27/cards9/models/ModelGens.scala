package com.codelab27.cards9.models

import java.net.URL

import com.codelab27.cards9.game.engines._
import com.codelab27.cards9.models.boards.{Board, BoardSettings}
import com.codelab27.cards9.models.cards.BattleClass._
import com.codelab27.cards9.models.cards._
import com.codelab27.cards9.models.matches.Match
import com.codelab27.cards9.models.matches.Match._
import com.codelab27.cards9.models.players.Player
import com.codelab27.cards9.services.settings.GameSettings

import org.scalacheck.{Arbitrary, Gen}

import scala.reflect.runtime.universe.TypeTag

object ModelGens {

  private val urlProtocol = "http://"

  private val cardClassGenerator: Gen[CardClass] = for {
    id    <- Gen.choose(0, Int.MaxValue)
    name  <- Gen.alphaStr
    img   <- Gen.alphaStr
  } yield {
    CardClass(CardClass.Name(name), new URL(urlProtocol + img), Some(CardClass.Id(id)))
  }

  private val battleClassGenerator: Gen[BattleClass] = Gen.oneOf(Physical, Magical, Flexible, Assault)

  private val arrowsGenerator: Gen[Seq[Arrow]] = Gen.someOf(Arrow.values)

  val invalidArrowsGenerator: Gen[List[Arrow]] = Gen.choose(1, Arrow.MAX_ARROWS + 1) flatMap { size =>
    Gen.listOfN(size, Gen.oneOf(Arrow.values))
  }

  implicit val arrows: Arbitrary[List[Arrow]] = Arbitrary(arrowsGenerator.map(_.toList))

  private def cardGenerator(implicit gameSettings: GameSettings): Gen[Card] = for {
    id          <- Gen.choose(0, Int.MaxValue)
    ownerId     <- Gen.choose(0, Int.MaxValue)
    cardClass   <- cardClassGenerator
    power       <- Gen.choose(0, gameSettings.CARD_MAX_LEVEL - 1)
    battleClass <- battleClassGenerator
    pdef        <- Gen.choose(0, gameSettings.CARD_MAX_LEVEL - 1)
    mdef        <- Gen.choose(0, gameSettings.CARD_MAX_LEVEL - 1)
    arrows      <- arrowsGenerator
  } yield {
    Card(Player.Id(ownerId), cardClass.id.get, power, battleClass, pdef, mdef, arrows.toList, Some(Card.Id(id)))
  }

  implicit def cards(implicit gameSettings: GameSettings): Arbitrary[Card] = Arbitrary(cardGenerator)

  private def handGenerator(implicit gameSettings: GameSettings): Gen[Set[Card]] =
    Gen.containerOfN[Set, Card](gameSettings.MAX_HAND_CARDS, cardGenerator)

  private def boardGenerator(implicit boardSettings: BoardSettings, gameSettings: GameSettings): Gen[Board] =
    for {
      redHand   <- handGenerator
      blueHand  <- handGenerator
    } yield {
      board.random(redHand, blueHand, boardSettings)
    }

  implicit def boards(implicit boardSettings: BoardSettings, gameSettings: GameSettings): Arbitrary[Board] = {
    Arbitrary(boardGenerator)
  }

  private val playerIdGenerator: Gen[Player.Id] = Gen.choose(0, Int.MaxValue).map(Player.Id.apply)

  implicit val playerIds: Arbitrary[Player.Id] = Arbitrary(playerIdGenerator)

  private val matchIdGenerator: Gen[Match.Id] = Gen.identifier.map(Match.Id.apply)

  implicit val matchIds: Arbitrary[Match.Id] = Arbitrary(matchIdGenerator)

  private def coloredPlayerGenerator[T <: ColoredPlayer](implicit ev: TypeTag[T]): Gen[T] = ev.tpe match {
    case _: RedPlayer.type  => playerIdGenerator.map(id => RedPlayer(id, IsReady(false)).asInstanceOf[T])
    case _: BluePlayer.type => playerIdGenerator.map(id => BluePlayer(id, IsReady(false)).asInstanceOf[T])
  }

  private val matchStateGenerator: Gen[MatchState] = Gen.oneOf(MatchState.values)

  private def matchGenerator: Gen[Match] =
    for {
      redPlayer   <- Gen.option(coloredPlayerGenerator[RedPlayer])
      bluePlayer  <- Gen.option(coloredPlayerGenerator[BluePlayer])
      matchState  <- matchStateGenerator
      matchId     <- Gen.option(matchIdGenerator)
    } yield {
      Match(redPlayer, bluePlayer, matchState, None, matchId)
    }

  implicit val matches: Arbitrary[Match] = Arbitrary(matchGenerator)

}
