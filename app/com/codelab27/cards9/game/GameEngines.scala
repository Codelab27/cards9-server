package com.codelab27.cards9.game

import com.codelab27.cards9.models.boards.Board._
import com.codelab27.cards9.models.boards.Color.{Blue, Red}
import com.codelab27.cards9.models.boards.Square.{Block, Free, Occupied}
import com.codelab27.cards9.models.boards._
import com.codelab27.cards9.models.cards.{Arrow, BattleClass, Card, Fight}
import com.codelab27.cards9.models.matches.Match
import com.codelab27.cards9.models.matches.Match.{BlueScore, RedScore, Score}
import com.codelab27.cards9.services.settings.GameSettings
import com.codelab27.cards9.utils.FightError

import cats.syntax.either._

import scala.math.{max, min}
import scala.util.Random

object GameEngines {

  object FightEngine {

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

  object BoardEngine {

    /**
      * Adds a new occupied square to the board.
      *
      * @param board     Board
      * @param newCoords new Coordinate
      * @param occupied  card and color
      * @return a new board with the occupied square
      */
    def add(board: Board, newCoords: Coordinates, occupied: Occupied, player: Color): Board = {
      // Target square position must be free
      require(areValidCoords(board, newCoords))
      require(board.grid.coords(newCoords.x)(newCoords.y) == Free)

      // Occupied card must be part of the hand of the selected player
      require(
        (board.redPlayer.contains(occupied.card) && player == Red) ||
          (board.bluePlayer.contains(occupied.card) && player == Blue)
      )

      board.grid.update(newCoords.x)(newCoords.y)(occupied)

      val (newRed, newBlue) = player match {
        case Red  => (board.redPlayer - occupied.card, board.bluePlayer)
        case Blue => (board.redPlayer, board.bluePlayer - occupied.card)
      }

      board.copy(grid = board.grid.clone, redPlayer = newRed, bluePlayer = newBlue)
    }

    /**
      * Flips the card on the specified position.
      *
      * @param board  Board
      * @param coords Coordinate
      * @return a new board with the card flipped
      */
    def flip(board: Board, coords: Coordinates): Board = {
      require(areValidCoords(board, coords))
      require(board.grid.coords(coords.x)(coords.y).isInstanceOf[Occupied])

      board.grid.coords(coords.x)(coords.y) match {
        case Occupied(card, color) => board.grid.update(coords.x)(coords.y)(Occupied(card, color.flip))
        case Block | Free          => // ERROR
      }

      board.copy(grid = board.grid.clone)
    }

    /**
      * Flips all the cards on the specified positions.
      *
      * @param board  Board
      * @param coords Coordinate
      * @return a new board with the card flipped
      */
    def flipAll(board: Board, coords: List[Coordinates]): Board = {
      coords
        .filter(coords => areValidCoords(board, coords))
        .map(coords => flip(board, coords))
      board.copy(grid = board.grid.clone)
    }

    /**
      * Get the opponents for a card on the given coords.
      *
      * @param board  Board
      * @param coords Coordinate
      * @return a list of possible opponents and the direction of the attack
      */
    def opponents(board: Board, coords: Coordinates): List[(Card, Arrow)] = {
      require(areValidCoords(board, coords))
      require(board.grid.coords(coords.x)(coords.y).isInstanceOf[Occupied])

      board.grid.coords(coords.x)(coords.y) match {
        case Occupied(card, color) =>

          card.arrows
            .map(arrow => (arrow, Arrow.arrowCoords(arrow, coords)))
            .filter { case (_, coords: Coordinates) => areValidCoords(board, coords) }
            .collect {
              case (arrow, arrowCoord) =>
                board.grid.coords(arrowCoord.x)(arrowCoord.y) match {
                  case Occupied(enemyCard, enemyColor) if color != enemyColor =>
                    (enemyCard, arrow)
                }
            }

        case Block | Free => List.empty // Maybe error
      }
    }

    /**
      * Retrieve all the cards from the board of the given color.
      *
      * @param board Board
      * @param color color of the cards to be retrieved
      * @return a list with all the cards on the board with that color
      */
    def cardsOf(board: Board, color: Color): List[Card] = {
      (board.grid flatMap { row =>
        row.collect {
          case Occupied(card, sqColor) if sqColor == color => card
        }
      }).toList

    }

    // Check against
    private def areValidCoords(board: Board, coords: Coordinates): Boolean = {
      coords.x.value >= 0 && coords.x.value < board.settings.size.value &&
        coords.y.value >= 0 && coords.y.value < board.settings.size.value
    }

    /**
      * Creates a fresh free board with some random blocks on it and the red and
      * blue player hands of cards.
      */
    def random(redPlayer: Hand, bluePlayer: Hand, boardSettings: BoardSettings): Board = {
      val randomBlocks: Int = Random.nextInt(boardSettings.maxBlocks.value + 1)

      // All possible coords of the grid
      val coords: Array[(Int, Int)] = Array((for {
        i <- 0 until boardSettings.size.value
        j <- 0 until boardSettings.size.value
      } yield (i, j)): _*)

      // Fisher-Yates
      for {
        i <- coords.length - 1 to 1 by -1
        j = Random.nextInt(i + 1)
      } {
        val aux = coords(i)
        coords(i) = coords(j)
        coords(j) = aux
      }

      // Create a new grid of Free squares and then throw in the random blocks
      val grid: Array[Array[Square]] = Array.fill(boardSettings.size.value, boardSettings.size.value)(Free)

      coords.take(randomBlocks).foreach {
        case (i, j) => grid(i)(j) = Block
      }

      Board(grid, redPlayer, bluePlayer, boardSettings)
    }
  }

  object MatchEngine {
    import BoardEngine._

    /**
      * Adds a new fight to the match.
      *
      * @param theMatch the match that receives the new fight
      * @param fight    the new fight
      * @return match with the fight added
      */
    def addFight(theMatch: Match, fight: Fight): Match = {

      val attemptAddingFight = for {
        snapshot <- theMatch.snapshot
      } yield {
        theMatch.copy(snapshot = Some(snapshot.copy(fights = snapshot.fights :+ fight)))
      }

      attemptAddingFight.getOrElse(theMatch)

    }

    /**
      * Get the current score of the match.
      *
      * @param theMatch match from where the score is extracted
      * @return the current score
      */
    def score(theMatch: Match): Score = {

      val attemptScoreRetrieval = for {
        snapshot <- theMatch.snapshot
      } yield {
        val redScore = RedScore(cardsOf(snapshot.board, Red).length)
        val blueScore = BlueScore(cardsOf(snapshot.board, Blue).length)

        Score(redScore, blueScore)
      }

      attemptScoreRetrieval.getOrElse(Score(RedScore(0), BlueScore(0)))

    }
  }
}
