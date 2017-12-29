package com.codelab27.cards9.game.engines

import com.codelab27.cards9.models.boards.Board.{Hand, _}
import com.codelab27.cards9.models.boards.Square.{Block, Free, Occupied}
import com.codelab27.cards9.models.boards.{Board, BoardSettings, Coordinates, Square}
import com.codelab27.cards9.models.cards.{Arrow, Card}
import com.codelab27.cards9.models.common.Common.Color
import com.codelab27.cards9.models.common.Common.Color.{Blue, Red}

import scala.util.Random

trait BoardEngine {

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
      (board.redHand.contains(occupied.card) && player == Red) ||
        (board.blueHand.contains(occupied.card) && player == Blue)
    )

    board.grid.update(newCoords.x)(newCoords.y)(occupied)

    val (newRed, newBlue) = player match {
      case Red  => (board.redHand - occupied.card, board.blueHand)
      case Blue => (board.redHand, board.blueHand - occupied.card)
    }

    board.copy(grid = board.grid.clone, redHand = newRed, blueHand = newBlue)
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
