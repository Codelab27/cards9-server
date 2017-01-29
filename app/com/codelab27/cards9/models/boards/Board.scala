package com.codelab27.cards9.models.boards

import com.codelab27.cards9.models.cards.{ Arrow, Card }
import com.codelab27.cards9.services.settings.GameSettings
import Array._
import scala.util.Random
import Board.Hand

/**
 * Possible colors of a card.
 */
sealed trait Color { def flip: Color }
case object Red extends Color { def flip: Color = Blue }
case object Blue extends Color { def flip: Color = Red }

/**
 * Possible states of a square.
 */
sealed trait Square
case class Occupied(card: Card, color: Color) extends Square { override def toString = card.id.toString + "," + color }
case object Block extends Square { override def toString = "B" }
case object Free extends Square { override def toString = "F" }

case class Board(
  grid: Array[Array[Square]],
  redPlayer: Hand,
  bluePlayer: Hand,
  size: Int) {
  override def toString = grid.map(row => row.mkString(" ")).mkString("\n")
}

case class Coordinate(i: Int, j:Int)

object Board {
  type Hand = Set[Card]



  /**
    * Adds a new occupied square to the board.
    *
    * @param board Board
    * @param newCoords new Coordinate
    * @param occupied card and color
    * @return a new board with the occupied square
    */
  def add(board: Board, newCoords: Coordinate, occupied: Occupied, player: Color): Board = {
    // Target square position must be free
    require(areValidCoords(board, newCoords))
    require(board.grid(newCoords.i)(newCoords.j) == Free)

    // Occupied card must be part of the hand of the selected player
    require(
      (board.redPlayer.contains(occupied.card) && player == Red) ||
        (board.bluePlayer.contains(occupied.card) && player == Blue)
    )

    board.grid(newCoords.i)(newCoords.j) = occupied

    val (newRed, newBlue) = player match {
      case Red  => (board.redPlayer - occupied.card, board.bluePlayer)
      case Blue => (board.redPlayer, board.bluePlayer - occupied.card)
    }

    board.copy(grid = board.grid.clone, redPlayer = newRed, bluePlayer = newBlue )
  }

  /**
    * Flips the card on the specified position.
    *
    * @param board Board
    * @param coords Coordinate
    * @return a new board with the card flipped
    */
  def flip(board: Board, coords: Coordinate): Board = {
    require(areValidCoords(board, coords))
    require(board.grid(coords.i)(coords.j).isInstanceOf[Occupied])

    board.grid(coords.i)(coords.j) match {
      case Occupied(card, color) => board.grid(coords.i)(coords.j) = Occupied(card, color.flip)
      case Block | Free          => // ERROR
    }

    board.copy(grid = board.grid.clone)
  }

  /**
    * Flips all the cards on the specified positions.
    *
    * @param board Board
    * @param coords Coordinate
    * @return a new board with the card flipped
    */
  def flipAll(board: Board, coords: List[Coordinate]): Board = {
    coords
      .filter(coords => areValidCoords(board, coords))
      .map(coords => flip(board, coords))
    board.copy(grid = board.grid.clone)
  }

  /**
    * Get the opponents for a card on the given coords.
    *
    * @param board Board
    * @param coords Coordinate
    * @return a list of possible opponents and the direction of the attack
    */
  def opponents(board: Board, coords: Coordinate): List[(Card, Arrow)] = {
    require(areValidCoords(board, coords))
    require(board.grid(coords.i)(coords.j).isInstanceOf[Occupied])

    board.grid(coords.i)(coords.j) match {
      case Occupied(card, color) =>

        card.arrows
          .map(arrow => (arrow, Arrow.arrowCoords(coords, arrow)))
          .filter { case (arrow: Arrow, coords: Coordinate) => areValidCoords(board, coords) }
          .collect {
            case (arrow, arrowCoord) =>
              board.grid(arrowCoord.i)(arrowCoord.j) match {
                case Occupied(enemyCard, enemyColor) if (color != enemyColor) =>
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
        case Occupied(card, sqColor) if (sqColor == color) => card
      }
    }).toList

  }

  // Check against
  private def areValidCoords(board: Board, coords: Coordinate): Boolean = {
    (coords.i >= 0 && coords.i < board.size && coords.j >= 0 && coords.j < board.size)
  }

  /**
   * Creates a fresh free board with some random blocks on it and the red and
   * blue player hands of cards.
   */
  def random(redPlayer: Hand, bluePlayer: Hand)(implicit gameSettings: GameSettings): Board = {
    val randomBlocks: Int = Random.nextInt(gameSettings.BOARD_MAX_BLOCKS + 1)

    // All possible coords of the grid
    val coords: Array[(Int, Int)] = Array((for {
      i <- (0 until gameSettings.BOARD_SIZE)
      j <- (0 until gameSettings.BOARD_SIZE)
    } yield (i, j)): _*)

    // Fisher-Yates
    for {
      i <- (coords.length - 1 to 1 by -1)
      j = Random.nextInt(i + 1)
    } {
      val aux = coords(i)
      coords(i) = coords(j)
      coords(j) = aux
    }

    // Create a new grid of Free squares and then throw in the random blocks
    val grid: Array[Array[Square]] = Array.fill(gameSettings.BOARD_SIZE, gameSettings.BOARD_SIZE)(Free)

    coords.take(randomBlocks).map {
      case (i, j) =>
        grid(i)(j) = Block
    }

    Board(grid, redPlayer, bluePlayer, gameSettings.BOARD_SIZE)
  }
}
