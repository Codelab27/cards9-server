package com.codelab27.cards9.models.boards

import com.codelab27.cards9.models.boards.Board.{Grid, Hand}
import com.codelab27.cards9.models.cards.Card

import scala.Array._

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
case class Occupied(card: Card, color: Color) extends Square { override def toString = s"${card.id},${color}" }
case object Block extends Square { override def toString = "B" }
case object Free extends Square { override def toString = "F" }

case class BoardSize(value: Int) extends AnyVal
case class BoardMaxBlocks(value: Int) extends AnyVal

case class BoardSettings(
  size: BoardSize,
  maxBlocks: BoardMaxBlocks)

final case class Board(
    grid: Grid,
    redPlayer: Hand,
    bluePlayer: Hand,
    settings: BoardSettings
) {
  override def toString = grid.map(row => row.mkString(" ")).mkString("\n")
}

case class XAxis(value: Int) extends AnyVal
case class YAxis(value: Int) extends AnyVal

case class Coordinates(x: XAxis, y: YAxis)

object Board {
  type Hand = Set[Card]
  type Grid = Array[Array[Square]]

  implicit class SafeGridAccess(grid: Grid) {

    def coords(xAxis: XAxis)(yAxis: YAxis): Square = grid(xAxis.value)(yAxis.value)

    def update(xAxis: XAxis)(yAxis: YAxis)(value: Square): Unit = grid(xAxis.value)(yAxis.value) = value

  }
}
