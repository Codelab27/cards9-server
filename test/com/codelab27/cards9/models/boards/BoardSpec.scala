package com.codelab27.cards9.models.boards

import com.codelab27.cards9.models.ModelGens._
import com.codelab27.cards9.specs.ModelSpec
import com.codelab27.cards9.services.settings.GameSettings

class BoardSpec extends ModelSpec {

  "A board" when {
    implicit val defaultGameSettings: GameSettings = new GameSettings {
      override val BOARD_SIZE: Int = 4
      override val BOARD_MAX_BLOCKS: Int = 6
      override val CARD_MAX_LEVEL: Int = 16
      override val MAX_HAND_CARDS: Int = 5
    }

    "created" should {
      "have the size specified" in {
        forAll { board: Board =>
          board.grid.length should be(defaultGameSettings.BOARD_SIZE)
        }
      }

      "have a number of blocks less or equal than max blocks" in {
        forAll { board: Board =>
          lazy val numBlocks: Int = board.grid.foldLeft(0)(sumTotalRows)

          def sumTotalRows(total: Int, row: Array[Square]): Int = {
            lazy val rowBlocks = row.foldLeft(0)(sumRow)
            total + rowBlocks
          }

          def sumRow(blocks: Int, cell: Square): Int = cell match {
            case Block => blocks + 1
            case _     => blocks
          }

          numBlocks should be <= defaultGameSettings.BOARD_MAX_BLOCKS
        }
      }
    }

  }

}
