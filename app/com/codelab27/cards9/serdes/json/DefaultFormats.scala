package com.codelab27.cards9.serdes.json

import com.codelab27.cards9.models.boards.Square.{Block, Free, Occupied}
import com.codelab27.cards9.models.boards._
import com.codelab27.cards9.models.cards._
import com.codelab27.cards9.models.common.Common.Color
import com.codelab27.cards9.models.matches.Match.{BluePlayer, MatchState, RedPlayer}
import com.codelab27.cards9.models.matches.{Match, MatchSnapshot}
import com.codelab27.cards9.models.players.Player

import enumeratum._
import org.joda.time.DateTime

import play.api.libs.json._

object DefaultFormats {

  // Helper formatter that goes from a O type value class to the I inner value serialization
  // O(value: I) extends AnyVal
  case class ValueClassJsonFormat[O, I](
      unapply: O => Option[I],
      apply: I => O
  )(implicit format: Format[I]) extends Format[O] {

    override def writes(o: O) = unapply(o).map(format.writes).getOrElse(JsNull)

    override def reads(json: JsValue) = format.reads(json).map(apply)

  }

  private def enumFormat[E <: EnumEntry](
      reader: String => Option[E], stringModifier: String => String
  )(implicit ev: Enum[E]): Format[E] = Format[E](
    Reads { json =>
      json.validate[JsString].flatMap { jstring =>
        reader(jstring.value) match {
          case Some(instance) => JsSuccess(instance)
          case None           => JsError(s"Error converting ${jstring.value} to a valid ${ev.getClass.getSimpleName.stripSuffix("$")}")
        }
      }
    },
    Writes(instance => JsString(stringModifier(instance.toString)))
  )

  private val pattern = "yyyy-MM-dd'T'HH:mm:ss.SSSZ"

  implicit val jodaDateFormat = Format[DateTime](JodaReads.jodaDateReads(pattern), JodaWrites.jodaDateWrites(pattern))

  implicit val playerIdFormat = ValueClassJsonFormat(Player.Id.unapply, Player.Id.apply)

  implicit val playerReadyFormat = ValueClassJsonFormat(Match.IsReady.unapply, Match.IsReady.apply)

  implicit val redPlayerFormat = Json.format[RedPlayer]

  implicit val bluePlayerFormat = Json.format[BluePlayer]

  implicit val cardIdFormat = ValueClassJsonFormat(Card.Id.unapply, Card.Id.apply)

  implicit val matchIdFormat = ValueClassJsonFormat(Match.Id.unapply, Match.Id.apply)

  implicit val cardClassIdFormat = ValueClassJsonFormat(CardClass.Id.unapply, CardClass.Id.apply)

  implicit val boardSizeFormat = ValueClassJsonFormat(BoardSize.unapply, BoardSize.apply)

  implicit val boardMaxBlocsFormat = ValueClassJsonFormat(BoardMaxBlocks.unapply, BoardMaxBlocks.apply)

  implicit val boardSettingsFormat = Json.format[BoardSettings]

  implicit val matchStateFormat = enumFormat(MatchState.withNameLowercaseOnlyOption, _.toLowerCase)

  implicit val arrowFormat = enumFormat(Arrow.withNameUppercaseOnlyOption, _.toUpperCase)

  implicit val colorFormat = enumFormat(Color.withNameUppercaseOnlyOption, _.toUpperCase)

  implicit val battleClassFormat = Format[BattleClass](
    Reads { json =>
      json.validate[JsString].filter(JsError(s"Battleclass is not a char or is empty"))(_.value.size == 1)
        .map(_.value.charAt(0))
        .flatMap {
          case BattleClass.Physical.uiChar => JsSuccess(BattleClass.Physical)
          case BattleClass.Magical.uiChar  => JsSuccess(BattleClass.Magical)
          case BattleClass.Flexible.uiChar => JsSuccess(BattleClass.Flexible)
          case BattleClass.Assault.uiChar  => JsSuccess(BattleClass.Assault)
          case other                       => JsError(s"Battleclass with char representation $other does not exist")
        }
    },
    Writes(bclass => JsString(bclass.uiChar.toString))
  )

  implicit val cardFormat = Json.format[Card]

  implicit val occupiedFormat = Json.format[Occupied]

  implicit val squareReads = Format[Square](
    Reads { json =>

      val blockFreeReads = Reads { json =>
        json.validate[JsString].map(_.value).flatMap[Square] {
          case "B"    => JsSuccess(Block)
          case "F"    => JsSuccess(Free)
          case other  => JsError(s"Error converting ${other} to a valid square")
          }
        }

      occupiedFormat.reads(json).orElse[Square](blockFreeReads.reads(json))
    },
    Writes {
      case oc: Occupied => occupiedFormat.writes(oc)
      case Block        => JsString(Block.toString)
      case Free         => JsString(Free.toString)
    }
  )

  implicit val boardFormat = Json.format[Board]

  implicit val fightFormat = Json.format[Fight]

  implicit val matchSnapshotIdFormat = ValueClassJsonFormat(MatchSnapshot.Id.unapply, MatchSnapshot.Id.apply)

  implicit val matchSnapshotFormat = Json.format[MatchSnapshot]

  implicit val matchFormat = Json.format[Match]

}
