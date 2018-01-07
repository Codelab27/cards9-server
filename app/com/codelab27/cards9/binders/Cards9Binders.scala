package com.codelab27.cards9.binders

import com.codelab27.cards9.models.common.Common.Color
import com.codelab27.cards9.models.matches.Match
import com.codelab27.cards9.models.matches.Match.MatchState
import com.codelab27.cards9.models.players.Player

import enumeratum._

import cats.syntax.either._

import play.api.mvc.PathBindable
import play.api.routing.sird.PathBindableExtractor

import scala.util.Try

object Cards9Binders {

  case class EnumPathBindable[T <: EnumEntry]()(implicit ev: Enum[T]) extends PathBindable[T] {
    override def bind(key: String, value: String) = {
      Try(ev.withNameLowercaseOnly(value))
        .toEither.leftMap(_ => s"Error in url converting $value to ${ev.getClass.getSimpleName.stripSuffix("$")}")
    }

    override def unbind(key: String, value: T) = value.entryName.toLowerCase
  }

  case class ValueClassPathBindable[O, I](
      unapply: O => Option[I],
      apply: I => O
  )(implicit primitive: PathBindable[I]) extends PathBindable[O] {

    override def bind(key: String, value: String) = primitive.bind(key, value).map(apply)

    override def unbind(key: String, value: O) = unapply(value).map(primitive.unbind(key, _)).getOrElse("")
  }

  val pbMatchState = EnumPathBindable[MatchState]

  val pbeMatchId = new PathBindableExtractor[Match.Id]()(ValueClassPathBindable(Match.Id.unapply, Match.Id.apply))

  val pbePlayerId = new PathBindableExtractor[Player.Id]()(ValueClassPathBindable(Player.Id.unapply, Player.Id.apply))

  val pbeColor = new PathBindableExtractor[Color]()(EnumPathBindable[Color])

}
