package com.codelab27.cards9.binders

import com.codelab27.cards9.models.matches.Match
import com.codelab27.cards9.models.matches.Match.{MatchState, PlayerAction}
import com.codelab27.cards9.models.players.Player

import enumeratum._

import cats.syntax.either._

import play.api.mvc.PathBindable
import play.api.routing.sird.PathBindableExtractor

import scala.reflect.ClassTag
import scala.util.Try

object Cards9Binders {

  private case class EnumBinder[T <: EnumEntry]()(implicit ev: Enum[T], ct: ClassTag[T]) extends PathBindable[T] {
    override def bind(key: String, value: String) = {
      Try(ev.withNameLowercaseOnly(value))
        .toEither.leftMap(_ => s"Error in url converting $value to ${ct.getClass.getSimpleName}")
    }

    override def unbind(key: String, value: T) = value.entryName.toLowerCase
  }

  private case class ValueClassBinder[O, I](
      unapply: O => Option[I],
      apply: I => O
  )(implicit primitive: PathBindable[I]) extends PathBindable[O] {

    override def bind(key: String, value: String) = primitive.bind(key, value).map(apply)

    override def unbind(key: String, value: O) = unapply(value).map(primitive.unbind(key, _)).getOrElse("")
  }

  val matchState = new PathBindableExtractor[MatchState]()(EnumBinder[MatchState])

  val playerAction = new PathBindableExtractor[PlayerAction]()(EnumBinder[PlayerAction])

  val matchId = new PathBindableExtractor[Match.Id]()(ValueClassBinder(Match.Id.unapply, Match.Id.apply))

  val playerId = new PathBindableExtractor[Player.Id]()(ValueClassBinder(Player.Id.unapply, Player.Id.apply))

}
