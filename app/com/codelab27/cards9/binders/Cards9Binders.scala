package com.codelab27.cards9.binders

import com.codelab27.cards9.models.matches.Match.MatchState

import play.api.mvc.PathBindable
import play.api.routing.sird.PathBindableExtractor

import scala.util.{Failure, Success, Try}

object Cards9Binders {

  private implicit def matchStateBinder: PathBindable[MatchState] = new PathBindable[MatchState] {
      override def bind(key: String, value: String): Either[String, MatchState] = {
        Try(MatchState.withNameLowercaseOnly(value)) match {
          case Success(state) => Right(state)
          case Failure(_)     => Left(s"Error in url converting $value to match state")
        }
      }

      override def unbind(key: String, state: MatchState): String = state.entryName.toLowerCase
  }

  val matchState = new PathBindableExtractor[MatchState]

}
