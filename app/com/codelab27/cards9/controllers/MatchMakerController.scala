package com.codelab27.cards9.controllers

import com.codelab27.cards9.models.matches.Match
import com.codelab27.cards9.models.matches.Match.MatchState
import com.codelab27.cards9.services.matchmaking.MatchMaker

import cats.{Comonad, Monad}

import play.api.mvc.{AbstractController, ControllerComponents}

class MatchMakerController[F[_] : Monad](
    cc: ControllerComponents,
    matchMaker: MatchMaker[F]
)(implicit evf: Comonad[F]) extends AbstractController(cc) {

  def getMatchesForState(state: MatchState) = Action {

    val attemptMatchesRetrieval: F[Seq[Match]] = matchMaker.findMatches(state)

    Ok(s"${evf.extract(attemptMatchesRetrieval).mkString(";")} for state: ${state}")

  }

}
