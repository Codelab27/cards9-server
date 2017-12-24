package com.codelab27.cards9.services.matchmaking

import com.codelab27.cards9.models.matches.Match
import com.codelab27.cards9.models.matches.Match.MatchState

trait MatchMaker[F[_]] {

  def findMatch(id: Option[Match.Id]): F[Option[Match]]

  def findMatches(state: MatchState): F[Seq[Match]]

  def storeMatch(theMatch: Match): F[Option[Match.Id]]

  def changeMatchState(id: Option[Match.Id], state: MatchState): F[Option[MatchState]]

}
