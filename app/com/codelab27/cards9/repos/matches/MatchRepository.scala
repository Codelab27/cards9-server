package com.codelab27.cards9.repos.matches

import com.codelab27.cards9.models.matches.Match
import com.codelab27.cards9.models.matches.Match.MatchState
import com.codelab27.cards9.models.players.Player

trait MatchRepository[F[_]] {

  def findMatch(id: Match.Id): F[Option[Match]]

  def findMatches(state: MatchState): F[Seq[Match]]

  def findMatchesForPlayer(playerId: Player.Id): F[Seq[Match]]

  def storeMatch(theMatch: Match): F[Option[Match.Id]]

  def changeMatchState(id: Match.Id, state: MatchState): F[Option[MatchState]]

}
