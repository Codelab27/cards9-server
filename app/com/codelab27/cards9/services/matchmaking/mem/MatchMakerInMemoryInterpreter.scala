package com.codelab27.cards9.services.matchmaking.mem

import com.codelab27.cards9.models.matches.Match
import com.codelab27.cards9.services.matchmaking.MatchMaker

import cats.Id

import scala.collection.concurrent.TrieMap

object MatchMakerInMemoryInterpreter extends MatchMaker[Id] {

  private def generateId = if (matchRepo.isEmpty) {
    Match.Id(1.toString)
  } else {
    Match.Id((matchRepo.keys.map(_.value.toInt).max + 1).toString)
  }

  final val matchRepo = TrieMap.empty[Match.Id, Match]

  override def findMatch(id: Option[Match.Id]) = for {
    matchId   <- id
    theMatch  <- matchRepo.get(matchId)
  } yield {
    theMatch
  }

  override def findMatches(state: Match.MatchState) = matchRepo
    .collect { case (_, theMatch) if theMatch.state == state => theMatch }
    .toSeq

  override def storeMatch(theMatch: Match) = theMatch.id match {
    case Some(id) => {
      matchRepo += ((id, theMatch))

      theMatch.id
    }
    case None     => {
      val id = generateId
      val matchWithId = theMatch.copy(id = Some(id))

      matchRepo += ((id, matchWithId))

      matchWithId.id
    }
  }

  override def changeMatchState(
      id: Option[Match.Id],
      state: Match.MatchState
  ) = for {
    foundMatch  <- findMatch(id)
    newMatch     = foundMatch.copy(state = state)
    _           <- storeMatch(newMatch)
  } yield {
    newMatch.state
  }

}
