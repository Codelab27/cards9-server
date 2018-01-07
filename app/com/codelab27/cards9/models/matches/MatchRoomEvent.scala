package com.codelab27.cards9.models.matches

import com.codelab27.cards9.models.common.Common.Color
import com.codelab27.cards9.models.matches.Match.IsReady
import com.codelab27.cards9.models.players.Player

import enumeratum.{Enum, EnumEntry}

sealed trait MatchRoomEvent extends EnumEntry

object MatchRoomEvent extends Enum[MatchRoomEvent] {

  val values = findValues

  case class MatchCreated(matchId: Match.Id) extends MatchRoomEvent

  case class MatchFinished(matchId: Match.Id) extends MatchRoomEvent

  case class PlayerJoin(matchId: Match.Id, color: Color, playerId: Player.Id) extends MatchRoomEvent

  case class PlayerLeave(matchId: Match.Id, color: Color) extends MatchRoomEvent

  case class PlayerIsReady(matchId: Match.Id, color: Color, isReady: IsReady) extends MatchRoomEvent

}
