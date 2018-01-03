package com.codelab27.cards9.routes

import com.codelab27.cards9.binders.Cards9Binders._
import com.codelab27.cards9.controllers.MatchMakerController
import com.codelab27.cards9.models.matches.Match.IsReady

import play.api.routing.Router.Routes
import play.api.routing.SimpleRouter
import play.api.routing.sird._

class GameRouter[MM[_]](
    matchMakerController: MatchMakerController[MM]
) extends SimpleRouter {

  lazy val routes: Routes = {
    // Create a match
    case POST(p"/matches")                                                                       => {
      matchMakerController.createMatch()
    }

    // Match retrieval
    case GET(p"/matches/${pbeMatchId(id)}")                                                      => {
        matchMakerController.retrieveMatch(id)
    }
    case GET(p"/matches" & q"state=${state}")                                                    => {
      matchMakerController.getMatchesForState(pbMatchState.bind("state", state))
    }

    // Fill the color slot with a player (join) or remove it (leave)
    case PUT(p"/matches/${pbeMatchId(id)}/${pbeColor(color)}/${pbePlayerId(playerId)}")          => {
      matchMakerController.addPlayer(id, color, playerId)
    }
    case DELETE(p"/matches/${pbeMatchId(id)}/${pbeColor(color)}")                                => {
      matchMakerController.removePlayer(id, color)
    }

    // Makes the player in the color slot ready or not ready
    case PUT(p"/matches/${pbeMatchId(id)}/${pbeColor(color)}/ready")                             => {
      matchMakerController.setReadiness(id, color, IsReady(true))
    }
    case DELETE(p"/matches/${pbeMatchId(id)}/${pbeColor(color)}/ready")                          => {
      matchMakerController.setReadiness(id, color, IsReady(false))
    }
  }

}
