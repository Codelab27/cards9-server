package com.codelab27.cards9.routes

import com.codelab27.cards9.binders.Cards9Binders._
import com.codelab27.cards9.controllers.MatchMakerController

import play.api.routing.Router.Routes
import play.api.routing.SimpleRouter
import play.api.routing.sird._

class GameRouter[MM[_]](
    matchMakerController: MatchMakerController[MM]
) extends SimpleRouter {

  lazy val routes: Routes = {
    case GET(p"/matches/${matchState(state)}") => {
      matchMakerController.getMatchesForState(state)
    }
    case POST(p"/matches/players/${playerId(id)}") => {
      matchMakerController.createMatch(id)
    }
    case POST(p"/matches/${matchId(id)}/players/${playerId(player)}/${playerAction(action)}") => {
      matchMakerController.playerActionOnMatch(id, player, action)
    }
  }

}
