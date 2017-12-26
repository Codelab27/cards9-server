package com.codelab27.cards9.routes

import com.codelab27.cards9.binders.Cards9Binders._
import com.codelab27.cards9.controllers.MatchMakerController

import play.api.routing.Router
import play.api.routing.sird._

class GameRouter[MM[_]](
    matchMakerController: MatchMakerController[MM]
) {

  lazy val routes = Router.from {
    case GET(p"/matches/${matchState(state)}")                   => matchMakerController.getMatchesForState(state)
    case POST(p"/matches/create")                                => matchMakerController.createMatch()
  }

}
