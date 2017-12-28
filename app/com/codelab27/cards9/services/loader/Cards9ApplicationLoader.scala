package com.codelab27.cards9.services.loader

import com.codelab27.cards9.controllers.MatchMakerController
import com.codelab27.cards9.routes.GameRouter
import com.codelab27.cards9.services.matchmaking.mem.MatchMakerInMemoryInterpreter

import play.api.ApplicationLoader.Context
import play.api.{ApplicationLoader, BuiltInComponentsFromContext, NoHttpFiltersComponents}

class Cards9ApplicationLoader extends ApplicationLoader {

  override def load(context: ApplicationLoader.Context) = new Cards9Components(context).application

}

class Cards9Components(context: Context) extends BuiltInComponentsFromContext(context) with NoHttpFiltersComponents {

  import com.codelab27.cards9.utils.DefaultCatsInstances._

  lazy val matchMaker = MatchMakerInMemoryInterpreter
  lazy val matchMakerController = new MatchMakerController(controllerComponents, matchMaker)

  lazy val router = new GameRouter(
    matchMakerController
  ).routes

}
