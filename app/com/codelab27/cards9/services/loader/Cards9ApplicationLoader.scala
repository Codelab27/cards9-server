package com.codelab27.cards9.services.loader

import com.codelab27.cards9.controllers.MatchMakerController
import com.codelab27.cards9.repos.matches.mem.MatchRepositoryInMemoryInterpreter
import com.codelab27.cards9.routes.GameRouter

import play.api.ApplicationLoader.Context
import play.api.{ApplicationLoader, BuiltInComponentsFromContext, NoHttpFiltersComponents}

class Cards9ApplicationLoader extends ApplicationLoader {

  override def load(context: ApplicationLoader.Context) = new Cards9Components(context).application

}

class Cards9Components(context: Context) extends BuiltInComponentsFromContext(context) with NoHttpFiltersComponents {

  import com.codelab27.cards9.utils.DefaultCatsInstances._

  lazy val matchRepo = MatchRepositoryInMemoryInterpreter
  lazy val matchMakerController = new MatchMakerController(controllerComponents, matchRepo)

  lazy val router = new GameRouter(matchMakerController)

}
