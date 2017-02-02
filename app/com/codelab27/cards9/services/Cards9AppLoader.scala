package com.codelab27.cards9.services

import play.api.ApplicationLoader
import play.api.ApplicationLoader.Context
import play.api.BuiltInComponentsFromContext
import play.api.db.slick.{ SlickComponents, DbName }
import play.api.routing.Router
import play.api.routing.sird._

class Cards9AppLoader extends ApplicationLoader {
  def load(context: Context) = new Cards9Components(context).application
}

class Cards9Components(context: Context) extends BuiltInComponentsFromContext(context) with SlickComponents {
  val dbName = new DbName(configuration.getString("db.name").getOrElse("default"))

  lazy val assets = new controllers.Assets(httpErrorHandler)
  override lazy val router = Router.from {
    case GET(p"/assets/$file*") => assets.versioned("/public", file)
  }
}
