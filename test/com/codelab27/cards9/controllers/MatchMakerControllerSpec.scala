package com.codelab27.cards9.controllers

import com.codelab27.cards9.models.matches.Match
import com.codelab27.cards9.serdes.json.DefaultFormats._
import com.codelab27.cards9.services.loader.Cards9Components
import com.codelab27.cards9.specs.ModelSpec

import org.scalatestplus.play.components.OneAppPerSuiteWithComponents

import play.api.libs.json.JsSuccess
import play.api.test.Helpers._
import play.api.test.{FakeRequest, Helpers}

class MatchMakerControllerSpec extends ModelSpec with OneAppPerSuiteWithComponents {

  override def components = new Cards9Components(context)

  "The match api" when {

    "requested to create a match" should {

      "return a match id that can be used to retrieve it" in {

        val Some(creationRequest) = Helpers.route(app, FakeRequest("POST", "/matches"))
        def retrieveMatch(id: Match.Id) = Helpers.route(app, FakeRequest("GET", s"/matches/${id.value}"))

        val jsonMatchId = Helpers.contentAsJson(creationRequest).validate[Match.Id]
        jsonMatchId shouldBe a[JsSuccess[_]]

        for {
          matchId <- jsonMatchId
        } yield {

          val Some(retrievalRequest) = retrieveMatch(matchId)

          val jsonMatch = Helpers.contentAsJson(retrievalRequest).validate[Match]
          jsonMatch shouldBe a[JsSuccess[_]]

          for (theMatch <- jsonMatch) yield {
            theMatch.id shouldBe defined
            theMatch.id.map(_ shouldBe matchId)
          }

        }

      }

    }

  }

}
