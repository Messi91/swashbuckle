package com.swashbuckle.documentation

import java.io.File

import com.swashbuckle.components.Components.RouteDef
import com.swashbuckle.components.Fields._
import org.scalatest.{FunSpec, Matchers}

import scala.meta._

class ServerDocumentationTest extends FunSpec with Matchers {

  describe("a route documentation") {
    it("should be created from an appropriate source file") {
      val createMessageRoute = RouteDef(
        name = "createMessage",
        method = Post,
        path = "path" :: "to" :: "messages" :: Nil,
        parameters = BodyParameter(
          name = "message",
          schema = "com.swashbuckle.service.Message"
        ) :: Nil,
        responses = Response(
          status = 200,
          schema = "com.swashbuckle.service.Message"
        ) :: Nil
      )

      val updateMessageRoute = RouteDef(
        name = "updateMessage",
        method = Put,
        path = "path" :: "to" :: "messages" :: "{id}" :: Nil,
        parameters = PathParameter(
          name = "id",
          `type` = "long"
        ) :: BodyParameter(
          name = "update",
          schema = "com.swashbuckle.service.Message"
        ) :: Nil,
        responses = Response(
          status = 200,
          schema = "com.swashbuckle.service.Message"
        ) :: Nil
      )

      val getMessagesRoute = RouteDef(
        name = "getMessages",
        method = Get,
        path = "path" :: "to" :: "messages" :: Nil,
        parameters = ArrayQueryParameter(
          name = "ids",
          required = false,
          collectionFormat = "csv",
          `type` = "long"
        ) :: QueryParameter(
          name = "type",
          `type` = "string",
          required = false
        ) :: QueryParameter(
          name = "isPositive",
          `type` = "boolean",
          required = false
        ) :: Nil,
        responses = Response(
          status = 200,
          schema = "Seq[com.swashbuckle.service.Message]"
        ) :: Nil
      )

      val getMessageRoute = RouteDef(
        name = "getMessage",
        method = Get,
        path = "path" :: "to" :: "messages" :: "{id}" :: Nil,
        parameters = PathParameter(
          name = "id",
          `type` = "long"
        ) :: Nil,
        responses = Response(
          status = 200,
          schema = "com.swashbuckle.service.Message"
        ) :: Nil
      )

      val deleteMessageRoute = RouteDef(
        name = "deleteMessage",
        method = Delete,
        path = "path" :: "to" :: "messages" :: "{id}" :: Nil,
        parameters = PathParameter(
          name = "id",
          `type` = "long"
        ) :: Nil,
        responses = Response(
          status = 200,
          schema = ""
        ) :: Nil
      )

      val expected = createMessageRoute :: updateMessageRoute :: getMessagesRoute :: getMessageRoute :: deleteMessageRoute :: Nil

      val source = new File("src/main/scala/com/swashbuckle/service/SampleRoute.scala").parse[Source].get

      //expected shouldBe RoutesDocumentation(source)

      println(ServerDocumentation(source))
    }
  }
}
