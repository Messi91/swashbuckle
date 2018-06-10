package com.swashbuckle.documentation

import java.io.File

import com.swashbuckle.components.Components._
import com.swashbuckle.components.Components.PathParameterTypes._
import com.swashbuckle.components.Components.QueryParameterTypes._
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
          `type` = LongNumber
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
          `type` = LongType
        ) :: QueryParameter(
          name = "type",
          `type` = StringType,
          required = false
        ) :: QueryParameter(
          name = "isPositive",
          `type` = BooleanType,
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
          `type` = LongNumber
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
          `type` = LongNumber
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
