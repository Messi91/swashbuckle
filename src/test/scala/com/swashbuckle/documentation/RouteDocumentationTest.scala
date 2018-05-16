package com.swashbuckle.documentation

import java.io.File

import com.swashbuckle.documentation.Fields._
import org.scalatest.{FunSpec, Matchers}

import scala.meta._

class RouteDocumentationTest extends FunSpec with Matchers {

  describe("a route documentation") {
    it("should be created from an appropriate source file") {
      val createMessageRoute = RouteDocumentation(
        operationId = "createMessage",
        method = Post,
        path = "/path/to/messages",
        parameters = BodyParameter(
          name = "message",
          schema = "com.swashbuckle.service.Message"
        ) :: Nil,
        responses = Response(
          status = 200,
          schema = "com.swashbuckle.service.Message"
        ) :: Nil
      )

      val updateMessageRoute = RouteDocumentation(
        operationId = "updateMessage",
        method = Put,
        path = "/path/to/messages/{id}",
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

      val getMessagesRoute = RouteDocumentation(
        operationId = "getMessages",
        method = Get,
        path = "/path/to/messages",
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

      val getMessageRoute = RouteDocumentation(
        operationId = "getMessage",
        method = Get,
        path = "/path/to/messages/{id}",
        parameters = PathParameter(
          name = "id",
          `type` = "long"
        ) :: Nil,
        responses = Response(
          status = 200,
          schema = "com.swashbuckle.service.Message"
        ) :: Nil
      )

      val deleteMessageRoute = RouteDocumentation(
        operationId = "deleteMessage",
        method = Delete,
        path = "/path/to/messages/{id}",
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

      expected shouldBe RoutesDocumentation(source)
    }
  }
}
