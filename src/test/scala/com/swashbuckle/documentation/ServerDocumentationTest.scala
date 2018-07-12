package com.swashbuckle.documentation

import java.io.File

import com.swashbuckle.components.Components._
import com.swashbuckle.components.Components.PathParameterTypes._
import com.swashbuckle.components.Components.PrimitiveTypes._
import com.swashbuckle.json.SwashbuckleJsonSupport
import org.scalatest.{FunSpec, Matchers}
import spray.json._

import scala.collection.mutable.ArrayBuffer
import scala.meta._

class ServerDocumentationTest extends FunSpec with Matchers with SwashbuckleJsonSupport {

  describe("a route documentation") {
    it("should be created from an appropriate source file") {
      val createMessageRoute = Path(
        name = "createMessageRoute",
        method = Post,
        url = "path" :: "to" :: "messages" :: Nil,
        parameters = BodyParameter(
          name = "message",
          schema = "Message"
        ) :: Nil,
        responses = Response(
          status = 201,
          message = None,
          schema = Some("Message"),
          isArray = false
        ) :: Nil
      )

      val updateMessageRoute = Path(
        name = "updateMessageRoute",
        method = Put,
        url = "path" :: "to" :: "{uuid}" :: "messages" :: "{id}" :: Nil,
        parameters = ArrayBuffer(PathParameter(
          name = "uuid",
          `type` = JavaUUID
        ), PathParameter(
          name = "id",
          `type` = LongNumber
        ), BodyParameter(
          name = "update",
          schema = "Message"
        )),
        responses = Response(
          status = 200,
          message = None,
          schema = Some("Message"),
          isArray = false
        ) :: Nil
      )

      val getMessagesRoute = Path(
        name = "getMessagesRoute",
        method = Get,
        url = "path" :: "to" :: "messages" :: Nil,
        parameters = ArrayQueryParameter(
          name = "ids",
          required = false,
          collectionFormat = "CsvSeq",
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
          message = None,
          schema = Some("Message"),
          isArray = true
        ) :: Nil
      )

      val getMessageRoute = Path(
        name = "getMessageRoute",
        method = Get,
        url = "path" :: "to" :: "messages" :: "{id}" :: Nil,
        parameters = PathParameter(
          name = "id",
          `type` = LongNumber
        ) :: Nil,
        responses = Response(
          status = 200,
          message = None,
          schema = Some("Message"),
          isArray = false
        ) :: Response(
          status = 404,
          message = Some("Message not found"),
          schema = None,
          isArray = false
        ) :: Nil
      )

      val deleteMessageRoute = Path(
        name = "deleteMessageRoute",
        method = Delete,
        url = "path" :: "to" :: "messages" :: "{id}" :: Nil,
        parameters = PathParameter(
          name = "id",
          `type` = LongNumber
        ) :: Nil,
        responses = Response(
          status = 200,
          message = None,
          schema = None,
          isArray = false
        ) :: Nil
      )

      val expected = ServerDocumentation(
        paths = createMessageRoute :: updateMessageRoute :: getMessagesRoute :: getMessageRoute :: deleteMessageRoute :: Nil,
        definitions = List(
          Definition("Message", List(("id", "Long"), ("sender", "Person"), ("content", "String"))),
          Definition("Person", List(("firstName", "String"), ("lastName", "String"), ("address", "Address"))),
          Definition("Address", List(("houseNumber", "Int"), ("streetName", "String"), ("postcode", "String")))
        )
      )

      val source = new File("src/main/scala/com/swashbuckle/service/SampleServer.scala").parse[Source].get

      println(ServerDocumentation(source).toJson.prettyPrint)

      expected shouldBe ServerDocumentation(source)
    }
  }
}
