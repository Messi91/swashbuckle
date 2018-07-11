package com.swashbuckle.service

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.{Directives, PathMatchers, Route}
import akka.http.scaladsl.unmarshalling.PredefinedFromStringUnmarshallers.CsvSeq
import akka.stream.Materializer
import fommil.sjs.FamilyFormats
import spray.json._

import scala.concurrent.ExecutionContext

trait SampleRoute extends Directives with SprayJsonSupport with DefaultJsonProtocol with FamilyFormats {

  implicit def routeEc: ExecutionContext
  implicit val materializer: Materializer

  private val pathSegment = "path"
  private val toPathSegment = "to"
  private val messagesPathSegment = "messages"

  val service = new SampleService

  private val createMessageRoute = pathPrefix(pathSegment / "to" / messagesPathSegment) {
    pathEndOrSingleSlash {
      post {
        entity(as[Message]) { message =>
          onSuccess(service.createMessage(message)) { response =>
            complete(StatusCodes.Created, response)
          }
        }
      }
    }
  }

  private val updateMessageRoute = pathPrefix("path" / toPathSegment / PathMatchers.JavaUUID / messagesPathSegment / PathMatchers.LongNumber) { case (uuid, id) =>
    pathEndOrSingleSlash {
      put {
        entity(as[Message]) { update =>
          onSuccess(service.updateMessage(id, update)) { response =>
            complete(StatusCodes.OK, response)
          }
        }
      }
    }
  }

  private val getMessagesRoute = pathPrefix(pathSegment / toPathSegment / "messages") {
    pathEndOrSingleSlash {
      get {
        parameters('ids.as(CsvSeq[Long]).?, 'type.as[String].?, 'isPositive.as[Boolean].?) { (_, _, _) =>
          onSuccess(service.getMessages) { response =>
            complete(StatusCodes.OK, response)
          }
        }
      }
    }
  }

  private val getMessageRoute = pathPrefix("path" / toPathSegment / messagesPathSegment / PathMatchers.LongNumber) { id =>
    pathEndOrSingleSlash {
      get {
        onSuccess(service.getMessage(id)) {
          case Some(message) => complete(StatusCodes.OK, message)
          case None => complete(StatusCodes.NotFound, "Message not found")
        }
      }
    }
  }

  private val deleteMessageRoute = pathPrefix(pathSegment / "to" / messagesPathSegment / PathMatchers.LongNumber) { id =>
    pathEndOrSingleSlash {
      delete {
        onSuccess(service.deleteMessage(id)) { response =>
          complete(StatusCodes.OK)
        }
      }
    }
  }

  val sampleRoute: Route = createMessageRoute ~
    updateMessageRoute ~
    getMessagesRoute ~
    getMessageRoute ~
    deleteMessageRoute
}
