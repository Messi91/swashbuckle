package com.swashbuckle.service

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.{Directives, PathMatchers, Route}
import akka.http.scaladsl.unmarshalling.PredefinedFromStringUnmarshallers.CsvSeq
import fommil.sjs.FamilyFormats
import spray.json._

import scala.concurrent.Future

trait SampleRoute extends Directives with SprayJsonSupport with DefaultJsonProtocol with FamilyFormats {
  private val pathSegment = "path"
  private val toPathSegment = "to"
  private val messagesPathSegment = "messages"

  val service = new SampleService

  private val createMessageRoute = pathPrefix(pathSegment / toPathSegment / messagesPathSegment) {
    pathEndOrSingleSlash {
      post {
        entity(as[Message]) { message =>
          onSuccess(service.createMessage(message)) { response =>
            complete(StatusCodes.OK, response)
          }
        }
      }
    }
  }

  private val updateMessageRoute = pathPrefix(pathSegment / toPathSegment / PathMatchers.JavaUUID / messagesPathSegment / PathMatchers.LongNumber) { case (uuid, id) =>
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

  private val getMessagesRoute = pathPrefix(pathSegment / toPathSegment / messagesPathSegment) {
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

  private val getMessageRoute = pathPrefix(pathSegment / toPathSegment / messagesPathSegment / PathMatchers.LongNumber) { id =>
    pathEndOrSingleSlash {
      get {
        onSuccess(service.getMessage(id)) { response =>
          complete(StatusCodes.OK, response)
        }
      }
    }
  }

  private val deleteMessageRoute = pathPrefix(pathSegment / toPathSegment / messagesPathSegment / PathMatchers.LongNumber) { id =>
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

class SampleService {
  def createMessage(message: Message): Future[Message] = Future.successful(message)

  def updateMessage(uuid: Long, message: Message): Future[Message] = Future.successful(message)

  def getMessages: Future[Seq[Message]] = Future.successful(Message.example :: Message.example :: Nil)

  def getMessage(uuid: Long): Future[Option[Message]] = Future.successful(Some(Message.example))

  def deleteMessage(uuid: Long): Future[Boolean] = Future.successful(true)
}
