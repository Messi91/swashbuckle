package com.swashbuckle.service

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

class SampleService {

  private var repo = Seq.empty[Message]

  def createMessage(message: Message): Future[Message] = {
    Future {
      repo = repo :+ message
      message
    }
  }

  def updateMessage(id: Long, message: Message): Future[Message] = {
    Future {
      repo = repo.map {
        case found @ Message(messageId, _, _) if messageId == id => found.copy(sender = message.sender, content = message.content)
        case other => other
      }
      message
    }
  }

  def getMessages: Future[Seq[Message]] = Future.successful(repo)

  def getMessage(id: Long): Future[Option[Message]] = Future.successful(repo.find(_.id == id))

  def deleteMessage(id: Long): Future[Boolean] = {
    Future {
      val temp = repo.filterNot(_.id == id)
      val result = temp.size < repo.size
      repo = temp
      result
    }
  }
}
