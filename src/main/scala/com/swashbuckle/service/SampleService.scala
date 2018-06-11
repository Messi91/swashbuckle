package com.swashbuckle.service

import scala.concurrent.Future

class SampleService {
  def createMessage(message: Message): Future[Message] = Future.successful(message)

  def updateMessage(uuid: Long, message: Message): Future[Message] = Future.successful(message)

  def getMessages: Future[Seq[Message]] = Future.successful(Message.example :: Message.example :: Nil)

  def getMessage(uuid: Long): Future[Option[Message]] = Future.successful(Some(Message.example))

  def deleteMessage(uuid: Long): Future[Boolean] = Future.successful(true)
}
