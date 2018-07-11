package com.swashbuckle.service

import akka.actor.ActorSystem
import akka.event.LoggingAdapter
import akka.http.scaladsl.Http
import akka.http.scaladsl.Http.ServerBinding
import akka.http.scaladsl.server.Route
import akka.stream.Materializer
import com.swashbuckle.config.{MicroServiceConfig, MicroServiceConfigComponent}

import scala.concurrent.{ExecutionContextExecutor, Future}

trait MicroService extends MicroServiceConfigComponent {
  implicit val system: ActorSystem
  implicit val executor: ExecutionContextExecutor
  implicit val materializer: Materializer

  val config: MicroServiceConfig

  val logger: LoggingAdapter

  def route: Route

  def startUp(): Future[ServerBinding] = {
    Http().bindAndHandle(route, config.interface, config.port)
  }
}
