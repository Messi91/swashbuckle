package com.swashbuckle.service

import akka.http.scaladsl.server.Route

abstract class SampleServer extends MicroService with SampleRoute {

  override def route: Route = sampleRoute
}
