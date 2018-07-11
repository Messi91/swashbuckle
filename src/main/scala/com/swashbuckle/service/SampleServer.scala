package com.swashbuckle.service

import akka.http.scaladsl.server.Route

abstract class SampleServer extends MicroService with SampleRoute {

  def route: Route = sampleRoute
}
