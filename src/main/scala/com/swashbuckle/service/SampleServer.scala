package com.swashbuckle.service

import akka.http.scaladsl.server.Route
import com.swashbuckle.documentation.SwashbuckleDocumentation

abstract class SampleServer extends MicroService with SampleRoute with SwashbuckleDocumentation {

  override def route: Route = sampleRoute ~ documentationRoute
}
