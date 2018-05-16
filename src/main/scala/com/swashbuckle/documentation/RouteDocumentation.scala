package com.swashbuckle.documentation

import scala.meta._

case class RouteDocumentation(operationId: String, method: Method, path: String, parameters: List[Parameter], responses: List[ApiResponse], produces: String)

object RouteDocumentation {
  def apply(source: Source): RouteDocumentation = {
    ???
  }
}
