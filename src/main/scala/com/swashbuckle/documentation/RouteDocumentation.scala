package com.swashbuckle.documentation

import com.swashbuckle.documentation.Fields._

import scala.meta._

case class RouteDocumentation(
  operationId: String,
  method: Method,
  path: String,
  parameters: List[Parameter],
  responses: List[Response]
)

object RoutesDocumentation {
  def apply(source: Source): List[RouteDocumentation] = {
    ???
  }
}
