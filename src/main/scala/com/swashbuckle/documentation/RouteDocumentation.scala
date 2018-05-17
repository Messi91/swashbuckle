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

  def extractImports(source: Source): Seq[String] = source match {
    case source"..$packages" => packages.collect {
      case q"package $packageName { ..$code }" => code.collect {
        case q"import ..$imported" => imported.map(_.toString)
      }.flatten
    }.flatten
  }


}
