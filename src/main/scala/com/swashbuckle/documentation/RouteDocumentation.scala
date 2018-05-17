package com.swashbuckle.documentation

import com.swashbuckle.documentation.Fields._

import scala.meta._

case class PathSegment(name: String, value: String)

case class RouteDef(name: String, value: String)

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

  private[documentation] def extractCode(source: Source): (String, Seq[Stat]) = source match {
    case source"..$packages" => packages.collect {
      case q"package $packageName { ..$code }" => (packageName.toString, code)
    }.groupBy(_._1).map(item => (item._1, item._2.flatMap(_._2))).head
  }

  private[documentation] def extractImports(code: Seq[Stat]): Seq[String] = {
    code.collect {
      case q"import ..$imported" => imported.map(_.toString)
    }.flatten
  }

  private[documentation] def extractTraitBody(code: Seq[Stat]): Seq[Stat] = {
    code.collect {
      case q"..$mods trait $tname[..$tparams] extends $template" => template.collect {
        case template"{ ..$stats1 } with ..$inits { $self => ..$stats2 }" => stats2
      }.flatten
    }.flatten
  }

  private[documentation] def extractSegments(code: Seq[Stat]): Seq[PathSegment] = {
    def inQuotes(value: Term): Boolean = {
      value.toString.startsWith("\"") && value.toString.endsWith("\"")
    }
    code.collect {
      case q"..$mods val ..$name = $value" if inQuotes(value) => PathSegment(name.head.toString, value.toString)
    }
  }

  private[documentation] def extractRouteDefs(code: Seq[Stat]): Seq[RouteDef] = {
    def isRouteDef(value: Term): Boolean = {
      value.toString.startsWith("path(") || value.toString.startsWith("pathPrefix(")
    }
    code.collect {
      case q"..$mods val ..$name = $value" if isRouteDef(value) => RouteDef(name.head.toString, value.toString)
    }
  }
}
