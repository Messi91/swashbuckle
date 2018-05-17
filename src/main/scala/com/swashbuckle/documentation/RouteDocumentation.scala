package com.swashbuckle.documentation

import com.swashbuckle.documentation.Fields._

import scala.meta._

case class PathSegment(name: String, value: String)

case class MethodDef(
  method: Method,
  body: String
)

case class RouteDef(
  name: String,
  method: Method,
  path: List[String],
  parameters: List[Parameter],
  responses: List[Response]
)

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
    def isRouteDef(definition: Term): Boolean = {
      definition.toString.startsWith("path(") || definition.toString.startsWith("pathPrefix(")
    }
    code.collect {
      case q"..$mods val ..$name = $definition" if isRouteDef(definition) =>
        val methodDefs = extractMethodDefs(definition.toString)

        RouteDef(name.head.toString, definition.toString)
    }
  }

  private def extractMethodDefs(definition: String): Seq[MethodDef] = {
    "get {" :: "post {" :: "put {" :: "delete {" :: Nil collect {
      case methodStart if definition.contains(methodStart) =>
        val start = definition.indexOf(methodStart)
        val finish = definition.substring(start).indexOf("}")
        val body = definition.substring(start, finish)
        val method = getMethodType(methodStart)
        MethodDef(method = method, body = body)
    }
  }

  private def getMethodType(str: String): Method = str match {
    case "get {" => Get
    case "post {" => Post
    case "put {" => Put
    case "delete {" => Delete
  }

  private def extractPath(definition: String): Seq[String] = {
    val keyword = "pathPrefix("
    val start = definition.indexOf(keyword) + keyword.size
    val finish = definition.indexOf(")")
    definition.substring(start, finish).split("/").map(_.trim)
  }

  private def extractParameters(methodDef: String): Seq[QueryParameter] = {
    val keyword = "parameters("
    val start = methodDef.indexOf(keyword) + keyword.length
    val finish = methodDef.indexOf(")")
    val parameters = methodDef.substring(start, finish).split(",").map(word => word.trim)
    parameters.map { parameter =>
      val Array(name, typeName) = parameter.split("as(")
      val required = !parameter.endsWith("?")
      QueryParameter(
        name = name.drop(0),
        `type` = typeName.split(")").head,
        required = required
      )
    }
  }
}
