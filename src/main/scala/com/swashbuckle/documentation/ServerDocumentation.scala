package com.swashbuckle.documentation

import com.swashbuckle.components.Components._
import com.swashbuckle.components.Fields._

import scala.meta._

object ServerDocumentation {
  def apply(source: Source): Seq[RouteDef] = {
    val (packageName, code) = extractCode(source)
    val imports = extractImports(code)
    val traitBody = extractTraitBody(code)
    val pathSegments = extractSegments(traitBody)
    extractRouteDefs(traitBody)
  }

  private def extractCode(source: Source): (String, Seq[Stat]) = source match {
    case source"..$packages" => packages.collect {
      case q"package $packageName { ..$code }" => (packageName.toString, code)
    }.groupBy(_._1).map(item => (item._1, item._2.flatMap(_._2))).head
  }

  private def extractImports(code: Seq[Stat]): Seq[String] = {
    code.collect {
      case q"import ..$imported" => imported.map(_.toString)
    }.flatten
  }

  private def extractTraitBody(code: Seq[Stat]): Seq[Stat] = {
    code.collect {
      case q"..$mods trait $tname[..$tparams] extends $template" => template.collect {
        case template"{ ..$stats1 } with ..$inits { $self => ..$stats2 }" => stats2
      }.flatten
    }.flatten
  }

  private def extractSegments(code: Seq[Stat]): Seq[PathSegment] = {
    def inQuotes(value: Term): Boolean = {
      value.toString.startsWith("\"") && value.toString.endsWith("\"")
    }

    code.collect {
      case q"..$mods val ..$name = $value" if inQuotes(value) => PathSegment(name.head.toString, value.toString)
    }
  }

  private def extractRouteDefs(code: Seq[Stat]): Seq[RouteDef] = {
    def isRouteDef(routeDef: Term): Boolean = {
      routeDef.toString.startsWith("path(") || routeDef.toString.startsWith("pathPrefix(")
    }

    code.collect {
      case q"..$mods val ..$name = $routeDef" if isRouteDef(routeDef) =>
        val routeName = name.head.toString
        val (path, pathParameters) = extractPath(routeDef.toString)
        val methodDefs = extractMethodDefs(routeDef.toString)
        methodDefs.map { methodDef =>
          RouteDef(
            name = routeName,
            method = methodDef.method,
            path = path,
            parameters = pathParameters ++ extractQueryParameters(methodDef.body),
            responses = Nil
          )
        }
    }.flatten
  }

  private def extractMethodDefs(routeDef: String): Seq[MethodDef] = {
    "get {" :: "post {" :: "put {" :: "delete {" :: Nil collect {
      case methodStart if routeDef.contains(methodStart) =>
        val start = routeDef.indexOf(methodStart)
        val finish = routeDef.indexOf("}")
        val body = routeDef.substring(start, finish)
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

  private def extractPath(routeDef: String): (Seq[String], Seq[PathParameter]) = {
    val keyword = "pathPrefix("
    val start = routeDef.indexOf(keyword) + keyword.length
    val finish = routeDef.indexOf("\n")
    val segment = routeDef.substring(start, finish).trim
    val path = segment.substring(0, segment.indexOf(")")).split("/").map(_.trim)

    if(segment.endsWith("=>")) {
      (path, extractPathParameters(segment))
    }
    else (path, Nil)
  }

  private def extractPathParameters(pathDef: String): Seq[PathParameter] = {
    val start = pathDef.indexOf("{")
    val finish = pathDef.indexOf("=>")

  }

  private def extractQueryParameters(methodDef: String): Seq[Parameter] = {
    val keyword = "parameters("
    if (methodDef.contains(keyword)) {
      val start = methodDef.indexOf(keyword) + keyword.length
      val finish = methodDef.indexOf(")")
      val queryParameters = methodDef.substring(start, finish).split(",").map(word => word.trim)
      queryParameters.map { parameter =>
        val Array(name, typeName) = parameter.split(".as\\(")
        val required = !parameter.endsWith("?")
        val trimmedType = typeName.split("\\)").head
        if (trimmedType.contains("[")) {
          val collectionType = trimmedType.split("\\[").head
          val innerType = trimmedType.substring(trimmedType.indexOf("[") + 1, trimmedType.indexOf("]"))
          ArrayQueryParameter(
            name = name.drop(0),
            `type` = innerType,
            collectionFormat = collectionType,
            required = required
          )
        } else {
          QueryParameter(
            name = name.drop(0),
            `type` = trimmedType,
            required = required
          )
        }
      }
    } else Nil
  }
}
