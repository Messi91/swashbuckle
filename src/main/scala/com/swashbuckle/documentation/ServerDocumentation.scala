package com.swashbuckle.documentation

import com.swashbuckle.components.Components._
import com.swashbuckle.components.Fields._
import com.swashbuckle.components.ParameterTypes
import com.swashbuckle.components.ParameterTypes.ParameterType

import scala.meta._
import scala.util.Try

object ServerDocumentation {
  def apply(source: Source): Seq[RouteDef] = {
    val (packageName, code) = extractCode(source)
    val imports = extractImports(code)
    val traitBody = extractTraitBody(code)
    val pathSegments = extractSegments(traitBody)
    extractRouteDefs(traitBody, pathSegments)
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

    def sanitizeString(str: String) = {
      str.replaceAll("\"", "")
    }

    code.collect {
      case q"..$mods val ..$name = $value" if inQuotes(value) => PathSegment(name.head.toString, sanitizeString(value.toString))
    }
  }

  private def extractRouteDefs(code: Seq[Stat], pathSegments: Seq[PathSegment]): Seq[RouteDef] = {
    def isRouteDef(routeDef: Term): Boolean = {
      routeDef.toString.startsWith("path(") || routeDef.toString.startsWith("pathPrefix(")
    }

    code.collect {
      case q"..$mods val ..$name = $routeDef" if isRouteDef(routeDef) =>
        val routeName = name.head.toString
        val (path, pathParameters) = extractPath(routeDef.toString, pathSegments)
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

  private def extractPath(routeDef: String, pathSegments: Seq[PathSegment]): (Seq[String], Seq[PathParameter]) = {
    val keyword = "pathPrefix("
    val start = routeDef.indexOf(keyword) + keyword.length
    val finish = routeDef.indexOf("\n")
    val segment = routeDef.substring(start, finish).trim
    val path = segment.substring(0, segment.indexOf(")")).split("/").map(_.trim).toSeq
    if(segment.endsWith("=>")) {
      val pathParameters = extractPathParameters(path, segment)
      val integratedPath = integratePathWithSegments(integratePathWithParameters(path, pathParameters), pathSegments)
      (integratedPath, pathParameters)
    } else (path, Nil)
  }

  private def extractPathParameters(path: Seq[String], pathDef: String): Seq[PathParameter] = {
    val start = pathDef.indexOf("{")
    val finish = pathDef.indexOf("=>")
    val segment = pathDef.substring(start + 1, finish).trim
    val parameterNames: Seq[String] = {
      if (segment == "_") Seq("?")
      else if (segment.startsWith("case")) {
        val innerStart = segment.indexOf("case (")
        val innerFinish = segment.lastIndexOf(")")
        val innerSegment = segment.substring(innerStart, innerFinish)
        innerSegment.split(",").map(_.trim).toSeq
      }
      else if (segment.startsWith("(")) {
        val innerStart = segment.indexOf("(")
        val innerFinish = segment.lastIndexOf(")")
        val innerSegment = segment.substring(innerStart, innerFinish)
        innerSegment.split(",").map(_.trim).toSeq
      }
      else Seq(segment)
    }
    val parameterTypes: Seq[ParameterType] = path.flatMap(getParameterType)
    val pairSequence: Seq[(String, ParameterType)] = parameterNames.zip(parameterTypes)
    pairSequence.map { case (name, typeName) => PathParameter(name, typeName.toString) }
  }

  private def getParameterType(pathSegment: String): Option[ParameterType] = {
    val parameterType = {
      if (pathSegment.startsWith("PathMatchers.")) {
        pathSegment.split("\\.")(1)
      } else pathSegment
    }
    Try(ParameterTypes.withName(parameterType)).toOption
  }

  private def isParameterType(pathSegment: String): Boolean = {
    getParameterType(pathSegment).nonEmpty
  }

  private def integratePathWithParameters(path: Seq[String], pathParameters: Seq[PathParameter]): Seq[String] = {
    pathParameters match {
      case Nil => path
      case _ => path match {
        case Nil => Nil
        case treasure if isParameterType(treasure.head) => Seq(s"{${pathParameters.head.name}}") ++ integratePathWithParameters(treasure.tail, pathParameters.tail)
        case other => Seq(other.head) ++ integratePathWithParameters(other.tail, pathParameters)
      }
    }
  }

  private def integratePathWithSegments(path: Seq[String], segments: Seq[PathSegment]): Seq[String] = {
    val segmentMap = segments.map { segment => (segment.name, segment.value) }.toMap
    path.foldLeft(Seq.empty[String]) { (list, segmentName) =>
      list ++ Seq(segmentMap.getOrElse(segmentName, segmentName))
    }
  }

  private def extractQueryParameters(methodDef: String): Seq[Parameter] = {
    val keyword = "parameters("
    if (methodDef.contains(keyword)) {
      val start = methodDef.indexOf(keyword) + keyword.length
      val finish = methodDef.indexOf(")")
      val queryParameters = methodDef.substring(start, finish).split(",").map(word => word.trim).toSeq
      queryParameters.map { parameter =>
        val Array(name, typeName) = parameter.split(".as\\(")
        val required = !parameter.endsWith("?")
        val trimmedType = typeName.split("\\)").head
        if (trimmedType.contains("[")) {
          val collectionType = trimmedType.split("\\[").head
          val innerType = trimmedType.substring(trimmedType.indexOf("[") + 1, trimmedType.indexOf("]"))
          ArrayQueryParameter(
            name = name.drop(1),
            `type` = innerType,
            collectionFormat = collectionType,
            required = required
          )
        } else {
          QueryParameter(
            name = name.drop(1),
            `type` = trimmedType,
            required = required
          )
        }
      }
    } else Nil
  }
}
