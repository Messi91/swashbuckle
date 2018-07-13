package com.swashbuckle.documentation

import java.io.File

import com.swashbuckle.components.Components.PathParameterTypes.PathParameterType
import com.swashbuckle.components.Components.PrimitiveTypes.PrimitiveType
import com.swashbuckle.components.Components._
import com.swashbuckle.config.SwashbuckleConfig

import scala.meta._
import scala.util.Try

case class ServerDocumentation(
  paths: Seq[Path],
  definitions: Seq[Definition],
  config: SwashbuckleConfig
)

object ServerDocumentation {

  private val rootDir = "src/main/scala/"

  private val statusMap = Map(
    "StatusCodes.OK" -> 200,
    "StatusCodes.Created" -> 201,
    "StatusCodes.BadRequest" -> 400,
    "StatusCodes.NotFound" -> 404
  )

  def apply(config: SwashbuckleConfig): ServerDocumentation = {
    val source = new File(config.entrypoint).parse[Source].get
    val (packageName, code) = extractCode(source)
    val extensionNames = getAllExtensionNames(code)
    val imports = extractImports(code)
    val body = extractBody(code)
    val mainRoutes = extractMainRoutes(body)
    val extensionCodes = extensionNames.flatMap { className =>
      findClassByName(className, code, packageName, imports).map((className, _))
    }
    val routeHolders = getRouteHolders(extensionCodes, mainRoutes)
    val documentations = routeHolders.flatMap { case (sourceName, chosenRoutes) =>
      findSourceByName(sourceName, code, packageName, imports).map(createDocumentation(_, chosenRoutes, config))
    }
    ServerDocumentation(
      paths = documentations.flatMap(_.paths).distinct.sortBy(_.url.mkString("/")),
      definitions = documentations.flatMap(_.definitions).distinct.sortBy(_.name),
      config = config
    )
  }

  private def createDocumentation(source: Source, chosenRoutes: Seq[String], config: SwashbuckleConfig): ServerDocumentation = {
    val (packageName, code) = extractCode(source)
    val imports = extractImports(code)
    val traitBody = extractBody(code)
    val pathSegments = extractPathSegments(traitBody)
    val objects = extractObjectDefinitions(traitBody)
    val paths = extractPaths(traitBody, pathSegments, objects, code, packageName, imports, chosenRoutes)
    val definitions = paths.collect {
      case Path(_, _, _, parameters, responses) => parameters.collect {
        case BodyParameter(_, schemaName) =>
          extractSchemaDefinition(schemaName, code, packageName, imports)
      }.flatten ++ responses.collect {
        case Response(_, _, Some(schemaName), _) =>
          extractSchemaDefinition(schemaName, code, packageName, imports)
      }.flatten
    }.flatten.distinct
    ServerDocumentation(
      paths = paths,
      definitions = definitions,
      config = config
    )
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

  private def extractBody(code: Seq[Stat]): Seq[Stat] = {
    code.collect {
      case q"..$mods trait $tname[..$tparams] extends $template" => template.collect {
        case template"{ ..$stats1 } with ..$inits { $self => ..$stats2 }" => stats2
      }.flatten
      case q"..$mods class $tname[..$tparams] ..$ctorMods (...$paramss) extends $template" => template.collect {
        case template"{ ..$stats1 } with ..$inits { $self => ..$stats2 }" => stats2
      }.flatten
    }.flatten
  }

  private def getAllExtensionNames(code: Seq[Stat]): Seq[String] = {
    code.collect {
      case q"..$mods trait $tname[..$tparams] extends $template" => template.collect {
        case template"{ ..$stats1 } with ..$inits { $self => ..$stats2 }" => inits.map(_.toString)
      }.flatten
      case q"..$mods class $tname[..$tparams] ..$ctorMods (...$paramss) extends $template" => template.collect {
        case template"{ ..$stats1 } with ..$inits { $self => ..$stats2 }" => inits.map(_.toString)
      }.flatten
    }.flatten
  }

  private def extractMainRoutes(code: Seq[Stat]): Seq[String] = {
    code.collect {
      case q"..$mods val ..$name = $value" if name.head.toString == "route" => getRoutes(value.toString)
      case q"..$mods val ..$name: $tpeopt = $value" if name.head.toString == "route" => getRoutes(value.toString)
      case q"..$mods def $name[..$tparams](...$paramss): $tpeopt = $expr" if name.toString == "route" => getRoutes(expr.toString)
    }.flatten
  }

  private def getRouteHolders(codes: Seq[(String, Stat)], routeNames: Seq[String]): Seq[(String, Seq[String])] = {
    def isChosenOne(name: String): Boolean = {
      routeNames.contains(name)
    }

    codes.flatMap { code =>
      val body = extractBody(code._2 :: Nil)
      body.collect {
        case q"..$mods val ..$name = $value" if isChosenOne(name.head.toString) => Some((code._1, getRoutes(value.toString)))
        case q"..$mods val ..$name: $tpeopt = $value" if isChosenOne(name.head.toString) => Some((code._1, getRoutes(value.toString)))
        case _ => None
      }.flatten
    }
  }

  private def getRoutes(routeStr: String): Seq[String] = {
    if (routeStr.contains("~")) routeStr.split("~").map(_.trim) else Seq(routeStr)
  }

  private def extractPathSegments(code: Seq[Stat]): Seq[Field] = {
    code.collect {
      case q"..$mods val ..$name = $value" if inQuotes(value.toString) => Field(name.head.toString, sanitizeString(value.toString))
    }
  }

  private def extractObjectDefinitions(code: Seq[Stat]): Seq[Field] = {
    def isObject(value: Term): Boolean = {
      value.toString.startsWith("new ")
    }

    code.collect {
      case q"..$mods val ..$name = $value" if isObject(value) => Field(name.head.toString, value.toString.split(" ")(1))
    }
  }

  private def findClass(className: String, code: Seq[Stat]): Option[Stat] = {
    code.collect {
      case clazz @ q"..$mods class $tname[..$tparams] ..$ctorMods (...$paramss) extends $template"
        if tname.value == className => clazz
      case traitt @ q"..$mods trait $tname[..$tparams] extends $template"
        if tname.value == className => traitt
    }.headOption
  }

  private def findClassByName(className: String, currentSource: Seq[Stat], currentPackage: String, imports: Seq[String]): Option[Stat] = {
    def searchPackages: Option[Stat] = {
      val paths = (Seq(currentPackage) ++ imports).map(packageName => rootDir + packageName.replaceAll("\\.", "/"))
      val sources = getSourceFiles(paths).map(_.parse[Source].get)
      val code = sources.map(extractCode).map(_._2)
      code.flatMap(findClass(className, _)).headOption
    }

    def searchCurrentSource: Option[Stat] = {
      findClass(className, currentSource)
    }

    def getSourceFiles(paths: Seq[String]): Seq[File] = {
      paths.map(new File(_)).filter(_ != null).flatMap { directory =>
        Try(directory.listFiles.filter(_.isFile)).toOption.toSeq.flatMap(_.toSeq)
      }
    }

    searchCurrentSource match {
      case clazz @ Some(_) => clazz
      case None => searchPackages
    }
  }

  private def findSourceByName(className: String, currentSource: Seq[Stat], currentPackage: String, imports: Seq[String]): Option[Source] = {
    def searchPackages: Option[Source] = {
      val paths = (Seq(currentPackage) ++ imports).map(packageName => rootDir + packageName.replaceAll("\\.", "/"))
      getSourceFiles(paths).map(_.parse[Source].get).find { source =>
        findClass(className, extractCode(source)._2).nonEmpty
      }
    }

    def getSourceFiles(paths: Seq[String]): Seq[File] = {
      paths.map(new File(_)).filter(_ != null).flatMap { directory =>
        Try(directory.listFiles.filter(_.isFile)).toOption.toSeq.flatMap(_.toSeq)
      }
    }

    searchPackages
  }

  private def extractSchemaDefinition(schemaName: String, currentSource: Seq[Stat], currentPackage: String, imports: Seq[String]): Seq[Definition] = {
    def findDefinition: Option[Definition] = {
      findClassByName(schemaName, currentSource,currentPackage, imports).collect {
        case q"..$mods class $tname[..$tparams] ..$ctorMods (...$paramss) extends $template" if tname.value == schemaName =>
          Definition(
            name = schemaName,
            schema = paramss.flatten.map { param =>
              (param.name.value, param.decltpe.map(_.toString).getOrElse(""))
            }
          )
      }
    }

    def needsDefinition(typeName: String): Boolean = {
      (typeName != schemaName) && !PrimitiveTypes.values.map(_.toString).contains(typeName)
    }

    val surfaceDefinition = findDefinition.toSeq
    val nonPrimitiveTypes = surfaceDefinition.flatMap(_.schema.map(_._2).filter(needsDefinition))
    surfaceDefinition ++ nonPrimitiveTypes.flatMap(extractSchemaDefinition(_, currentSource, currentPackage, imports))
  }

  private def extractPaths(code: Seq[Stat], pathSegments: Seq[Field], objects: Seq[Field],
    currentSource: Seq[Stat], currentPackage: String, imports: Seq[String], chosenRoutes: Seq[String]): Seq[Path] = {
    def isRouteDef(routeDef: Term): Boolean = {
      routeDef.toString.startsWith("path(") || routeDef.toString.startsWith("pathPrefix(")
    }

    def extractMethodDefs(routeDef: String): Seq[MethodDef] = {
      "get {" :: "post {" :: "put {" :: "delete {" :: Nil collect {
        case methodStart if routeDef.contains(methodStart) =>
          val start = routeDef.indexOf(methodStart)
          val finish = routeDef.indexOf("}")
          val body = routeDef.substring(start, finish)
          val method = getMethodType(methodStart)
          MethodDef(method = method, body = body)
      }
    }

    def extractQueryParameters(methodDef: String): Seq[Parameter] = {
      def pickSplitRegex(parameter: String) = {
        if (parameter.contains(".as(")) ".as\\("
        else ".as\\["
      }

      val keyword = "parameters("
      if (methodDef.contains(keyword)) {
        val start = methodDef.indexOf(keyword) + keyword.length
        val finish = methodDef.indexOf("=>")
        val parametersStr = methodDef.substring(start, finish).split("\\{")(0).trim
        val queryParameters = parametersStr.split(",").map(word => word.trim.replaceAll("\\)", "")).toSeq
        queryParameters.map { parameter =>
          val splitRegex = pickSplitRegex(parameter)
          val Array(name, typeName) = parameter.split(splitRegex)
          val required = !parameter.endsWith("?")
          val trimmedType = typeName.split("\\)").head
          if (trimmedType.contains("[")) {
            val collectionType = trimmedType.split("\\[").head
            val innerType = trimmedType.substring(trimmedType.indexOf("[") + 1, trimmedType.indexOf("]"))
            ArrayQueryParameter(
              name = name.drop(1),
              `type` = getQueryParameterType(innerType).get,
              collectionFormat = collectionType,
              required = required
            )
          } else {
            val innerType = trimmedType.substring(0, trimmedType.indexOf("]"))
            QueryParameter(
              name = name.drop(1),
              `type` = getQueryParameterType(innerType).get,
              required = required
            )
          }
        }
      } else Nil
    }

    def extractBodyParameter(methodDef: String): Option[BodyParameter] = {
      val keyword = "entity(as["
      if (methodDef.contains(keyword)) {
        val start = methodDef.indexOf(keyword) + keyword.length
        val finish = methodDef.indexOf("=>")
        val segment = methodDef.substring(start, finish)
        val typeFinish = segment.indexOf("])")
        val nameStart = segment.indexOf("{") + 1
        val typeName = segment.substring(0, typeFinish).trim
        val name = segment.substring(nameStart).trim
        Some(BodyParameter(name = name, schema = typeName))
      } else None
    }

    def getFunctionReturnType(clazz: Seq[Stat], functionName: String): Option[String] = {
      clazz.collect {
        case q"..$mods def $ename[..$tparams](...$paramss): $tpeopt = $expr" if ename.value == functionName => tpeopt.toString
      }.headOption
    }

    def getCompletions(string: String, schemaName: String): Seq[(Int, Option[String], Option[String])] = {
      val keyword = "complete("
      if (!string.contains(keyword)) Nil
      else {
        val start = string.indexOf(keyword) + keyword.length
        val segment = string.substring(start).trim
        val response = segment.substring(0, segment.indexOf(")")).trim
        val completion = if (response.contains(",")) {
          val Array(status, message) = response.split(",").map(_.trim)
          val statusCode = statusMap.getOrElse(status, 200)

          if (inQuotes(message)) (statusCode, Some(sanitizeString(message)), None)
          else (statusCode, None, Some(schemaName))
        } else {
          statusMap.get(response) match {
            case Some(statusCode) => (statusCode, None, None)
            case None => (200, None, Some(schemaName))
          }
        }
        val theRest = string.substring(string.length - segment.length)
        Seq(completion) ++ getCompletions(theRest, schemaName)
      }
    }

    def extractResponses(methodDef: String, objects: Seq[Field], currentSource: Seq[Stat], currentPackage: String, imports: Seq[String]): Seq[Response] = {
      val keyword = "onSuccess("
      if (methodDef.contains(keyword)) {
        val start = methodDef.indexOf(keyword) + keyword.length
        val segment = methodDef.substring(start)
        val serviceCall = segment.substring(0, segment.indexOf(")"))
        val Array(serviceObj, functionSegment) = serviceCall.split("\\.").map(_.trim)
        objects.find(_.name == serviceObj).flatMap { service =>
          val functionName = if (functionSegment.contains("(")) functionSegment.split("\\(")(0) else functionSegment
          val serviceClass = findClassByName(service.value, currentSource, currentPackage, imports)
          serviceClass.map(sc => extractBody(Seq(sc))) match {
            case Some(parentClass) => getFunctionReturnType(parentClass, functionName).map { returnType =>
              val beforeInnerType = returnType.lastIndexOf("[") + 1
              val afterInnerType = returnType.indexOf("]")
              val innerType = returnType.substring(beforeInnerType, afterInnerType)
              val isArray = returnType.contains("Seq[") || returnType.contains("List[")
              val responses = getCompletions(segment, innerType).map { case (status, message, schema) =>
                Response(
                  status = status,
                  message = message,
                  schema = schema,
                  isArray = isArray
                )
              }
              responses
            }
            case None => None
          }

        }.getOrElse(Nil)
      } else Nil
    }

    code.collect {
      case q"..$mods val ..$name = $routeDef" if chosenRoutes.contains(name.head.toString) && isRouteDef(routeDef) =>
        val routeName = name.head.toString
        val (path, pathParameters) = extractPath(routeDef.toString, pathSegments)
        val methodDefs = extractMethodDefs(routeDef.toString)
        methodDefs.map { methodDef =>
          Path(
            name = routeName,
            method = methodDef.method,
            url = path,
            parameters = pathParameters ++ extractQueryParameters(methodDef.body) ++ extractBodyParameter(methodDef.body),
            responses = extractResponses(methodDef.body, objects, currentSource, currentPackage, imports)
          )
        }
    }.flatten
  }

  private def getMethodType(str: String): Method = str match {
    case "get {" => Get
    case "post {" => Post
    case "put {" => Put
    case "delete {" => Delete
  }

  private def extractPath(routeDef: String, pathSegments: Seq[Field]): (Seq[String], Seq[PathParameter]) = {
    def isParameterType(pathSegment: String): Boolean = {
      getPathParameterType(pathSegment).nonEmpty
    }

    def integratePathWithParameters(path: Seq[String], pathParameters: Seq[PathParameter]): Seq[String] = {
      pathParameters match {
        case Nil => path
        case _ => path match {
          case Nil => Nil
          case treasure if isParameterType(treasure.head) => Seq(s"{${pathParameters.head.name}}") ++ integratePathWithParameters(treasure.tail, pathParameters.tail)
          case other => Seq(other.head) ++ integratePathWithParameters(other.tail, pathParameters)
        }
      }
    }

    def integratePathWithSegments(path: Seq[String], segments: Seq[Field]): Seq[String] = {
      val segmentMap = segments.map { segment => (segment.name, segment.value) }.toMap
      path.foldLeft(Seq.empty[String]) { (list, segmentName) =>
        list ++ Seq(segmentMap.getOrElse(segmentName, segmentName))
      }
    }

    def extractPathParameters(path: Seq[String], pathDef: String): Seq[PathParameter] = {
      val start = pathDef.indexOf("{") + 1
      val finish = pathDef.indexOf("=>")
      val segment = pathDef.substring(start, finish).trim
      val parameterNames: Seq[String] = {
        if (segment == "_") Seq("?")
        else if (segment.startsWith("case") || segment.startsWith("(")) {
          val innerStart = segment.indexOf("(") + 1
          val innerFinish = segment.lastIndexOf(")")
          val innerSegment = segment.substring(innerStart, innerFinish)
          innerSegment.split(",").map(_.trim).toSeq
        }
        else Seq(segment)
      }
      val parameterTypes: Seq[PathParameterType] = path.flatMap(getPathParameterType)
      val pairSequence: Seq[(String, PathParameterType)] = parameterNames.zip(parameterTypes)
      pairSequence.map { case (name, typeName) => PathParameter(name, typeName) }
    }

    val keyword = "pathPrefix("
    val start = routeDef.indexOf(keyword) + keyword.length
    val finish = routeDef.indexOf("\n")
    val segment = routeDef.substring(start, finish).trim
    val path = integratePathWithSegments(segment.substring(0, segment.indexOf(")")).split("/").map(_.trim).toSeq, pathSegments)
    if(segment.endsWith("=>")) {
      val pathParameters = extractPathParameters(path, segment)
      val integratedPath = integratePathWithParameters(path, pathParameters)
      (sanitize(integratedPath), pathParameters)
    } else (sanitize(path), Nil)
  }

  private def getPathParameterType(pathSegment: String): Option[PathParameterType] = {
    val parameterType = {
      if (pathSegment.startsWith("PathMatchers.")) {
        pathSegment.split("\\.")(1)
      } else pathSegment
    }
    Try(PathParameterTypes.withName(parameterType)).toOption
  }

  private def getQueryParameterType(parameterType: String): Option[PrimitiveType] = {
    Try(PrimitiveTypes.withName(parameterType)).toOption
  }

  private def sanitizeString(str: String): String = {
    str.replaceAll("\"", "")
  }

  private def sanitize(pathParameters: Seq[String]): Seq[String] = {
    pathParameters.map(sanitizeString)
  }

  private def inQuotes(value: String): Boolean = {
    value.startsWith("\"") && value.toString.endsWith("\"")
  }
}
