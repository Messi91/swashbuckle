package com.swashbuckle.json

import com.swashbuckle.components.Components.PathParameterTypes.PathParameterType
import com.swashbuckle.components.Components.PrimitiveTypes.PrimitiveType
import com.swashbuckle.components.Components._
import com.swashbuckle.documentation.ServerDocumentation
import spray.json.{JsObject, JsString, _}

trait SwashbuckleJsonSupport {

  implicit object ParameterFormat extends JsonWriter[Parameter] {
    override def write(parameter: Parameter): JsValue = parameter match {
      case PathParameter(name, parameterType) =>
        val (typeName, format) = translatePathParameterType(parameterType)
        addFormatField(
          JsObject(
            "name" -> JsString(name),
            "in" -> JsString("path"),
            "required" -> JsBoolean(true),
            "type" -> JsString(typeName)
          ),
          format
        )

      case QueryParameter(name, primitiveType, required) =>
        val (typeName, format) = translatePrimitiveType(primitiveType)
        addFormatField(
          JsObject(
            "name" -> JsString(name),
            "in" -> JsString("query"),
            "required" -> JsBoolean(required),
            "type" -> JsString(typeName)
          ),
          format
        )

      case ArrayQueryParameter(name, primitiveType, collectionFormat, required) =>
        val (typeName, format) = translatePrimitiveType(primitiveType)
          JsObject(
            "name" -> JsString(name),
            "in" -> JsString("query"),
            "required" -> JsBoolean(required),
            "type" -> JsString("array"),
            "items" -> addFormatField(JsObject("type" -> JsString(typeName)), format),
            "collectionFormat" -> JsString(translateCollectionType(collectionFormat))
          )

      case BodyParameter(name, schemaName) =>
        JsObject(
          "in" -> JsString("body"),
          "name" -> JsString(name),
          "required" -> JsBoolean(true),
          "schema" -> JsObject(
            "$ref" -> JsString(tagReference(schemaName))
          )
        )
    }
  }

  implicit object ResponseFormat extends JsonWriter[Response] {
    override def write(response: Response): JsValue = response match {
      case Response(status, None, None, _) =>
        JsObject(
          status.toString -> JsObject(
            "description" -> JsString("successful operation")
          )
        )

      case Response(status, Some(message), None, _) =>
        JsObject(
          status.toString -> JsObject(
            "description" -> JsString(message)
          )
        )

      case Response(status, None, Some(schemaName), false) =>
        JsObject(
          status.toString -> JsObject(
            "description" -> JsString("successful operation"),
            "schema" -> JsObject(
              "$ref" -> JsString(tagReference(schemaName))
            )
          )
        )

      case Response(status, None, Some(schemaName), true) =>
        JsObject(
          status.toString -> JsObject(
            "description" -> JsString("successful operation"),
            "schema" -> JsObject(
              "type" -> JsString("array"),
              "items" -> JsObject(
                "$ref" -> JsString(tagReference(schemaName))
              )
            )
          )
        )

      case _ => throw new Exception("Impossibru!")
    }
  }

  implicit object PathFormat extends JsonWriter[Path] {
    override def write(path: Path): JsValue = {
      JsObject(
        translateMethod(path.method) -> JsObject(
          "operationId" -> JsString(path.name),
          "produces" -> JsArray(
            JsString("application/json")
          ),
          "parameters" -> JsArray(
            path.parameters.map(_.toJson).toVector
          ),
          "responses" -> JsArray(
            path.responses.map(_.toJson).toVector
          )
        )
      )
    }
  }

  implicit object DefinitionFormat extends JsonWriter[Definition] {
    override def write(definition: Definition): JsValue = {
      JsObject(
        "type" -> JsString("object"),
        "properties" -> JsObject(definition.schema.map { case (fieldName, typeName) =>
          val (swaggerType, format) = translateDefinitionType(typeName)
          fieldName -> { swaggerType match {
            case Some(primitiveType) => addFormatField(JsObject(
              "type" -> JsString(primitiveType)
            ), format)
            case None => JsObject(
              "$ref" -> JsString(tagReference(typeName))
            )
          }}
        }.toMap)
      )
    }
  }

  implicit object ServerDocumentationFormat extends JsonWriter[ServerDocumentation] {
    override def write(server: ServerDocumentation): JsValue = {
      JsObject(
        "paths" -> JsObject(server.paths.map { path =>
          s"/${path.url.mkString("/")}" -> path.toJson
        }.toMap),
        "definitions" -> JsObject(server.definitions.map { definition =>
          definition.name -> definition.toJson
        }.toMap)
      )
    }
  }

  private def translatePathParameterType(`type`: PathParameterType): (String, Option[String]) = `type` match {
    case PathParameterTypes.IntNumber => ("integer", Some("int32"))
    case PathParameterTypes.LongNumber => ("integer", Some("int64"))
    case PathParameterTypes.DoubleNumber => ("number", Some("double"))
    case PathParameterTypes.JavaUUID => ("string", Some("uuid"))
  }

  private def translatePrimitiveType(`type`: PrimitiveType): (String, Option[String]) = `type` match {
    case PrimitiveTypes.IntType => ("integer", Some("int32"))
    case PrimitiveTypes.LongType => ("integer", Some("int64"))
    case PrimitiveTypes.DoubleType => ("number", Some("double"))
    case PrimitiveTypes.StringType => ("string", None)
    case PrimitiveTypes.BooleanType => ("boolean", None)
  }

  private def translateDefinitionType(`type`: String): (Option[String], Option[String]) = `type` match {
    case "Int" => (Some("integer"), Some("int32"))
    case "Long" => (Some("integer"), Some("int64"))
    case "Double" => (Some("number"), Some("double"))
    case "String" => (Some("string"), None)
    case "Boolean" => (Some("boolean"), None)
    case _ => (None, None)
  }

  private def translateCollectionType(`type`: String): String = `type` match {
    case "CsvSeq" => "csv"
  }

  private def translateMethod(method: Method): String = method match {
    case Get => "get"
    case Post => "post"
    case Put => "put"
    case Delete => "delete"
  }

  private def addFormatField(json: JsObject, format: Option[String]) = {
    format match {
      case Some(fmt) => json.copy(fields = json.fields + ("format" -> JsString(fmt)))
      case None => json
    }
  }

  private def tagReference(schemaName: String): String = s"#/definitions/$schemaName"
}
