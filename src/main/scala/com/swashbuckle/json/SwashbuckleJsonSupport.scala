package com.swashbuckle.json

import com.swashbuckle.components.Components.PathParameterTypes.PathParameterType
import com.swashbuckle.components.Components.PrimitiveTypes.PrimitiveType
import com.swashbuckle.components.Components._
import com.swashbuckle.documentation.ServerDocumentation
import spray.json.{JsString, _}

trait SwashbuckleJsonSupport {

  private def translateType(`type`: PathParameterType): (String, Option[String]) = `type` match {
    case PathParameterTypes.IntNumber => ("integer", Some("int32"))
    case PathParameterTypes.LongNumber => ("integer", Some("int64"))
    case PathParameterTypes.DoubleNumber => ("number", Some("double"))
    case PathParameterTypes.JavaUUID => ("string", Some("uuid"))
  }

  private def translateType(`type`: PrimitiveType): (String, Option[String]) = `type` match {
    case PrimitiveTypes.IntType => ("integer", Some("int32"))
    case PrimitiveTypes.LongType => ("integer", Some("int64"))
    case PrimitiveTypes.DoubleType => ("number", Some("double"))
    case PrimitiveTypes.StringType => ("string", None)
    case PrimitiveTypes.BooleanType => ("boolean", None)
  }

//  {
//    "name":"tags",
//    "in":"query",
//    "description":"Tags to filter by",
//    "required":true,
//    "type":"array",
//    "items":{
//      "type":"string"
//    },
//    "collectionFormat":"multi"
//  }

  implicit object ParameterFormat extends JsonWriter[Parameter] {
    override def write(parameter: Parameter): JsValue = parameter match {
      case PathParameter(name, parameterType) =>
        val (typeName, formatOpt) = translateType(parameterType)
        val default = JsObject(
          "name" -> JsString(name),
          "in" -> JsString("path"),
          "required" -> JsBoolean(true),
          "type" -> JsString(typeName)
        )
        formatOpt match {
          case Some(format) => default.copy(fields = default.fields + ("format" -> JsString(format)))
          case None => default
        }

      case QueryParameter(name, primitiveType, required) =>
        val (typeName, formatOpt) = translateType(primitiveType)
        val default = JsObject(
          "name" -> JsString(name),
          "in" -> JsString("query"),
          "required" -> JsBoolean(required),
          "type" -> JsString(typeName)
        )
        formatOpt match {
          case Some(format) => default.copy(fields = default.fields + ("format" -> JsString(format)))
          case None => default
        }

      case ArrayQueryParameter()
    }
  }

  implicit object ResponseFormat extends JsonWriter[Response] {
    override def write(obj: Response): JsValue = {
      ???
    }
  }

  implicit object PathFormat extends JsonWriter[Path] {
    override def write(obj: Path): JsValue = {
      ???
    }
  }

  implicit object DefinitionFormat extends JsonWriter[Definition] {
    override def write(obj: Definition): JsValue = {
      ???
    }
  }

  implicit object ServerDocumentationFormat extends JsonWriter[ServerDocumentation] {
    override def write(obj: ServerDocumentation): JsValue = {
      ???
    }
  }
}
