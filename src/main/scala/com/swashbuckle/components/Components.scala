package com.swashbuckle.components

import com.swashbuckle.components.Components.PathParameterTypes.PathParameterType
import com.swashbuckle.components.Components.PrimitiveTypes.PrimitiveType

object Components {

  type DefinitionFields = Seq[(String, String)]

  case class Definition(name: String, schema: DefinitionFields)

  sealed trait Method

  case object Get extends Method

  case object Post extends Method

  case object Put extends Method

  case object Delete extends Method


  sealed trait Parameter

  case class PathParameter(name: String, `type`: PathParameterType) extends Parameter

  case class QueryParameter(name: String, `type`: PrimitiveType, required: Boolean) extends Parameter

  case class ArrayQueryParameter(name: String, `type`: PrimitiveType, collectionFormat: String, required: Boolean) extends Parameter

  case class BodyParameter(name: String, schema: String) extends Parameter

  case class Response(status: Int, message: Option[String], schema: Option[String], isArray: Boolean)


  case class Field(name: String, value: String)

  case class MethodDef(
    method: Method,
    body: String
  )

  case class RouteDef(
    name: String,
    method: Method,
    path: Seq[String],
    parameters: Seq[Parameter],
    responses: Seq[Response]
  )

  case class ServerDocumentation()

  object PathParameterTypes extends Enumeration {

    type PathParameterType = Value

    val IntNumber = Value(0, "IntNumber")

    val LongNumber = Value(1, "LongNumber")

    val DoubleNumber = Value(2, "DoubleNumber")

    val JavaUUID = Value(3, "JavaUUID")
  }

  object PrimitiveTypes extends Enumeration {

    type PrimitiveType = Value

    val IntType = Value(0, "Int")

    val LongType = Value(1, "Long")

    val DoubleType = Value(2, "Double")

    val StringType = Value(4, "String")

    val BooleanType = Value(5, "Boolean")
  }
}
