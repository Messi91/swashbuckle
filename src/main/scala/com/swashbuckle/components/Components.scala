package com.swashbuckle.components

import com.swashbuckle.components.Components.PathParameterTypes.PathParameterType
import com.swashbuckle.components.Components.QueryParameterTypes.QueryParameterType

object Components {

  type Schema = Seq[(String, String)]

  case class SchemaDefinition(name: String, schema: Schema)

  sealed trait Method

  case object Get extends Method

  case object Post extends Method

  case object Put extends Method

  case object Delete extends Method


  sealed trait Parameter

  case class PathParameter(name: String, `type`: PathParameterType) extends Parameter

  case class QueryParameter(name: String, `type`: QueryParameterType, required: Boolean) extends Parameter

  case class ArrayQueryParameter(name: String, `type`: QueryParameterType, collectionFormat: String, required: Boolean) extends Parameter

  case class BodyParameter(name: String, schema: String) extends Parameter

  case class Response(status: Int, schema: String)


  case class PathSegment(name: String, value: String)

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

  object QueryParameterTypes extends Enumeration {

    type QueryParameterType = Value

    val IntType = Value(0, "Int")

    val LongType = Value(1, "Long")

    val DoubleType = Value(2, "Double")

    val StringType = Value(4, "String")

    val BooleanType = Value(5, "Boolean")
  }
}
