package com.swashbuckle.components

import Fields._

object Fields {

  sealed trait Method

  case object Get extends Method

  case object Post extends Method

  case object Put extends Method

  case object Delete extends Method


  sealed trait Parameter

  case class PathParameter(name: String, `type`: String) extends Parameter

  case class QueryParameter(name: String, `type`: String, required: Boolean) extends Parameter

  case class ArrayQueryParameter(name: String, `type`: String, collectionFormat: String, required: Boolean) extends Parameter

  case class BodyParameter(name: String, schema: String) extends Parameter


  case class Response(status: Int, schema: String)
}

object ParameterTypes extends Enumeration {

  type ParameterType = Value

  val IntNumber = Value(0, "IntNumber")

  val LongNumber = Value(1, "LongNumber")

  val DoubleNumber = Value(2, "DoubleNumber")

  val JavaUUID = Value(3, "JavaUUID")
}

object Components {

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

  // case class ServerDocumentation()
}
