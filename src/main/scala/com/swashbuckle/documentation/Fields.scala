package com.swashbuckle.documentation

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
