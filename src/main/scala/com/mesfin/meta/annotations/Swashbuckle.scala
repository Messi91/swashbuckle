package com.mesfin.meta.annotations

import scala.annotation.StaticAnnotation
import scala.collection.immutable
import scala.meta._

class Swashbuckle extends StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
    defn match {
      case q"trait $routeName { ..$body }" =>
        val newBody = immutable.Seq(
          q"println(${body.mkString("\n")})"
        ) ++ body
        q"trait $routeName { ..$newBody }"
      case _ =>
        abort("@Swashbuckle must annotate a trait belonging to a route.")
    }
  }
}
