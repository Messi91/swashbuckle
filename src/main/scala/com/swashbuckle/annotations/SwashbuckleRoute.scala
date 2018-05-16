package com.swashbuckle.annotations

import scala.annotation.StaticAnnotation
import scala.collection.immutable
import scala.meta._

class SwashbuckleRoute extends StaticAnnotation {
//  inline def apply(defn: Any): Any = meta {
//    defn match {
//      case q"trait $routeName extends $inheritance1 with $inheritance2 with $inheritance3 with $inheritance4 { ..$body }" =>
//        val newBody = immutable.Seq(
//          q"println(${body.mkString("\n")})"
//        ) ++ body
//        q"trait $routeName { ..$newBody }"
//      case _ =>
//        abort("@SwashbuckleRoute must annotate a trait belonging to a route.")
//    }
//  }
}
