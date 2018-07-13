package com.swashbuckle.documentation

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.http.scaladsl.server.{Directives, Route}
import com.swashbuckle.config.SwashbuckleConfigComponent
import com.swashbuckle.cors.CorsSupport
import com.swashbuckle.json.SwashbuckleJsonSupport
import spray.json._

trait SwashbuckleDocumentation extends SwashbuckleConfigComponent with Directives with SwashbuckleJsonSupport with CorsSupport {
  val documentationRoute: Route = pathPrefix("documentation") {
    corsHandler(pathEndOrSingleSlash {
      get {
        complete(ServerDocumentation(swashbuckleConfig).toJson.prettyPrint)
      }
    })
  }
}
