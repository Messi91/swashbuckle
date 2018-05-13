package com.mesfin.meta.sample

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import com.mesfin.meta.annotations.SwashbuckleRoute
import fommil.sjs.FamilyFormats
import spray.json.DefaultJsonProtocol

@SwashbuckleRoute
trait SampleRoute extends SprayJsonSupport with DefaultJsonProtocol with FamilyFormats {
  private val createPathPrefix = "create"
  private val userPathSegment = "user"
  private val organisationPathSegment = "organisation"


}
