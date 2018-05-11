package com.mesfin.meta.annotations

import org.scalatest.FunSpec

class SwashBuckleTest extends FunSpec {

  describe("annotating a route trait") {
    it("should print out its body") {
      new SampleRoute {}
    }
  }
}

@Swashbuckle
trait SampleRoute {
  private val createPathPrefix = "create"
  private val userPathSegment = "user"
  private val organisationPathSegment = "organisation"
}
