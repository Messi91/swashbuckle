package com.swashbuckle.annotations

import com.swashbuckle.sample.SampleRoute
import org.scalatest.FunSpec

class SwashbuckleRouteTest extends FunSpec {

  describe("annotating a route trait") {
    it("should print out its body") {
      new SampleRoute {}
    }
  }
}
