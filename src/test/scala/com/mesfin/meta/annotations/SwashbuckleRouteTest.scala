package com.mesfin.meta.annotations

import com.mesfin.meta.sample.SampleRoute
import org.scalatest.FunSpec

class SwashbuckleRouteTest extends FunSpec {

  describe("annotating a route trait") {
    it("should print out its body") {
      new SampleRoute {}
    }
  }
}
