package com.mesfin.meta.tutorial.part_1_tokenization

import scala.meta._

object Tokenization extends App {
  val someCode =
    """
    def testMethod = {
      println("printing");
    }
  """.tokenize

  val tokens = someCode match {
    case Tokenized.Success(t) => t
    case Tokenized.Error(_, _, details) => throw new Exception(details)
  }

  println(tokens.tokens)
  println(tokens.structure)
  println(tokens.syntax)
}
