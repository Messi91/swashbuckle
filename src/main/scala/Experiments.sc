import scala.meta._

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