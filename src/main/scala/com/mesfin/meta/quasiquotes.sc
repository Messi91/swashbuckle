import scala.meta._

q"case class User(name: String, age: Int)"

val method = q"def `is a baby` = age < 1"
