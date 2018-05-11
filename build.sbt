name := "swashbuckle"

version := "1.0"

scalaVersion := "2.12.2"

resolvers += Resolver.sonatypeRepo("releases")
addCompilerPlugin("org.scalameta" % "paradise_2.12.2" % "3.0.0-M8")

libraryDependencies ++= Seq(
  "org.scalameta" %% "scalameta" % "1.8.0",
  "org.scalatest" %% "scalatest" % "3.0.1" % Test
)
