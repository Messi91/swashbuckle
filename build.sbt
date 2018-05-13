name := "swashbuckle"

version := "1.0"

scalaVersion := "2.12.2"

resolvers += Resolver.sonatypeRepo("releases")
addCompilerPlugin("org.scalameta" % "paradise_2.12.2" % "3.0.0-M8")

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor"           % dependencies.akkaV,
  "com.typesafe.akka" %% "akka-stream"          % dependencies.akkaV,
  "com.typesafe.akka" %% "akka-http-core"       % dependencies.akkaHttpV,
  "com.typesafe.akka" %% "akka-http"            % dependencies.akkaHttpV,
  "com.typesafe.akka" %% "akka-http-spray-json" % dependencies.akkaHttpV,
  "com.typesafe.akka" %% "akka-http-jackson"    % dependencies.akkaHttpV,
  "com.typesafe.akka" %% "akka-http-xml"        % dependencies.akkaHttpV,
  "com.typesafe.akka" %% "akka-http-testkit"    % dependencies.akkaHttpV,
  "com.github.fommil" %% "spray-json-shapeless" % dependencies.fommilV,
  "org.scalameta"     %% "scalameta"            % dependencies.scalametaV,
  "org.scalatest"     %% "scalatest"            % dependencies.scalatestV % Test
)

lazy val dependencies = new {
  val akkaV       = "2.4.16"
  val akkaHttpV   = "10.0.1"
  val fommilV     = "1.4.0"
  val scalametaV  = "1.8.0"
  val scalatestV  = "3.0.1"
}
