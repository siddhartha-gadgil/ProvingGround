name := "ProvingGround"

version := "0.8"

organization := "in.ernet.iisc.math"

libraryDependencies += "com.lihaoyi" %% "ammonite-repl" % "0.2.4" % "test"

initialCommands in console := "ammonite.repl.Repl.main(null)"

scalaVersion := "2.11.5"


resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.1.0"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.3"

libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.0.3"

libraryDependencies ++= Seq(
  ws
)

libraryDependencies ++= Seq(
  "org.reactivemongo" %% "play2-reactivemongo" % "0.10.5.0.akka23"
)

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

unmanagedBase in Compile <<= baseDirectory(_ / "scalalib")
