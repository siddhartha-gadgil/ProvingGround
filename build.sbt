name := "ProvingGround"

version := "0.8"

organization := "in.ernet.iisc.math"

scalaVersion := "2.11.5"


resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.1.0"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.3"

libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.0.3"

libraryDependencies ++= Seq(
  ws
)



scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

unmanagedBase in Compile <<= baseDirectory(_ / "scalalib")
