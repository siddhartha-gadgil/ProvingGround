name := "ProvingGround"

version := "0.8"

organization := "in.ernet.iisc.math"

scalaVersion := "2.10.4"

libraryDependencies <+= scalaVersion { "org.scala-lang" % "scala-swing" % _ }

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.1.0"

scalaSource in Compile <<= baseDirectory(_ / "src/main/scala")

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

unmanagedBase in Compile <<= baseDirectory(_ / "scalalib")
