name := "ProvingGround"

version := "0.3"

scalaVersion := "2.10.1"

libraryDependencies <+= scalaVersion { "org.scala-lang" % "scala-swing" % _ }

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"
 
libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.1.0"

