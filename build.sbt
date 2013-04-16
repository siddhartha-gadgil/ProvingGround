name := "ProvingGround"

version := "0.3"

scalaVersion := "2.10.1"

libraryDependencies <+= scalaVersion { "org.scala-lang" % "scala-swing" % _ }
