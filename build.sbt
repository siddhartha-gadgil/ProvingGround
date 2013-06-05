name := "ProvingGround"

version := "0.3"

scalaVersion := "2.10.1"

libraryDependencies <+= scalaVersion { "org.scala-lang" % "scala-swing" % _ }

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"
 
libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.1.0"

libraryDependencies += "org.scalafx" % "scalafx_2.10" % "1.0.0-M4"

libraryDependencies += "joda-time" % "joda-time" % "2.2"

libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.1.0"

libraryDependencies += "edu.stanford.nlp" % "stanford-corenlp" % "1.3.4" classifier("models") classifier("")

libraryDependencies += "xom" % "xom" % "1.2.5"
