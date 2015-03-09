name := "ProvingGround"

version := "0.8"

organization := "in.ernet.iisc.math"

scalaVersion := "2.11.5"


resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies ++= Seq("com.typesafe.akka" %% "akka-actor" % "2.1.0",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.3",
  "org.scala-lang.modules" %% "scala-xml" % "1.0.3",
  "com.lihaoyi" %% "ammonite-repl" % "0.2.4" % "test",
  ws,
  "org.reactivemongo" %% "play2-reactivemongo" % "0.10.5.0.akka23",
  "edu.stanford.nlp" % "stanford-corenlp" % "3.4",
  "edu.stanford.nlp" % "stanford-corenlp" % "3.4" classifier "models",
  "edu.stanford.nlp" % "stanford-parser" % "3.4"
  )


initialCommands in console := "ammonite.repl.Repl.main(null)"


lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

unmanagedBase in Compile <<= baseDirectory(_ / "scalalib")
