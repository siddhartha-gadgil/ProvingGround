lazy val commonSettings = Seq(
  version := "0.8",
  organization := "in.ernet.iisc.math",
  scalaVersion := "2.11.5",
  resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/",
  resolvers += "Akka Snapshot Repository" at "http://repo.akka.io/snapshots/",
  libraryDependencies ++= Seq(
    "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.3",
  "org.scala-lang.modules" %% "scala-xml" % "1.0.3"
  ),
  scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")
  )

lazy val jvmSettings = Seq(
  libraryDependencies ++= Seq("com.lihaoyi" %% "ammonite-repl" % "0.2.4" % "test"),
  initialCommands in console := "ammonite.repl.Repl.main(null)"
  )

lazy val serverSettings = Seq(
  name := "ProvingGround-jvm",
  libraryDependencies ++= Seq("com.typesafe.akka" %% "akka-actor" % "2.4-SNAPSHOT",
  ws,
  "org.reactivemongo" %% "play2-reactivemongo" % "0.10.5.0.akka23",
  "edu.stanford.nlp" % "stanford-corenlp" % "3.4",
  "edu.stanford.nlp" % "stanford-corenlp" % "3.4" classifier "models",
  "edu.stanford.nlp" % "stanford-parser" % "3.4"
  ),
  initialCommands in console := "ammonite.repl.Repl.main(null)"
  )

  lazy val digressionSettings = Seq(
    name := "ProvingGround-Digressions",
    libraryDependencies ++= Seq("com.typesafe.akka" %% "akka-actor" % "2.4-SNAPSHOT"
    ),
    scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")
    )


lazy val core = (project in file("core")).
  settings(commonSettings : _*).
  settings(name := "ProvingGround-Core")

lazy val jvm = (project in file("jvm")).enablePlugins(PlayScala).
        settings(commonSettings : _*).
        settings(jvmSettings : _*).
        settings(serverSettings : _*).
        dependsOn(core)

lazy val realfunctions = (project in file("realfunctions")).
        settings(commonSettings : _*).
        settings(libraryDependencies  ++= Seq(
          // other dependencies here
          "org.scalanlp" %% "breeze" % "0.11.2",
          // native libraries are not included by default. add this if you want them (as of 0.7)
          // native libraries greatly improve performance, but increase jar sizes.
          "org.scalanlp" %% "breeze-natives" % "0.11.2",
          "com.lihaoyi" %% "ammonite-repl" % "0.2.7" % "test"
          ),
          resolvers ++= Seq(
            // other resolvers here
            // if you want to use snapshot builds (currently 0.12-SNAPSHOT), use this.
            "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/",
            "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"
            ),
            name := "RealFunctions")



lazy val digressions = (project in file("digressions")).
  settings(commonSettings : _*).
  settings(digressionSettings : _*).
  dependsOn(core).dependsOn(jvm)

// unmanagedBase in Compile <<= baseDirectory(_ / "scalalib")
