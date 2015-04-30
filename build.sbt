import sbt.Project.projectToRef

lazy val jsProjects = Seq(client)


lazy val commonSettings = Seq(
  version := "0.8",
  organization := "in.ernet.iisc.math",
  scalaVersion := "2.11.5",
  resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/",
  resolvers += "Akka Snapshot Repository" at "http://repo.akka.io/snapshots/",
  libraryDependencies ++= Seq(
    "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.3",
  "org.scala-lang.modules" %% "scala-xml" % "1.0.3",
  "org.spire-math" %% "spire" % "0.9.1"
  ),
  scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")
  )

lazy val jvmSettings = Seq(
  libraryDependencies ++= Seq("com.lihaoyi" %% "ammonite-repl" % "0.2.7" % "test")
  )

lazy val serverSettings = Seq(
  name := "ProvingGround-jvm",
  libraryDependencies ++= Seq("com.typesafe.akka" %% "akka-actor" % "2.4-SNAPSHOT",
  ws,
  "org.reactivemongo" %% "play2-reactivemongo" % "0.10.5.0.akka23",
  "edu.stanford.nlp" % "stanford-corenlp" % "3.4",
  "edu.stanford.nlp" % "stanford-corenlp" % "3.4" classifier "models",
  "edu.stanford.nlp" % "stanford-parser" % "3.4",
  "com.vmunier" %% "play-scalajs-scripts" % "0.2.0",
  "org.webjars" % "jquery" % "1.11.1"
  ),
  scalaJSProjects := jsProjects,
  pipelineStages := Seq(scalaJSProd),
  initialCommands in console := """ammonite.repl.Repl.main(null); import provingground._ ; import HoTT._"""
  )

  lazy val digressionSettings = Seq(
    name := "ProvingGround-Digressions",
    libraryDependencies ++= Seq("com.typesafe.akka" %% "akka-actor" % "2.4-SNAPSHOT"
    ),
    scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")
    )


    lazy val client = project.
      settings(name := "ProvingGround-JS",
      scalaVersion := "2.11.5",
      persistLauncher := true,
      persistLauncher in Test := false,
      sourceMapsDirectories += coreJS.base / "..",
      unmanagedSourceDirectories in Compile := Seq((scalaSource in Compile).value),
      libraryDependencies ++= Seq(
        "org.scala-js" %%% "scalajs-dom" % "0.8.0",
        "com.lihaoyi" %%% "scalatags" % "0.4.6"
        )
        ).
        enablePlugins(ScalaJSPlugin, ScalaJSPlay).
        dependsOn(coreJS)



lazy val core = (crossProject.crossType(CrossType.Pure) in  file("core")).
  settings(commonSettings : _*).
  settings(name := "ProvingGround-Core").
  jsConfigure(_ enablePlugins ScalaJSPlay).
  jsSettings(sourceMapsBase := baseDirectory.value / "..")

lazy val coreJVM = core.jvm
lazy val coreJS = core.js

lazy val functionfinder = project.
  settings(commonSettings: _*).
  settings(name := "ProvingGround-FunctionFinder").
  dependsOn(coreJVM)

lazy val jvm = (project in file("jvm")).enablePlugins(PlayScala).
        settings(commonSettings : _*).
        settings(jvmSettings : _*).
        settings(serverSettings : _*).
        aggregate(jsProjects.map(projectToRef): _*).
        dependsOn(coreJVM).dependsOn(functionfinder)

lazy val realfunctions = (project in file("realfunctions")).
        settings(commonSettings : _*).
        settings(jvmSettings : _ *).
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
//            initialCommands in console := """ammonite.repl.Repl.main(null)""",
            name := "RealFunctions")



lazy val digressions = (project in file("digressions")).
  settings(commonSettings : _*).
  settings(digressionSettings : _*).
  dependsOn(coreJVM).dependsOn(jvm).dependsOn(functionfinder)

  EclipseKeys.skipParents in ThisBuild := false

// unmanagedBase in Compile <<= baseDirectory(_ / "scalalib")
