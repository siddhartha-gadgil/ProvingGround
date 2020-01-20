import sbt.Project.projectToRef

val scalaV = "2.13.1"

val ammV = "2.0.4"


scalaVersion in ThisBuild := scalaV


resolvers += Resolver.sonatypeRepo("releases")

// addCompilerPlugin("io.tryp" % "splain" % "0.2.7" cross CrossVersion.patch)

libraryDependencies += compilerPlugin(
  "org.scalameta" % "semanticdb-scalac" % "4.3.0" cross CrossVersion.full)
scalacOptions += "-Yrangepos"
// scalacOptions += "-P:splain:all:true"


lazy val jsProjects = Seq(client)

lazy val baseSettings = Seq(
  version := "0.1.0",
  organization := "io.github.siddhartha-gadgil" //, scalaVersion := scalaV
)

lazy val commonSettings = baseSettings ++ Seq(
  resolvers += "Typesafe Repository" at "https://repo.typesafe.com/typesafe/releases/",
  libraryDependencies ++= Seq(
    // "org.scala-lang" % "scala-reflect" % scalaVersion.value,
    "org.scala-lang.modules" %%% "scala-parser-combinators" % "1.1.2",
     "org.scala-lang.modules" %% "scala-xml" % "1.2.0",
    "org.typelevel" %%% "spire"         % "0.17.0-M1",
    "com.lihaoyi"   %%% "fansi"         % "0.2.8",
    "com.lihaoyi"   %%% "upickle"       % "0.9.8",
    "com.lihaoyi" %%% "fastparse" % "2.2.3",
    "com.chuusai"   %%% "shapeless"     % "2.3.2",
    "org.typelevel" %%% "cats-core"     % "2.1.0",
    "io.monix"      %%% "monix"         % "3.1.0",
    "org.scalameta" %%% "scalameta"     % "4.3.0",
    // "com.geirsson"  %%% "scalafmt-core" % "1.6.0-RC1",
    "com.lihaoyi" %%% "pprint"      % "0.5.8",
    // "com.lihaoyi"   % "ammonite"       % ammV cross CrossVersion.full,
    "com.lihaoyi"   %%% "sourcecode"    % "0.1.4"
  ),
  scalacOptions in Compile ++= Seq("-unchecked",
                                   "-deprecation",
                                   "-feature",
                                   "-language:existentials"),
  scalacOptions in (Compile, doc) ++= Seq("-diagrams",
                                          "-implicits",
                                          "-implicits-show-all"),
  testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-oD")
)

val akkaV = "2.6.1"

assemblyMergeStrategy in assembly := {
  case x if x.endsWith("io.netty.versions.properties") => MergeStrategy.first
}

lazy val jvmSettings = Seq(
  libraryDependencies ++= Seq(
    "com.lihaoyi"   % "ammonite"       % ammV cross CrossVersion.full,
    "com.lihaoyi" %% "os-lib" % "0.2.5",
    "com.github.nscala-time" %% "nscala-time"   % "2.22.0",
    "org.mongodb.scala" %% "mongo-scala-driver" % "2.8.0",
    "org.reactivemongo"      %% "reactivemongo" % "0.20.1",
    "com.typesafe.akka"      %% "akka-actor"    % akkaV,
    "com.typesafe.akka"      %% "akka-slf4j"    % akkaV,
    // "de.heikoseeberger"      %% "akka-sse"      % "2.0.0",
    "org.scalactic" %% "scalactic" % "3.0.1",
    "org.scalatest" %% "scalatest" % "3.1.0" % "test",
    "com.lihaoyi" %% "cask" % "0.2.7",
//    "ch.qos.logback" % "logback-classic" % "1.0.9",
    "com.typesafe" % "config" % "1.3.0",
    "org.apache.logging.log4j" % "log4j-core" % "2.13.0",
    // "org.mongodb"  %% "casbah" % "3.1.1",
//    "org.mongodb.scala" %% "mongo-scala-driver" % "1.0.0",
    "com.typesafe.akka" %% "akka-stream" % akkaV,
    "com.typesafe.akka" %% "akka-http"   % "10.1.11",
    // "com.typesafe.akka" %% "akka-http" % akkaV,
    "com.typesafe.akka" %% "akka-http-spray-json" % "10.1.11",
//    "com.lihaoyi" %% "upickle" % "0.3.4",
//    "com.lihaoyi" %% "ammonite-ops" % ammV,
//    "com.lihaoyi" %% "ammonite-shell" % ammV,
    // "org.scala-lang.modules" %% "scala-pickling" % "0.10.1",
    "org.slf4j"   % "slf4j-api"    % "1.7.30",
    "org.slf4j"   % "slf4j-simple" % "1.7.30",
    // Last stable release
    // "org.scalanlp" %% "breeze" % "0.13.2",
    "com.atlassian.commonmark" % "commonmark" % "0.13.1",
    "org.scalameta" %% "mdoc" % "2.1.1",
    "org.platanios" %% "tensorflow" % "0.4.0" classifier "linux-cpu-x86_64",
    "org.deeplearning4j" % "deeplearning4j-core"  % "1.0.0-beta4",
      "org.deeplearning4j" % "deeplearning4j-graph" % "1.0.0-beta4",
      "org.nd4j"           % "nd4j-native-platform" % "1.0.0-beta4",
      "org.deeplearning4j" % "deeplearning4j-nlp"   % "1.0.0-beta4",
    "org.eclipse.jgit" % "org.eclipse.jgit" % "5.6.0.201912101111-r"
  )
  // ,
  // resources in Compile += (fastOptJS in (client, Compile)).value.data
)

//lazy val logback = "ch.qos.logback" % "logback-classic" % "1.0.9"

lazy val serverSettings = Seq(
  libraryDependencies ++= Seq(
    // "org.scala-lang" % "scala-compiler" % scalaVersion.value,
    // ws,
    // "org.reactivemongo" %% "play2-reactivemongo" % "0.11.2.play24",
    // "com.vmunier" %% "play-scalajs-scripts" % "0.3.0",
    "org.webjars" % "jquery" % "1.11.1"
  ),
  // scalaJSProjects := jsProjects,
  // pipelineStages := Seq(scalaJSProd),
  initialCommands in console := """import provingground._ ; import HoTT._; /*import pprint.Config.Colors._; import pprint.pprintln*/"""
)

lazy val nlpSettings = Seq(
  libraryDependencies ++= Seq(
    "com.lihaoyi"         % "ammonite"         % ammV % "test" cross CrossVersion.full,
    "com.lihaoyi"         %% "ammonite-ops"    % ammV,
    "com.lihaoyi"   %% "upickle"       % "0.7.1",
    "edu.stanford.nlp"    % "stanford-corenlp" % "3.7.0",
    "edu.stanford.nlp"    % "stanford-corenlp" % "3.7.0" classifier "models",
    "com.google.protobuf" % "protobuf-java"    % "2.6.1",
    "edu.mit" % "jwi" % "2.2.3"
  )
)


lazy val acSettings = Seq(
  name := "AndrewsCurtis",
  libraryDependencies ++= Seq("com.typesafe.akka" %% "akka-actor" % akkaV),
  scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature"),
  initialCommands in console := """import provingground.andrewscurtis._"""
)

lazy val nfSettings = Seq(
  name := "NormalForm",
  libraryDependencies ++= Seq("org.typelevel" %% "spire" % "0.17.0-M1"),
  scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature"),
  initialCommands in console := """import provingground.normalform._ ; import provingground.normalform.NormalForm._"""
)

lazy val client = project
  .settings(
    name := "ProvingGround-JS",
    // scalaVersion := scalaV,
    // coverageEnabled := false,
    scalaJSUseMainModuleInitializer := false,
    // persistLauncher in Test := false,
    // sourceMapsDirectories += coreJS.base / "..",
    unmanagedSourceDirectories in Compile := Seq(
      (scalaSource in Compile).value),
    Compile / fastOptJS / artifactPath := baseDirectory.value / "target" / "out.js",
    // resolvers += "amateras-repo" at "http://amateras.sourceforge.jp/mvn/",
    resolvers += "jitpack" at "https://jitpack.io",
    libraryDependencies ++= Seq(
      "org.scala-js" %%% "scalajs-dom" % "0.9.2",
      "com.lihaoyi"  %%% "scalatags"   % "0.6.7",
      "com.lihaoyi"  %%% "upickle"     % "0.6.6",
      // "com.github.karasiq" %%% "scalajs-marked" % "1.0.2",
      // "com.scalawarrior" %%% "scalajs-ace" % "0.0.4" //,
      //  "com.github.kindlychung" % "sjs-katex" % "0.1"
    )
  )
  .enablePlugins(ScalaJSPlugin)
  .dependsOn(coreJS)

lazy val core = (sbtcrossproject.crossProject(JSPlatform, JVMPlatform).crossType(sbtcrossproject.CrossType.Pure) in file("core"))
  .settings(commonSettings: _*)
  .settings(name := "ProvingGround-Core")
  .settings(
    libraryDependencies ++= Seq(
//      "com.lihaoyi" %%% "upickle" % "0.3.4"
    ))
  // .jsConfigure(_ enablePlugins ScalaJSWeb)
//  .jsSettings(sourceMapsBase := baseDirectory.value / "..")

lazy val coreJVM = core.jvm
lazy val coreJS  = core.js

lazy val server = (project in file("server"))
  .settings(commonSettings: _*)
  .settings(jvmSettings: _*)
  .settings(
    name := "provingground-server",
    // scalaVersion := scalaV,
    // scalaJSProjects := Seq(client),
    // pipelineStages in Assets := Seq(scalaJSPipeline),
    // triggers scalaJSPipeline when using compile or continuous compilation
    // compile in Compile <<= (compile in Compile) dependsOn scalaJSPipeline,
    libraryDependencies ++= Seq(
      "com.typesafe.akka" %% "akka-http"       % "10.1.1",
      "com.vmunier"       %% "scalajs-scripts" % "1.1.0",
      "com.typesafe.akka" %% "akka-actor"  % akkaV,
      "com.typesafe.akka" %% "akka-stream" % akkaV,
      "com.github.scopt"  %% "scopt"       % "3.5.0"
    )
    // ,
    // resources in Compile += (fastOptJS in (client, Compile)).value.data

  )
  // .enablePlugins(JavaAppPackaging, UniversalPlugin)
  .dependsOn(coreJVM)


val initCommands =
  """import provingground._, HoTT._, induction._, ammonite.ops._, translation.FansiShow._; repl.pprinter.bind(fansiPrint)"""

lazy val leanlib =
  (project in file("leanlib"))
  .settings(commonSettings: _*)
  .settings(jvmSettings: _*)
  .dependsOn(coreJVM)

// lazy  val rootLocation: File = file(".").getAbsoluteFile

lazy val mantle = (project in file("mantle"))
  .settings(
    name := "ProvingGround-mantle",
    unmanagedResourceDirectories in Compile += {
      baseDirectory.in(root).value / "docs"
    },
    resources in Compile += (fastOptJS in (client, Compile)).value.data
    // resourceDirectory := baseDirectory.value / "docs"
    // scalaJSProjects := Seq(client),
    // pipelineStages in Assets := Seq(scalaJSPipeline)
    //  libraryDependencies += "com.lihaoyi" % "ammonite" % ammV cross CrossVersion.full,
  )
  .settings(commonSettings: _*)
  .settings(jvmSettings: _*)
//        .settings(serverSettings : _*)
  .settings(sourceGenerators in Test += Def.task {
    val file = (sourceManaged in Test).value / "amm.scala"
    IO.write(
      file,
      s"""object amm extends App { ammonite.Main("$initCommands").run() }""")
    Seq(file)
  }.taskValue)
  .dependsOn(coreJVM)
  .dependsOn(server)
  .dependsOn(trepplein)
  .dependsOn(leanlib)
  // .enablePlugins(SbtWeb
    // , TutPlugin
  // )

  lazy val crust = (project in file("crust"))
  .settings(
    name := "ProvingGround-crust",
    // resourceDirectory := baseDirectory.value / "docs"
    // scalaJSProjects := Seq(client),
    // pipelineStages in Assets := Seq(scalaJSPipeline)
    //  libraryDependencies += "com.lihaoyi" % "ammonite" % ammV cross CrossVersion.full
  )
  .settings(commonSettings: _*)
  .settings(jvmSettings: _*)
//        .settings(serverSettings : _*)
  .settings(sourceGenerators in Test += Def.task {
    val file = (sourceManaged in Test).value / "amm.scala"
    IO.write(
      file,
      s"""object amm extends App { ammonite.Main("$initCommands").run() }""")
    Seq(file)
  }.taskValue)
  .dependsOn(coreJVM)
  .dependsOn(server)
  .dependsOn(trepplein)
  .dependsOn(leanlib)
  .dependsOn(mantle)


lazy val exploring = project
  .settings(name := "ProvingGround-exploring",
            libraryDependencies += "com.lihaoyi" %% "ammonite-ops" % ammV)
  .dependsOn(coreJVM)
  .dependsOn(mantle)
  .dependsOn(crust)
  // .enablePlugins(JavaAppPackaging, UniversalPlugin)

val nlpInitCommands =
  "import scala.collection.JavaConversions._, provingground._, PennTrees._, cats._, cats.implicits._, translation._, Functors._, SubTypePattern._, TreeToMath._, StanfordParser._"

lazy val nlp = (project in file("nlp"))
  .settings(name := "ProvingGround-NLP")
  .settings(commonSettings: _*)
  .settings(nlpSettings: _*)
  .settings(sourceGenerators in Test += Def.task {
    val file = (sourceManaged in Test).value / "amm.scala"
    IO.write(
      file,
      s"""object amm extends App { ammonite.Main("$nlpInitCommands").run() }""")
    Seq(file)
  }.taskValue)
       .settings(jvmSettings : _*)
  .dependsOn(coreJVM)
  .dependsOn(mantle)
  .dependsOn(crust)

// lazy val translation = (project in file("translation"))
//   .settings(name := "ProvingGround-Translation",
//             libraryDependencies += "com.lihaoyi" %% "ammonite-ops" % ammV)
//   .settings(baseSettings: _*)
//   .dependsOn(coreJVM)

lazy val deepwalk = (project in file("deepwalk"))
  .settings(
    name := "DeepWalk4s",
    classpathTypes += "maven-plugin",
    libraryDependencies ++= Seq(
      "org.deeplearning4j" % "deeplearning4j-core"  % "1.0.0-beta4",
      "org.deeplearning4j" % "deeplearning4j-graph" % "1.0.0-beta4",
      "org.nd4j"           % "nd4j-native-platform" % "1.0.0-beta4",
      "org.deeplearning4j" % "deeplearning4j-nlp"   % "1.0.0-beta4",
//              "org.deeplearning4j" % "deeplearning4j-ui" % "1.0.0-beta4",
      "org.nd4j"    % "nd4j-native"   % "1.0.0-beta4"
    )
  )
  .settings(baseSettings: _*)
  

// lazy val playServer = (project in file("play-server"))
//   .enablePlugins(PlayScala)
//   .settings(name := "ProvingGround-Play-Server")
//   .settings(commonSettings: _*)
//   .settings(jvmSettings: _*)
//   .settings(serverSettings: _*)
//   .settings(
//     libraryDependencies += specs2 % Test,
//     resolvers += "scalaz-bintray" at "https://dl.bintray.com/scalaz/releases",
//     TwirlKeys.templateImports += "controllers._")
//   .aggregate(jsProjects.map(projectToRef): _*)
//   .dependsOn(coreJVM)
//   .dependsOn(functionfinder)
//   .dependsOn(andrewscurtis)
//   .dependsOn(mantle)
//   .dependsOn(nlp)
//   .enablePlugins(SbtWeb)

lazy val realfunctions = (project in file("realfunctions"))
  .settings(commonSettings: _*)
  .settings(jvmSettings: _*)
  .settings(
    //       libraryDependencies  ++= Seq(
    // // other dependencies here
    // //"org.scalanlp" %% "breeze" % "0.11.2",
    // // native libraries are not included by default. add this if you want them (as of 0.7)
    // // native libraries greatly improve performance, but increase jar sizes.
    // //"org.scalanlp" %% "breeze-natives" % "0.11.2"
    // ),
    // resolvers ++= Seq(
    //   // other resolvers here
    //   // if you want to use snapshot builds (currently 0.12-SNAPSHOT), use this.
    //   "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/",
    //   "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"
    //   ),
    name := "RealFunctions")

// lazy val digressions = (project in file("digressions"))
//   .settings(commonSettings: _*)
//   .settings(digressionSettings: _*)
//   .dependsOn(coreJVM)
//   .dependsOn(playServer)
//   .dependsOn(functionfinder)

// EclipseKeys.skipParents in ThisBuild := false

// unmanagedBase in Compile <<= baseDirectory(_ / "scalalib")

lazy val andrewscurtis = (project in file("andrewscurtis"))
  .settings(commonSettings: _*)
  .settings(jvmSettings: _*)
  .settings(acSettings: _*)
  .dependsOn(coreJVM)
  .dependsOn(mantle)
  .dependsOn(crust)

lazy val normalform = (project in file("normalform"))
  .settings(commonSettings: _*)
  .settings(jvmSettings: _*)
  .settings(nfSettings: _*)

lazy val trepplein = (project in file("trepplein"))

fork in run := true
connectInput := true
outputStrategy := Some(StdoutOutput)

val root = (project in file("."))
//   .settings(baseSettings: _*)
//   .enablePlugins(ScalaUnidocPlugin)
//   .settings(
//     name := "Proving-Ground",
//     unidocProjectFilter in (ScalaUnidoc, unidoc) := inAnyProject -- inProjects(
//       coreJS,
//       client)
//   )
//   .aggregate(andrewscurtis,
//              //  client,
//              coreJVM,
//              //  coreJS,
//              exploring,
//              mantle,
//              nlp,
//              normalform,
//              realfunctions,
//              server,
//              trepplein)
