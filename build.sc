import mill._
import scalalib._, publish._
import scalajslib._
import mill.scalalib.scalafmt._
import define.{Sources, Task}
import ammonite.ops._
//import coursier.maven.MavenRepository
// import $ivy.`ch.epfl.scala::mill-bloop:1.1.2`
// import $ivy.`fun.valycorp::mill-ensime:0.0.1`


trait MetalsModule extends ScalaModule{
  import java.io._

  def metalsBuildInfo = T{
    def targDeps : Agg[eval.PathRef] = resolveDeps(transitiveIvyDeps, false)()

    Map[String, String](
      "sources" -> allSourceFiles().map(_.path).mkString(java.io.File.pathSeparator),
      "unmanagedSourceDirectories" -> "",
      "managedSourceDirectories" -> "",
      "scalacOptions" -> scalacOptions().mkString(" "),
      "classDirectory" -> compile().classes.path.toString,
      "dependencyClasspath" ->
        (targDeps  ++
          Task.traverse(moduleDeps)(_.sources)().flatten
        ).map(_.path).mkString(java.io.File.pathSeparator),
      "scalaVersion" -> scalaVersion(),
      "sourceJars" ->
        resolveDeps(transitiveIvyDeps, true)().map(_.path).mkString(java.io.File.pathSeparator)
      )
  }

  def metalsConfig() = T.command{
    def outFile = pwd / ".metals" / "buildinfo" / RelPath(artifactName().toString) / "main.properties"
    def info = metalsBuildInfo()
    def output = info.map{
      case (k, v) => s"$k=$v"
    }.mkString("\n")
    write.over(outFile, output)
    output
  }
}

val scalaV = "2.12.8"

val ammV = "1.6.0"


val commonLibs = List(
  ivy"org.scala-lang.modules::scala-parser-combinators::1.0.5",
  ivy"org.scala-lang.modules::scala-xml:1.1.0",
  ivy"org.typelevel::spire::0.16.0",
  ivy"com.lihaoyi::fansi::0.2.4",
  ivy"com.lihaoyi::upickle::0.7.1",
  ivy"com.lihaoyi::fastparse::2.1.0",
  ivy"com.chuusai::shapeless::2.3.3",
  ivy"org.typelevel::cats-core::1.4.0",
  ivy"io.monix::monix::3.0.0-RC2",
  ivy"com.lihaoyi::pprint::0.5.2",
  ivy"com.lihaoyi::sourcecode::0.1.4"//,
  // ivy"com.geirsson::scalafmt-core::1.6.0-RC1"
)

trait CommonModule extends ScalaModule with ScalafmtModule with MetalsModule {
  def scalaVersion= scalaV
  override def ivyDeps = Agg(commonLibs: _*)
  def version = "0.1-SNAPSHOT"

  // def publishVersion = "0.1-SNAPSHOT"

  def organization = "in.ac.iisc.math"
  def name = "ProvingGround"

  // def pomSettings = PomSettings(
  //     description = "Automated theorem proving through learning in HoTT",
  //     organization = "in.ac.iisc.math",
  //     url = "https://github.com/siddhartha-gadgil/ProvingGround",
  //     licenses = Seq(License.MIT),
  //     versionControl = VersionControl.github("siddhartha-gadgil", "ProvingGround"),
  //     developers = Seq()
  //   )

  // override def scalacPluginIvyDeps = Agg(ivy"org.scalameta:::semanticdb-scalac:4.0.0")

  override def scalacOptions =
    Seq("-Ypartial-unification",
      "-Yrangepos",
      // "-Xplugin-require:semanticdb",
      "-unchecked",
      "-deprecation",
      "-feature",
      "-language:existentials")

  def scalaDocOptions =
    T{
      scalacOptions() ++
      Seq(
        "-diagrams",
        "-implicits",
        "-implicits-show-all"
      )
    }

  def bin() : define.Command[PathRef] = T.command {
    def ass: PathRef = assembly()
    def name: String = artifactName()
    cp.over(ass.path, pwd/ "bin" / s"provinground-$name-SNAPSHOT")
    ass
  }
}

trait CommonJSModule extends CommonModule with ScalaJSModule{
  def scalaJSVersion = "0.6.25"
}

val jvmLibs = List(
  ivy"com.lihaoyi:::ammonite:$ammV",
  ivy"com.lihaoyi::cask:0.1.9",
  ivy"org.scalameta::scalameta:4.1.0",
  ivy"com.github.nscala-time::nscala-time:2.16.0",
  ivy"org.reactivemongo::reactivemongo:0.12.1",
  ivy"com.typesafe.akka::akka-actor:2.5.11",
  ivy"com.typesafe.akka::akka-slf4j:2.5.11",
  ivy"org.scalactic::scalactic:3.0.1",
  ivy"com.typesafe:config:1.3.0",
  ivy"com.typesafe.akka::akka-stream:2.5.11",
  ivy"com.typesafe.akka::akka-http:10.1.1",
  ivy"com.typesafe.akka::akka-http-spray-json:10.1.1",
  ivy"org.slf4j:slf4j-api:1.7.16",
  ivy"org.slf4j:slf4j-simple:1.7.16",
  ivy"com.github.scopt::scopt:3.5.0",
  ivy"com.atlassian.commonmark:commonmark:0.11.0",
  ivy"org.apache.logging.log4j:log4j-core:2.11.1",
  ivy"org.platanios::tensorflow:0.4.0;classifier=linux-cpu-x86_64",
  ivy"org.scalameta::mdoc:1.2.8"
)



trait JvmModule extends CommonModule {
  override def ivyDeps =
    T{
      super.ivyDeps() ++ Agg(jvmLibs: _*)
    }
}

trait PGPublish extends PublishModule{
  def publishVersion = "0.1-SNAPSHOT"

    def pomSettings = PomSettings(
      description = "Automated theorem proving through learning in HoTT",
      organization = "in.ac.iisc.math",
      url = "https://github.com/siddhartha-gadgil/ProvingGround",
      licenses = Seq(License.MIT),
      versionControl = VersionControl.github("siddhartha-gadgil", "ProvingGround"),
      developers = Seq()
    )
}

object core extends Module{


  object jvm extends CommonModule with SbtModule with PGPublish{

    override def millSourcePath = super.millSourcePath / up
    def name = "ProvingGround-Core"




  }

  object js extends CommonJSModule with SbtModule{
    // def scalaVersion = "2.12.4"
    override def scalaJSVersion = "0.6.25"
    override def millSourcePath = super.millSourcePath / up
    // def ivyDeps = Agg(commonLibs: _*)
  }
}

object trepplein extends SbtModule with PublishModule{
  // def millsourcePath = pwd / 'trepplein
  def scalaVersion = scalaV
  override def ivyDeps =
    Agg(
      ivy"com.github.scopt::scopt:3.7.0"
    )

    def name = "trepplein"

    def publishVersion = "1.0"

  def pomSettings = PomSettings(
      description = "Independent type-checker for the dependently typed theorem prover Lean",
      organization = "trepplein",
      url = "https://github.com/gebner/trepplein",
      licenses = Seq(License.`Apache-2.0`),
      versionControl = VersionControl.github("gebner", "trepplein"),
      developers = Seq()
    )
}

object mantle extends SbtModule with JvmModule with PGPublish{
  override def moduleDeps = Seq(core.jvm, trepplein, leanlib.jvm, server)

  override def mainClass = Some("provingground.interface.MantleCask")

  object test extends Tests{
    override def ivyDeps = Agg(ivy"org.scalatest::scalatest:3.0.4")
    def testFrameworks = Seq("org.scalatest.tools.Framework")
  }

}


object leanlib extends Module{
  object jvm extends CommonModule with SbtModule with PGPublish{
    override def millSourcePath = super.millSourcePath / up
    override def moduleDeps = Seq(core.jvm)

    def name = "ProvingGround-Lean-Library"
  }

  object js extends CommonJSModule with SbtModule{
    override def millSourcePath = super.millSourcePath / up
    override def moduleDeps = Seq(core.js)
  }
}

object nlp extends SbtModule with ServerModule{
  override def moduleDeps = Seq(core.jvm, mantle)

  override def ivyDeps = T{
    super.ivyDeps() ++  Agg(
      ivy"edu.stanford.nlp:stanford-corenlp:3.7.0",
      ivy"edu.stanford.nlp:stanford-corenlp:3.7.0;classifier=models",
      ivy"com.google.protobuf:protobuf-java:2.6.1",
      ivy"edu.mit:jwi:2.2.3"
    )
  }

  override def mainClass = Some("provingground.interface.ParserCask")
}

object jvmRoot extends CommonModule{
  val projects = Seq(core.jvm, leanlib.jvm, mantle, nlp, server)

  override def sources = T.sources{
    core.jvm.sources() ++ leanlib.jvm.sources() ++ mantle.sources() ++ nlp.sources() ++ andrewscurtis.sources() ++ server.sources()
  }

  override def ivyDeps = T{
    core.jvm.ivyDeps() ++ mantle.ivyDeps() ++ nlp.ivyDeps()
  }

  override def moduleDeps = Seq(trepplein)

  def docs() = T.command{
    def jar = docJar()
    cp.over(jar.path / up / "javadoc", pwd / "docs" / "scaladoc")
    jar
  }
}

object exploring extends JvmModule{
  override def moduleDeps = Seq(core.jvm, mantle)
}

object realfunctions extends JvmModule

object andrewscurtis extends JvmModule with SbtModule{
  override def moduleDeps = Seq(core.jvm, mantle)

  object test extends Tests{
    override def ivyDeps = Agg(ivy"org.scalatest::scalatest:3.0.4")
    def testFrameworks = Seq("org.scalatest.tools.Framework")
  }
}

object normalform extends CommonModule with SbtModule

object client extends CommonJSModule with SbtModule{

  override def moduleDeps : Seq[ScalaJSModule] = Seq(core.js, leanlib.js)

    def platformSegment = "js"

    import coursier.maven.MavenRepository

  override def repositories = super.repositories ++ Seq(
      MavenRepository("http://amateras.sourceforge.jp/mvn/")
    )

  override def ivyDeps = Agg(
    ivy"org.scala-js::scalajs-dom::0.9.2",
    ivy"com.lihaoyi::scalatags::0.6.7",
    ivy"com.scalawarrior::scalajs-ace::0.0.4"
  )

  def pack(): define.Command[PathRef] = T.command {
    def js = fastOpt()
    cp.over(js.path, pwd/ "docs" / "js" / "provingground.js")
    js
  }

}

trait ServerModule extends JvmModule{
     override def resources: Sources = T.sources {
       def base: Seq[Path] = super.resources().map(_.path)

       def jsout = client.fastOpt().path / up

       cp.over(jsout / "out.js", jsout / "provingground-js-fastopt.js")

       (base ++ Seq(jsout, pwd / "docs")).map(PathRef(_))
     }
}

object server extends SbtModule with ServerModule with PGPublish {
  override def moduleDeps = Seq(core.jvm)

  override def mainClass = Some("provingground.interface.ScriptServer")

  def name = "ProvingGround-Server"
}

object experiments extends CommonModule{
  override def ivyDeps =
    super.ivyDeps() ++ Agg(
      ivy"org.platanios::tensorflow:0.2.2;classifier=linux-cpu-x86_64"
    )
}

object deepwalk extends JvmModule{
  override def ivyDeps =
    super.ivyDeps() ++ Agg(
      ivy"org.deeplearning4j:deeplearning4j-core:1.0.0-beta",
      ivy"org.deeplearning4j:deeplearning4j-nlp:1.0.0-beta",
      ivy"org.deeplearning4j:deeplearning4j-graph:1.0.0-beta",
      ivy"org.nd4j:nd4j-native-platform:1.0.0-beta"
    )
}
