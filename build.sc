import $ivy.`com.goyeau::mill-scalafix:0.2.1`
import com.goyeau.mill.scalafix.ScalafixModule
import mill._
import scalalib._, publish._
import scalajslib._
import mill.scalalib.scalafmt._
import define.{Sources, Task}
// import ammonite.ops._
import os._
import $ivy.`org.eclipse.jgit:org.eclipse.jgit:5.10.0.202012080955-r`

val scalaV = "2.13.3"

val ammV = "2.3.8"

val commonLibs = List(
  ivy"org.scala-lang.modules::scala-parser-combinators::1.1.2",
  ivy"org.scala-lang.modules::scala-xml:1.3.0",
  ivy"org.typelevel::spire::0.17.0",
  ivy"com.lihaoyi::fansi::0.2.9",
  ivy"com.lihaoyi::upickle::1.2.2",
  ivy"com.lihaoyi::fastparse::2.3.0",
  ivy"com.chuusai::shapeless::2.4.0-M1",
  ivy"org.typelevel::cats-core::2.3.1",
  ivy"io.monix::monix::3.3.0",
  ivy"com.lihaoyi::pprint::0.6.0",
  ivy"com.outr::scribe::3.1.9",
  ivy"org.scala-lang:scala-reflect:2.13.4",
  ivy"org.scala-lang.modules::scala-parallel-collections:1.0.0",
  ivy"com.lihaoyi::sourcecode::0.2.1" //,
  // ivy"com.geirsson::scalafmt-core::1.6.0-RC1"
)

trait CommonModule extends ScalaModule with ScalafmtModule with ScalafixModule {
  def scalaVersion     = scalaV
  override def ivyDeps = Agg(commonLibs: _*)
  def version          = "0.1.2-SNAPSHOT"

  def organization = "io.github.siddhartha-gadgil"
  def name         = "ProvingGround"

  override def artifactName = T { "provingground-" + super.artifactName() }

  override def scalacOptions =
    Seq(
      "-Yrangepos",
      // "-Ywarn-unused:imports",
      "-unchecked",
      "-deprecation",
      "-feature",
      "-language:existentials"
    )

  def scalaDocOptions =
    T {
      scalacOptions() ++
        Seq(
          "-diagrams",
          "-implicits",
          "-implicits-show-all"
        )
    }

  def bin(): define.Command[PathRef] = T.command {
    def ass: PathRef = assembly()
    def name: String = artifactName()
    os.copy
      .over(ass.path, os.pwd / "bin" / s"$name.fat.jar", createFolders = true)
    val hashName = s"$name-${glog.abbreviate(10).name}.fat.jar"
    os.copy
      .over(ass.path, os.pwd / "notes" / "bin" / hashName, createFolders = true)
    println("Include in a Jupyter (almond) notebook:")
    println(s"""
import ${"$"}cp.bin.`$hashName`
import provingground._ , interface._, HoTT._, learning._ 
repl.pprinter() = {
  val p = repl.pprinter()
  p.copy(
    additionalHandlers = p.additionalHandlers.orElse {
      translation.FansiShow.fansiHandler
    }
  )
}
""")
    ass
  }

  def slimbin(): define.Command[PathRef] = T.command {
    def ass: PathRef = jar()
    def name: String = artifactName()
    os.copy.over(ass.path, os.pwd / "bin" / s"$name.slim.jar")
    ass
  }

  import coursier.maven.MavenRepository

  def repositories = super.repositories ++ Seq(
    MavenRepository("https://oss.sonatype.org/content/repositories/snapshots")
  )
}

trait CommonJSModule extends CommonModule with ScalaJSModule {
  def scalaJSVersion = "1.5.0"
}

val jvmLibs = List(
  ivy"com.lihaoyi:::ammonite:$ammV",
  ivy"com.lihaoyi::cask:0.7.8",
  ivy"org.scalameta::scalameta:4.4.5",
  ivy"com.github.nscala-time::nscala-time:2.22.0",
  ivy"org.reactivemongo::reactivemongo:0.20.2",
  ivy"com.typesafe.akka::akka-actor:2.6.1",
  ivy"com.typesafe.akka::akka-slf4j:2.6.1",
  ivy"org.scalactic::scalactic:3.1.0",
  ivy"com.typesafe:config:1.3.0",
  ivy"com.typesafe.akka::akka-stream:2.6.1",
  ivy"com.typesafe.akka::akka-http:10.1.11",
  ivy"com.typesafe.akka::akka-http-spray-json:10.1.11",
  ivy"org.slf4j:slf4j-api:1.7.30",
  ivy"org.slf4j:slf4j-simple:1.7.30",
  ivy"com.github.scopt::scopt:4.0.0",
  ivy"com.atlassian.commonmark:commonmark:0.13.1",
  ivy"org.apache.logging.log4j:log4j-core:2.13.0",
  ivy"org.platanios::tensorflow:0.5.1-SNAPSHOT;classifier=linux-cpu-x86_64",
  ivy"org.scalameta::mdoc:2.2.14",
  ivy"org.eclipse.jgit:org.eclipse.jgit:5.10.0.202012080955-r",
  ivy"org.tensorflow:tensorflow-core-platform:0.2.0",
  ivy"org.tensorflow:tensorflow-framework:0.2.0"
)

trait JvmModule extends CommonModule {
  override def ivyDeps =
    T {
      super.ivyDeps() ++ Agg(jvmLibs: _*)
    }
}

trait PGPublish extends PublishModule {
  def publishVersion = "0.1.2-SNAPSHOT"

  def pomSettings = PomSettings(
    description = "Automated theorem proving through learning in HoTT",
    organization = "io.github.siddhartha-gadgil",
    url = "https://github.com/siddhartha-gadgil/ProvingGround",
    licenses = Seq(License.MIT),
    versionControl = VersionControl.github("siddhartha-gadgil", "ProvingGround"),
    developers = Seq(
      Developer(
        "siddhartha-gadgil",
        "Siddhartha Gadgil",
        "https://github.com/siddhartha-gadgil"
      )
    )
  )
}

object core extends Module {

  object jvm extends CommonModule with SbtModule with PGPublish {

    override def millSourcePath = super.millSourcePath / up
    def name                    = "ProvingGround-Core"

    // def artifactName = "provingground-core-jvm"

  }

  object js extends CommonJSModule with SbtModule {
    override def scalaJSVersion = "1.5.0"
    override def millSourcePath = super.millSourcePath / up
    // def ivyDeps = Agg(commonLibs: _*)
  }
}

object trepplein extends SbtModule with PublishModule {
  // def millsourcePath = os.pwd / 'trepplein
  def scalaVersion = scalaV
  override def ivyDeps =
    Agg(
      ivy"com.github.scopt::scopt:4.0.0-RC2"
    )

  def name = "trepplein"

  def publishVersion = "1.0"

  def pomSettings = PomSettings(
    description =
      "Independent type-checker for the dependently typed theorem prover Lean",
    organization = "trepplein",
    url = "https://github.com/gebner/trepplein",
    licenses = Seq(License.`Apache-2.0`),
    versionControl = VersionControl.github("gebner", "trepplein"),
    developers = Seq()
  )
}

val mantleLibs = List(
  ivy"com.lihaoyi::cask:0.7.3",
  ivy"org.scalameta::scalameta:4.3.0",
  ivy"com.atlassian.commonmark:commonmark:0.11.0",
  ivy"org.apache.logging.log4j:log4j-core:2.13.0",
  ivy"org.platanios::tensorflow:0.5.1-SNAPSHOT;classifier=linux-cpu-x86_64",
  ivy"org.scalameta::mdoc:2.2.13",
  ivy"com.lihaoyi::os-lib:0.7.0",
  ivy"org.eclipse.jgit:org.eclipse.jgit:5.10.0.202012080955-r",
  ivy"org.deeplearning4j:deeplearning4j-core:1.0.0-beta7",
  ivy"org.deeplearning4j:deeplearning4j-nlp:1.0.0-beta7",
  ivy"org.deeplearning4j:deeplearning4j-graph:1.0.0-beta7",
  // ivy"org.nd4j:nd4j-native-platform:1.0.0-beta7",
  ivy"org.mongodb.scala::mongo-scala-driver:2.8.0",
  ivy"org.tensorflow:tensorflow-core-platform:0.2.0",
  ivy"org.tensorflow:tensorflow-framework:0.2.0"
)

def glog = {
  import java.io._
  import org.eclipse.jgit._
  import storage.file._
  val builder = new FileRepositoryBuilder()
  val repo    = builder.findGitDir(new File(".")).readEnvironment().build()
  val git     = new api.Git(repo)
  import scala.jdk.CollectionConverters._
  git.log().call().asScala.head
}

def gitlog() = {
  os.write.over(
    os.pwd / "mantle" / "src" / "main" / "resources" / "gitlog.txt",
    glog.name + "\n"
  )
}

object mantle extends CommonModule with SbtModule with PGPublish {
  override def moduleDeps = Seq(core.jvm, jvmcore, trepplein, leanlib.jvm)

  // def artifactName = "provingground-mantle"

  override def ivyDeps =
    T {
      super.ivyDeps() ++ Agg(mantleLibs: _*)
    }

  override def unmanagedClasspath =
    T {
      import coursier._
      val files = Fetch()
        .addDependencies(dep"org.nd4j:nd4j-native-platform:1.0.0-beta7")
        .run()
      val pathRefs = files.map(f => PathRef(Path(f)))
      Agg(pathRefs: _*)
    }

  override def resources: Sources = T.sources {
    def base: Seq[Path] = super.resources().map(_.path)

    gitlog()

    def jsout = client.fastOpt().path / up

    os.copy.over(jsout / "out.js", jsout / "provingground-js-fastopt.js")

    (base ++ Seq(jsout, os.pwd / "docs")).map(PathRef(_))
  }

  override def mainClass = Some("provingground.interface.MantleCask")

  object test extends Tests {
    override def ivyDeps = Agg(ivy"org.scalatest::scalatest:3.1.0")
    def testFrameworks   = Seq("org.scalatest.tools.Framework")
  }

}

object crust extends SbtModule with JvmModule with PGPublish {
  override def moduleDeps =
    Seq(core.jvm, jvmcore, trepplein, leanlib.jvm, server, mantle)

  // def artifactName = "provingground-crust"
}

object leanlib extends Module {
  object jvm extends CommonModule with SbtModule with PGPublish {
    override def millSourcePath = super.millSourcePath / up
    override def moduleDeps     = Seq(core.jvm)

    def name = "ProvingGround-Lean-Library"
  }

  object js extends CommonJSModule with SbtModule {
    override def millSourcePath = super.millSourcePath / up
    override def moduleDeps     = Seq(core.js)
  }
}

object nlp extends SbtModule with ServerModule with PGPublish {
  override def moduleDeps = Seq(core.jvm, jvmcore, mantle, crust)

  override def ivyDeps = T {
    super.ivyDeps() ++ Agg(
      ivy"edu.stanford.nlp:stanford-corenlp:3.7.0",
      ivy"edu.stanford.nlp:stanford-corenlp:3.7.0;classifier=models",
      ivy"com.google.protobuf:protobuf-java:2.6.1",
      ivy"edu.mit:jwi:2.2.3"
    )

  }

  // def artifactName = "provingground-nlp"

  override def mainClass = Some("provingground.interface.ParserCask")
}

object jvmRoot extends CommonModule {
  val projects = Seq(core.jvm, jvmcore, leanlib.jvm, mantle, nlp, server, crust)

  override def sources = T.sources {
    core.jvm.sources() ++ jvmcore.sources() ++ leanlib.jvm.sources() ++ mantle
      .sources() ++ nlp
      .sources() ++ andrewscurtis.sources() ++ server.sources() ++ crust
      .sources()
  }

  override def ivyDeps = T {
    core.jvm.ivyDeps() ++ mantle.ivyDeps() ++ nlp.ivyDeps()
  }

  override def moduleDeps = Seq(trepplein)

  def docs() = T.command {
    def jar = docJar()
    os.copy.over(jar.path / up / "javadoc", os.pwd / "docs" / "scaladoc")
    jar
  }
}

object jvmcore extends JvmModule with SbtModule with PGPublish {
  def moduleDeps = Seq(core.jvm)

  def name = "ProvingGround--JVM-Core"
}
object exploring extends JvmModule {
  override def moduleDeps = Seq(core.jvm, jvmcore, mantle, crust)
}

object realfunctions extends JvmModule

object andrewscurtis extends JvmModule with SbtModule {
  override def moduleDeps = Seq(core.jvm, mantle, crust)

  object test extends Tests {
    override def ivyDeps = Agg(ivy"org.scalatest::scalatest:3.1.0")
    def testFrameworks   = Seq("org.scalatest.tools.Framework")
  }
}

object normalform extends CommonModule with SbtModule

object client extends CommonJSModule with SbtModule {

  override def moduleDeps: Seq[ScalaJSModule] = Seq(core.js, leanlib.js)

  def platformSegment = "js"

  import coursier.maven.MavenRepository

  override def repositories = super.repositories ++ Seq(
    MavenRepository("http://amateras.sourceforge.jp/mvn/")
  )

  override def ivyDeps = Agg(
    ivy"org.scala-js::scalajs-dom::1.1.0",
    ivy"com.lihaoyi::scalatags::0.9.2"
    // ivy"com.scalawarrior::scalajs-ace::0.0.4"
  )

  def pack(): define.Command[PathRef] = T.command {
    def js = fastOpt()
    os.copy.over(js.path, os.pwd / "docs" / "js" / "provingground.js")
    js
  }

}

trait ServerModule extends JvmModule {
  override def resources: Sources = T.sources {
    def base: Seq[Path] = super.resources().map(_.path)

    def jsout = client.fastOpt().path / up

    os.copy.over(jsout / "out.js", jsout / "provingground-js-fastopt.js")

    (base ++ Seq(jsout, os.pwd / "docs")).map(PathRef(_))
  }
}

object server extends SbtModule with ServerModule with PGPublish {
  override def moduleDeps = Seq(core.jvm, jvmcore)

  override def mainClass = Some("provingground.interface.ScriptServer")

  def name = "ProvingGround-Server"

  // def artifactName = "provingground-server"
}

object experiments extends CommonModule {
  override def ivyDeps =
    super.ivyDeps() ++ Agg(
      ivy"org.platanios::tensorflow:0.5.1-SNAPSHOT;classifier=linux-cpu-x86_64"
    )
}

object deepwalk extends JvmModule {
  override def ivyDeps =
    super.ivyDeps() ++ Agg(
      ivy"org.deeplearning4j:deeplearning4j-core:1.0.0-beta7",
      ivy"org.deeplearning4j:deeplearning4j-nlp:1.0.0-beta7",
      ivy"org.deeplearning4j:deeplearning4j-graph:1.0.0-beta7"
      // ivy"org.nd4j:nd4j-native-platform:1.0.0-beta7"
    )

  override def unmanagedClasspath =
    T {
      import coursier._
      val files = Fetch()
        .addDependencies(dep"org.nd4j:nd4j-native-platform:1.0.0-beta7")
        .run()
      val pathRefs = files.map(f => PathRef(Path(f)))
      Agg(pathRefs: _*)
    }
}
