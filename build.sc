import mill._, scalalib._, scalajslib._, define.Task
import ammonite.ops._

val scalaV = "2.12.4"

val ammV = "1.1.0-12-f07633d"

trait CommonModule extends ScalaModule{
  def scalaVersion= scalaV
  def ivyDeps = Agg(commonLibs: _*)
  def version = "0.1-SNAPSHOT"
  def organization = "in.ac.iisc"
  def name = "ProvingGround"

  def scalacOptions =
    Seq("-Ypartial-unification",
      "-unchecked",
      "-deprecation",
      "-feature",
      "-language:existentials")
}

trait JvmModule extends CommonModule {
  def ivyDeps =
    T{
      super.ivyDeps() ++ Agg(jvmLibs: _*)
    }
}


val commonLibs = List(
  ivy"org.scala-lang.modules::scala-parser-combinators::1.0.5",
  ivy"org.scala-lang.modules::scala-xml:1.1.0",
  ivy"org.typelevel::spire::0.15.0",
  ivy"com.lihaoyi::fansi::0.2.4",
  ivy"com.lihaoyi::upickle::0.6.6",
  ivy"com.chuusai::shapeless::2.3.3",
  ivy"org.typelevel::cats-core::1.1.0",
  ivy"io.monix::monix::3.0.0-RC1",
  ivy"org.scalameta::scalameta::3.4.0",
  ivy"com.geirsson::scalafmt-core::1.4.0",
  ivy"com.lihaoyi::pprint::0.5.2",
  ivy"com.lihaoyi::sourcecode::0.1.4"
)

val jvmLibs = List(
  ivy"com.lihaoyi:::ammonite:1.1.1",
  ivy"com.github.nscala-time::nscala-time:2.16.0",
  ivy"org.reactivemongo::reactivemongo:0.12.1",
  ivy"com.typesafe.akka::akka-actor:2.4.17",
  ivy"com.typesafe.akka::akka-slf4j:2.4.17",
  ivy"org.scalactic::scalactic:3.0.1",
  ivy"com.typesafe:config:1.3.0",
  ivy"com.typesafe.akka::akka-stream:2.4.17",
  ivy"com.typesafe.akka::akka-http:10.0.5",
  ivy"com.typesafe.akka::akka-http-spray-json:10.0.5",
  ivy"org.slf4j:slf4j-api:1.7.16",
  ivy"org.slf4j:slf4j-simple:1.7.16"//,
  // ivy"org.scalanlp::breeze:0.13.2"
)

object core extends Module{


  object jvm extends CommonModule with SbtModule{
    // def scalaVersion = "2.12.4"
    def millSourcePath = super.millSourcePath / up
    // def ivyDeps = Agg(commonLibs: _*)
  }

  object js extends ScalaJSModule with CommonModule with SbtModule{
    // def scalaVersion = "2.12.4"
    def scalaJSVersion = "0.6.22"
    def millSourcePath = super.millSourcePath / up
    // def ivyDeps = Agg(commonLibs: _*)
  }
}

object trepplein extends SbtModule{
  // def millsourcePath = pwd / 'trepplein
  def scalaVersion = scalaV
  def ivyDeps =
    Agg(
      ivy"com.github.scopt::scopt:3.7.0"
    )
}

object mantle extends SbtModule with JvmModule{
  def moduleDeps = Seq(core.jvm, trepplein, leanlib)


  object test extends Tests{
    def ivyDeps = Agg(ivy"org.scalatest::scalatest:3.0.4")
    def testFrameworks = Seq("org.scalatest.tools.Framework")
  }

}


object leanlib extends JvmModule{
  def moduleDeps = Seq(core.jvm, trepplein)
}

object nlp extends CommonModule with SbtModule{
  def moduleDeps = Seq(core.jvm)

  def ivyDeps = T{
    super.ivyDeps() ++  Agg(
      ivy"com.lihaoyi:::ammonite:1.1.1",
      ivy"edu.stanford.nlp:stanford-corenlp:3.7.0",
      ivy"edu.stanford.nlp:stanford-corenlp:3.7.0;classifier=models",
      ivy"com.google.protobuf:protobuf-java:2.6.1"
    )
  }
}

object jvmRoot extends CommonModule{
  val projects = Seq(core.jvm, leanlib, mantle, nlp)

  def sources = T.sources{
    core.jvm.sources() ++ leanlib.sources() ++ mantle.sources() ++ nlp.sources()
  }

  def ivyDeps = T{
    core.jvm.ivyDeps() ++ leanlib.ivyDeps() ++ mantle.ivyDeps() ++ nlp.ivyDeps()
  }

  def moduleDeps = Seq(trepplein)

  def docs = T.command(){
    def jar = docJar()
    cp.over(jar / up / "javadoc", pwd / "docs" / "unidoc")
    jar
  }
}

// object server extends ScalaModule{
//   def scalaVersion = "2.12.4"
//
//   def moduleDeps = Seq(shared.jvm)
//
//   def ivyDeps = Agg(
//     ivy"com.typesafe.akka::akka-http:10.1.0-RC2",
//     ivy"com.typesafe.akka::akka-stream:2.5.9"
//   )
//
//   def resources = T.sources {
//     def base : Seq[Path] = super.resources().map(_.path)
//     def jsout = client.fastOpt().path / up
//     (base ++ Seq(jsout)).map(PathRef(_))
//   }
// }
//
// object client extends ScalaJSModule {
//   def scalaVersion = "2.12.4"
//   def scalaJSVersion = "0.6.22"
//   def moduleDeps : Seq[ScalaJSModule] = Seq(shared.js)
//
//   def platformSegment = "js"
//
//   import coursier.maven.MavenRepository
//
//   def repositories = super.repositories ++ Seq(
//     MavenRepository("https://oss.sonatype.org/content/repositories/releases")
//   )
//
//   def ivyDeps = Agg(
//     ivy"org.scala-js::scalajs-dom::0.9.4"
//   )
//
// }
