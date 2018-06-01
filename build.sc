import mill._
import scalalib._
import scalajslib._
import define.{Sources, Task}
import ammonite.ops._
import coursier.maven.MavenRepository

val scalaV = "2.12.4"

val ammV = "1.1.0-12-f07633d"


val commonLibs = List(
  ivy"org.scala-lang.modules::scala-parser-combinators::1.0.5",
  ivy"org.scala-lang.modules::scala-xml:1.1.0",
  ivy"org.typelevel::spire::0.15.0",
  ivy"com.lihaoyi::fansi::0.2.4",
  ivy"com.lihaoyi::upickle::0.6.6",
  ivy"com.lihaoyi::fastparse::1.0.0",
  ivy"com.chuusai::shapeless::2.3.3",
  ivy"org.typelevel::cats-core::1.1.0",
  ivy"io.monix::monix::3.0.0-RC1",
  ivy"com.geirsson::scalafmt-core::1.4.0",
  ivy"com.lihaoyi::pprint::0.5.2",
  ivy"com.lihaoyi::sourcecode::0.1.4"
)

trait CommonModule extends ScalaModule{
  def scalaVersion= scalaV
  override def ivyDeps = Agg(commonLibs: _*)
  def version = "0.1-SNAPSHOT"
  def organization = "in.ac.iisc"
  def name = "ProvingGround"

  override def scalacOptions =
    Seq("-Ypartial-unification",
      "-unchecked",
      "-deprecation",
      "-feature",
      "-language:existentials")
}

trait CommonJSModule extends CommonModule with ScalaJSModule{
  def scalaJSVersion = "0.6.22"
}

val jvmLibs = List(
  ivy"com.lihaoyi:::ammonite:1.1.1",
  ivy"org.scalameta::scalameta:3.7.3",
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
  ivy"com.github.scopt::scopt:3.5.0"
)



trait JvmModule extends CommonModule {
  override def ivyDeps =
    T{
      super.ivyDeps() ++ Agg(jvmLibs: _*)
    }
}

object core extends Module{


  object jvm extends CommonModule with SbtModule{
    // def scalaVersion = "2.12.4"
    override def millSourcePath = super.millSourcePath / up
    // def ivyDeps = Agg(commonLibs: _*)
  }

  object js extends CommonJSModule with SbtModule{
    // def scalaVersion = "2.12.4"
    override def scalaJSVersion = "0.6.22"
    override def millSourcePath = super.millSourcePath / up
    // def ivyDeps = Agg(commonLibs: _*)
  }
}

object trepplein extends SbtModule{
  // def millsourcePath = pwd / 'trepplein
  def scalaVersion = scalaV
  override def ivyDeps =
    Agg(
      ivy"com.github.scopt::scopt:3.7.0"
    )
}

object mantle extends SbtModule with JvmModule{
  override def moduleDeps = Seq(core.jvm, trepplein, leanlib.jvm)


  object test extends Tests{
    override def ivyDeps = Agg(ivy"org.scalatest::scalatest:3.0.4")
    def testFrameworks = Seq("org.scalatest.tools.Framework")
  }

}


object leanlib extends Module{
  object jvm extends CommonModule with SbtModule{
    override def millSourcePath = super.millSourcePath / up
    override def moduleDeps = Seq(core.jvm)
  }

  object js extends CommonJSModule with SbtModule{
    override def millSourcePath = super.millSourcePath / up
    override def moduleDeps = Seq(core.js)
  }
}

object nlp extends CommonModule with SbtModule{
  override def moduleDeps = Seq(core.jvm)

  override def ivyDeps = T{
    super.ivyDeps() ++  Agg(
      ivy"com.lihaoyi:::ammonite:1.1.1",
      ivy"edu.stanford.nlp:stanford-corenlp:3.7.0",
      ivy"edu.stanford.nlp:stanford-corenlp:3.7.0;classifier=models",
      ivy"com.google.protobuf:protobuf-java:2.6.1"
    )
  }
}

object jvmRoot extends CommonModule{
  val projects = Seq(core.jvm, leanlib.jvm, mantle, nlp)

  override def sources = T.sources{
    core.jvm.sources() ++ leanlib.jvm.sources() ++ mantle.sources() ++ nlp.sources() ++ andrewscurtis.sources()
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
    ivy"org.scala-js::scalajs-dom::0.9.4",
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

object server extends SbtModule with ServerModule {
  override def moduleDeps = Seq(core.jvm)
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
