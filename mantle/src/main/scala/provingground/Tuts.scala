package provingground.interface

import Amm._, ammonite.ops._

object Tuts {
  implicit val wd: Path = pwd

  def tutdir : Path = pwd / 'mantle / 'src / 'main / 'tut

  def gitHash: String = %%("git", "rev-parse", "HEAD").out.lines.head

  def gitBranch: String = %%("git", "symbolic-ref", "--short", "HEAD").out.lines.head

  lazy  val gitrep: String =
  s"""
     |
     |#### git commit hash when running tutorial: $gitHash
     |
 """.stripMargin


  def mkTut(f: String): String = {
    val top =
      """
        |import ammonite.ops._
        |interp.load.cp(pwd / 'out/'mantle/'assembly/'dest/"out.jar")
        |repl.pprinter.bind(provingground.translation.FansiShow.simplePrint)
        |repl.prompt() = "scala> "
      """.stripMargin

    val spl = f.split("```tut").map(_.split("```").toVector).toVector

    val tutcode: String =
      spl.tail
        .map(_(0))
        .mkString(top, "// tutEnd", "")

    pprint.log(tutcode)

    val output = replResult(tutcode)

    pprint.log(output)

    val tutChunks =
      output
        .split("repl.prompt\\(\\) = \"scala> \"")(1)
        .split("""// tutEnd""")
        .map((s) => s"""```scala
           |${s.trim.dropRight(6).trim}
           |```
           |
       |
     """.stripMargin)
        .toVector

    pprint.log(tutChunks.size)

    val tailTextChunks: Vector[String] =
      spl.tail.map((v) => v.applyOrElse[Int, String](1, (_) => ""))

    val textTail: Vector[String] =
      tutChunks
        .zip(tailTextChunks)
        .flatMap { case (a, b) => Vector(a, b) }

    val allChunks: Vector[String] = spl.head.head +: textTail

   
    allChunks.mkString("", "\n", gitrep)
  }

  def outdir: Path = pwd / "docs" / "tuts"

}
