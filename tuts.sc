import ammonite.ops._

println("Assembling")

implicit val wd = pwd
%%("mill", "mantle.assembly")

val tutdir = pwd / 'mantle / 'src / 'main / 'tut

val gitLog = %%("git", "log", "-1").out.lines

val gitBranch = %%("git", "symbolic-ref", "--short", "HEAD").out.lines.head

def mkTut(f: String): String = {

  val top =
  """
    |  repl.frontEnd() = ammonite.repl.AmmoniteFrontEnd()
    |  interp.colors() = ammonite.util.Colors.BlackWhite
    |  import ammonite.ops._
    |  interp.load.cp(pwd / 'out/'mantle/'assembly/'dest/"out.jar")
    |  repl.pprinter.bind(provingground.translation.FansiShow.simplePrint)
    |  repl.prompt() = "scala> "
  """.stripMargin

  val spl = f.split("```tut").map(_.split("```").toVector).toVector
  val tutcode =
    spl.tail.map(_(0))
    .mkString(top,"// tutEnd","")

  import java.io._

  val outputStream = new ByteArrayOutputStream()

  val errorStream = new ByteArrayOutputStream()

  val inp = new ByteArrayInputStream(tutcode.getBytes)

  val ammrun = ammonite.Main.main0(List(), inp, outputStream, errorStream)

  lazy val output = new String(outputStream.toByteArray, "UTF-8")

  lazy val errors = new String(errorStream.toByteArray, "UTF-8")

  val silly = """\[[0-9]+[A-Z]""".r

  val errs = silly.replaceAllIn(output
    .replace("\u001b", ""), "")

  println(errors)

  // println(errs)

  val tutChunks =
    silly.replaceAllIn(output
    .replace("\u001b", ""), "")
    .split("repl.prompt\\(\\) = \"scala> \"")(1)
    .split("""// tutEnd""").map((s) =>
    s"""```scala
       |${s.trim.dropRight(6).trim}
       |```
       |
       |
     """.stripMargin
    ).toVector

  val textTail = tutChunks.zip(spl.tail.map(_(1))).map{case (a, b) => Vector(a, b)}.flatten

  val allChunks = spl.head.head +: textTail


  println(tutChunks.size)

  val gitrep =
    s"""
       |#### Git Log when running tutorial
       |```
       |${gitLog.mkString("\n")}
       |```
       |
       |* __Branch__ : $gitBranch
       |
     """.stripMargin

  allChunks.mkString("", "\n", gitrep)
  }

val outdir = pwd / "docs" / "tuts"


def writeMD = ls(tutdir).foreach{(p) =>
  println(s"compiling tutorial ${p.name}")
  write.over(outdir / p.name, mkTut(read(p)))
}


