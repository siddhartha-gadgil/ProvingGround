import ammonite.ops._
val tutdir = pwd / 'mantle / 'src / 'main / 'tut

val f = read(tutdir / "HoTT.md")

val top =
"""
repl.frontEnd() = ammonite.repl.AmmoniteFrontEnd()
interp.colors() = ammonite.util.Colors.BlackWhite
import ammonite.ops._
interp.load.cp(pwd / 'out/'mantle/'assembly/'dest/"out.jar")
repl.pprinter.bind(provingground.translation.FansiShow.simplePrint)
repl.prompt() = "scala> "
"""

val spl = f.split("```tut").map(_.split("```").toVector).toVector
val tutcode =
  spl.tail.map(_(0))
  .mkString(top,"// tutEnd","")

import java.io._

val outputStream = new ByteArrayOutputStream()

val inp = new ByteArrayInputStream(tutcode.getBytes)

val ammrun = ammonite.Main.main0(List(), inp, outputStream, outputStream)

lazy val output = new String(outputStream.toByteArray, "UTF-8")

val silly = """\[[0-9]+[A-Z]""".r

val tutChunks =
  silly.replaceAllIn(output
  .replace("\u001b", ""), "")
  .split("repl.prompt\\(\\) = \"scala> \"")(1)
  .split("""// tutEnd""").map((s) =>
s"""```scala
${s.trim.dropRight(6).trim}
```
""").toVector

val textTail = tutChunks.zip(spl.tail.map(_(1))).map{case (a, b) => Vector(a, b)}.flatten

val allChunks = spl.head.head +: textTail


println(tutChunks.size)

println(spl.head)

write.over(pwd / "HoTTout.md", allChunks.mkString("","\n","\n"))
