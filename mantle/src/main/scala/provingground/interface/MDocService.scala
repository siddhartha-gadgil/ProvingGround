package provingground.interface
import java.io.PrintStream


object MDocService {
  import os._

  def replResult(code: String): Either[String, String] = {
    pprint.log(code)
    import java.io.ByteArrayOutputStream
    val outputS = new ByteArrayOutputStream()
    val ps = new PrintStream(outputS)
    val md = s"```scala mdoc:to-string\n$code\n```"
    write.over(pwd / "tmp" / "docs" / "fiddle.md", md)
    val td = java.nio.file.Paths.get("tmp/")
    pprint.log(td)
    val exitCode = mdoc.Main.process(Array[String](), ps, td)
    val output = new String(outputS.toByteArray, "UTF-8")
    pprint.log(output)
    def content = read.lines(pwd / "tmp" / "out" / "fiddle.md").drop(1).dropRight(2).mkString("\n")
    if (exitCode == 0) Right(content)
    else Left("--INFO--\n" + "mdoc failed to parse\n" + "--OUTPUT--\n" + output)
      }
}
