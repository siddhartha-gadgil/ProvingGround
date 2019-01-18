package provingground.interface
import java.io.PrintStream

object AmmService {

  val initCommands =
    "import provingground._\nimport HoTT._\nimport induction.TLImplicits._\nimport shapeless._\n; repl.pprinter.bind(translation.FansiShow.simplePrint)"


  import java.nio.charset.StandardCharsets

  def replResult(code: String): Either[String, String] = {
    import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
    val outputS = new ByteArrayOutputStream()
    val errLog  = new ByteArrayOutputStream()
    val infoLog = new ByteArrayOutputStream()
    val inpStream = new ByteArrayInputStream(
      (code + "\nexit\n").getBytes(StandardCharsets.UTF_8))

    println(code)

    println("running ammonite")

    val ammMain =
      Console.withIn(inpStream) {
        Console.withErr(errLog) {
          Console.withOut(outputS) {
            ammonite.Main.main0(
              args =
                List("--predef-code",
                     "interp.colors() = ammonite.util.Colors.BlackWhite\n"),
              stdIn = inpStream,
              stdOut = outputS,
              stdErr = errLog
            )

          }
        }
      }

    pprint.log("ran ammonite")

    val silly = """\[[0-9]+[A-Z]""".r

    val output =
      silly.replaceAllIn(
        new String(outputS.toByteArray, "UTF-8").replace("\u001b", ""),
        "")

    val err = new String(errLog.toByteArray, "UTF-8")

    val info = new String(infoLog.toByteArray, "UTF-8")

    pprint.log(output)
    pprint.log(info)
    pprint.log(err)

    if (err == "") Right(output)
    else Left("--INFO--\n" + info + err + "--OUTPUT--\n" + output)
  }

  val indexHTML =
    """
      |<!DOCTYPE html>
      |
      |<html>
      |  <head>
      |    <title>ProvingGround Fiddle</title>
      |    <link rel="stylesheet" href="/resources/bootstrap.min.css">
      |    <link rel="icon" href="/resources/IIScLogo.jpg">
      |    <script src="/resources/src-min/ace.js" type="text/javascript" charset="utf-8"></script>
      |    <link rel="stylesheet" href="/resources/katex.min.css">
      |    <script src="/resources/katex.min.js" type="text/javascript" charset="utf-8"></script>
      |   <script src="/resources/auto-render.min.js" type="text/javascript" charset="utf-8"></script>
      |    <link rel="stylesheet" href="/resources/github-gist.css">
      |    <script src="/resources/highlight.pack.js" type="text/javascript" charset="utf-8"></script>
      |    <script src="/resources/provingground-js-fastopt.js" type="text/javascript" charset="utf-8"></script>
      |    <style type="text/css" media="screen">
      |        .editor {
      |            height: 300px;
      |            font-size: 14px;
      |
      |        }
      |        .view {
      |          overflow-y: auto;
      |          height: 300px;
      |        }
      |        .btn-space {
      |    margin-right: 5px;
      |}
      |    </style>
      |  </head>
      |  <body>
      |
      |  <div class="container">
      |    <h2 class="text-center"> ProvingGround Fiddle </h2>
      |    <p> Any commands entered below are run in an interpreter, with the code of
      |    the ProvingGround project, <em>excluding</em> the natural language processing component, in the class path.</p>
      |
      |
      |    <div id="edit-div"></div>
      |  </div>
      |
      |  <script>
      |    CodeEditorJS.main()
      |    </script>
      |
      |  </body>
      |</html>
      |
    """.stripMargin



}


object MDocService {
  import ammonite.ops._

  def replResult(code: String): Either[String, String] = {
    pprint.log(code)
    import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
    val outputS = new ByteArrayOutputStream()
    val ps = new PrintStream(outputS)
    val md = s"```scala mdoc\n$code\n```"
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
