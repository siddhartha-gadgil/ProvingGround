package provingground.interface

import java.nio.charset.StandardCharsets

object Amm {
  def replResult(code: String) = {
    import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
    val outputS = new ByteArrayOutputStream()
    val errLog  = new ByteArrayOutputStream()
    val infoLog = new ByteArrayOutputStream()
    val inpStream = new ByteArrayInputStream(
      (code + "\nexit\n").getBytes(StandardCharsets.UTF_8))

    pprint.log("ready to run ammonite")

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

    val silly = """\[[0-9]+[A-Zm]""".r

    val rawOutput = new String(outputS.toByteArray, "UTF-8")

    println(rawOutput.split("\u001b"))

    val output =
      silly.replaceAllIn(rawOutput.replace("\u001b", ""), "")

    val err = new String(errLog.toByteArray, "UTF-8")

    val info = new String(infoLog.toByteArray, "UTF-8")

    pprint.log(err)

    output
  }

}
