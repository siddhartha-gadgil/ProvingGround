package provingground

//import scala.scalajs.js
import org.scalajs.dom
//import dom.html
import scalajs.js.annotation.JSExport
import scalatags.JsDom.all._

import scala.scalajs.js
import org.scalajs.dom

import com.scalawarrior.scalajs.ace._

import HoTT.{id => _, _}

object ScalaJSExample extends js.JSApp {
  def main(): Unit = {
    import org.scalajs.dom.document._
    dom.document.getElementById("scalajsShoutOut").textContent = HoTT.Type.toString

    import org.scalajs.dom.ext._

    val echo = span.render

    // import dom.ext._
    import scala.scalajs.concurrent.JSExecutionContext.Implicits.runNow

    val box = input(
      `type` := "text",
      placeholder := "Type here!"
    ).render

    val results = div().render

    val ticks = div("Ticks:").render

    val sse = new dom.EventSource("/events")

    import upickle.default._

    sse.onmessage = (event: dom.MessageEvent) => {
      ticks.appendChild(
        p(event.data.toString).render
      )
      ticks.appendChild(p("tick").render)
    }


    box.onchange = (e: dom.Event) => {
      echo.textContent = box.value
      Ajax.post("/kernel", box.value).foreach{(xhr) => {
          val answer = xhr.responseText
          results.appendChild(p(answer).render)
      }
    }

    }

    val target = dom.document.getElementById("jsdiv")

    val (animalA, animalB) = ("fox", "dog")


    target.appendChild(
      div(
        h1("Hello World!"),
        p(
          "The quick brown ",
          b(animalA),
          " jumps over the lazy ",
          i(animalB),
          "."
        ),
        box,
        echo,
        p("ammonite results"),
        results,
        ticks
  ).render
)

  // Ace editor code

  val editDiv = dom.document.getElementById("edit-div").asInstanceOf[org.scalajs.dom.html.Div]

  val editButton = input(`type`:= "button", value:= "compile (ctrl-B)").render

  val errDiv = div().render

  editDiv.appendChild(div(editButton, div("Errors:", errDiv)).render)

  val editor = ace.edit("editor")
  editor.setTheme("ace/theme/chrome")
  editor.getSession().setMode("ace/mode/scala")


  // val ed = dom.document.getElementById("editor").asInstanceOf[org.scalajs.dom.html.Div]

  val text = editor.getValue()
  val initCommands = "import provingground._\nimport HoTT._\nimport TLImplicits._\nimport shapeless._\n\n"
  editor.insert(initCommands)

  def editorAppend(text: String) = {
    val prev = editor.getValue
    val lines = prev.count(_ == '\n') + (if (prev.endsWith("\n")) 1 else 2)
    editor.gotoLine(lines)
    editor.insert(text)
    editor.gotoLine(lines+2)
  }




  def parseAnswer(text: String) : Option[Either[String, String]] =
    if (text == "None") None
    else {
      assert(text.startsWith("Some(") && text.endsWith(")"))
      val e = text.drop(5).dropRight(1)
      if (e.startsWith("Right(")) Some(Right(e.drop(6).dropRight(1)))
        else Some(Left(e.drop(5).dropRight(1)))
    }

    def compile() = {
      val code = editor.getValue()
      echo.textContent = code
        Ajax.post("/kernel", code).foreach{(xhr) => {
            val answer = xhr.responseText
            results.appendChild(p(answer).render)
            val answerLines = parseAnswer(answer).toString.replace("\n", "\n// ")
            editorAppend(s"// $answerLines \n\n")
        }
      }
    }


  editButton.onclick = (event: dom.Event) => compile()

  editDiv.onkeydown = (e) => {
    if (e.ctrlKey && e.keyCode == 66) compile()
  }

  def scriptListFut = Ajax.get("/list-scripts")

  def loadScriptFut(name: String) = Ajax.get(s"/script/$name")

  def saveScript(name: String, body: String) = Ajax.post(s"/save-script/$name", body)

  }
}

@JSExport
object JsTest {
  @JSExport
  def jstest(): Unit = {
    dom.document
      .getElementById("scalajs")
      .textContent = "Hello from Scala-js: " +
        Type

    val bouncers = div.render

    val echo = span.render

    import dom.ext._
    import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

    val box = input(
      `type` := "text",
      placeholder := "Type here!"
    ).render

    box.onchange = (e: dom.Event) => {
      echo.textContent = box.value
      Ajax.post("/bounce", box.value)
    }

    val sse = new dom.EventSource("/bouncestream")

    import upickle.default._

    sse.onmessage = (event: dom.MessageEvent) => {
      bouncers.appendChild(
        p(event.data.toString).render
      )
      val (a, b) = read[(String, String)](event.data.toString)
      bouncers.appendChild(i(a).render)
    }

    val target = dom.document.getElementById("jsdiv")

    val (animalA, animalB) = ("fox", "dog")

    target.appendChild(
      div(
        h1("Hello World!"),
        p(
          "The quick brown ",
          b(animalA),
          " jumps over the lazy ",
          i(animalB),
          "."
        ),
        box,
        echo,
        p("bouncers below"),
        bouncers
      ).render
    )
  }
}
