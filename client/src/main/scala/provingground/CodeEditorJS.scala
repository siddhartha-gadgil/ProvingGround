package provingground

//import scala.scalajs.js
import org.scalajs.dom
//import dom.html
import scalajs.js.annotation.JSExport
import scalatags.JsDom.all._

import scala.scalajs.js
import org.scalajs.dom

import com.scalawarrior.scalajs.ace._

import dom.ext._
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue


import HoTT.{id => _, _}

object CodeEditorJS  extends js.JSApp  {

  def main() = {
    val editDiv = dom.document.getElementById("edit-div").asInstanceOf[org.scalajs.dom.html.Div]

    val editButton = input(`type`:= "button", value:= "compile (ctrl-B)").render

    val errDiv = div().render

    val ed = div(id:= "editor", `class` := "editor")

    editDiv.appendChild(div(ed, editButton, div("Errors:", errDiv)).render)

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
      if (!prev.endsWith("\n")) editor.insert("\n")
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

    def showAnswer(result: Option[Either[String, String]]) =
      result.foreach{(lr) =>
        lr match {
          case Right(resp) =>
            editorAppend(s"//result: $resp\n\n")
            errDiv.innerHTML = ""
          case Left(err) =>
            errDiv.innerHTML = ""
            errDiv.appendChild( pre(div(`class`:= "text-danger")(err)).render)
        }
      }

      def compile() = {
        val code = editor.getValue()
          Ajax.post("/kernel", code).foreach{(xhr) => {
              val answer = xhr.responseText
              // results.appendChild(p(answer).render)
              // val answerLines = parseAnswer(answer).toString.replace("\n", "\n// ")
              // editorAppend(s"// $answerLines \n\n")
              showAnswer(parseAnswer(answer))
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
