package provingground

//import scala.scalajs.js
import org.scalajs.dom
//import dom.html
import scalajs.js.annotation._
import scalatags.JsDom.all._

import scala.scalajs.js
import org.scalajs.dom

// import com.karasiq.highlightjs.HighlightJS

import js.Dynamic.{global => g}

import com.scalawarrior.scalajs.ace._

import dom.ext._
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

import ujson.Js

import scala.util.{Try, Success, Failure}

import HoTT.{id => _, _}
@JSExportTopLevel("CodeEditorJS")
object CodeEditorJS {

  @JSExport
  def main(): Unit = {
    val editDiv = dom.document
      .getElementById("edit-div")
      .asInstanceOf[org.scalajs.dom.html.Div]

    val runButton =
      input(`type` := "button",
            value := "Run (ctrl-B)",
            `class` := "btn btn-space btn-primary pull-right").render


    val logDiv = div(`class` := "panel-body").render

    val viewDiv = div(`class` := "panel-body view")().render

    val ed = div(id := "editor", `class` := "panel-body editor")

    editDiv.appendChild(
      div(
        div(`class` := "panel panel-primary")(
          div(`class` := "panel-heading")(h4("Script:")),
          ed,
          div(`class` := "panel-footer clearfix")(runButton)
        ),
        div(`class` := "panel panel-info")(
          div(`class` := "panel-heading")(h4("Logs:")),
          logDiv),
        div(`class` := "panel panel-default")(
          div(`class` := "panel-heading")(h4("Output:")),
          viewDiv)
      ).render)

    val editor = ace.edit("editor")
    editor.setTheme("ace/theme/chrome")
    editor.getSession().setMode("ace/mode/scala")


    val initCommands =
      "import provingground._\nimport HoTT._\nimport induction.TLImplicits._\nimport shapeless._\n\n"
    editor.insert(initCommands)

    def compile(): Unit = {
      val codetext = editor.getValue()
      runButton.value = "Running..."
      Ajax.post("./kernel", codetext).foreach { (xhr) =>
        {
          val answer = xhr.responseText
          runButton.value = "Run (ctrl-B)"
          logDiv.innerHTML = ""
          val view =
            if (answer.startsWith("--RESULT--\n"))
              answer.drop("--RESULT--\n".length)
            else answer.split("--OUTPUT--\n")(1)
          viewDiv.innerHTML = ""
          val codeDiv = code(`class` := "scala", view).render
          viewDiv.appendChild(pre(codeDiv).render)
          g.renderMathInElement(viewDiv, js.Dynamic.literal(ignoreTags = Seq()))
          if (answer.startsWith("--ERROR--\n")) {
            val err = answer
              .drop("--ERROR--\n".length)
              .split("--OUTPUT--\n")(0)
              .drop("--INFO--\n".length)
            logDiv.appendChild(pre(div(`class` := "text-danger")(err)).render)
          }
          g.hljs.highlightBlock(codeDiv)
        }
      }
    }

    runButton.onclick = (event: dom.Event) => compile()

    dom.window.onkeydown = (e) => {
      if (e.ctrlKey && e.keyCode == 66) compile()
    }



  }
}
