package provingground

import org.scalajs.dom
import scalajs.js.annotation._
import scalatags.JsDom.all._

import scala.scalajs.js
import org.scalajs.dom

import js.Dynamic.{global => g}

import com.scalawarrior.scalajs.ace._

import dom.ext._
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

import upickle.{Js, json}

import scala.util.{Try, Success, Failure}

import HoTT.{id => _, _}

@js.native
object katex extends js.Object {
  def renderToString(texString: String): String = js.native
}


object ScratchPad{
  def load() : Unit = {
    Option(dom.document.querySelector("#hott-scratch")).map{
      (ediv) =>
        val editDiv = ediv.asInstanceOf[org.scalajs.dom.html.Div]
        val runButton =
          input(`type` := "button",
            value := "Run (ctrl-B)",
            `class` := "btn btn-success").render

        val logDiv = div().render

        val viewDiv = div(`class` := "view bg-info")().render

        val ed = div(id := "editor", `class` := "panel-body editor")

        editDiv.appendChild(
          div(
            div(`class` := "panel panel-primary")(
              div(`class` := "panel-heading")(h3("HoTT Scratchpad")),
              ed,
              div(`class`:="panel-footer")(runButton)),
            div("Logs:", logDiv),
            div(
              h3("Result:"),
              viewDiv)
          ).render
        )

        val editor = ace.edit("editor")
        editor.setTheme("ace/theme/chrome")
        editor.getSession().setMode("ace/mode/scala")

        val parser = translation.HoTTParser()

        def compile(): Unit = {
          val text = editor.getValue
          val result = h4(`class` := "text-primary")(parser.block.parse(text).toString)

          viewDiv.innerHTML = ""
          viewDiv.appendChild(result.render)
        }

        runButton.onclick = (event: dom.Event) => compile()

        editDiv.onkeydown = (e) => {
          if (e.ctrlKey && e.keyCode == 66) compile()
        }


    }
  }
}
