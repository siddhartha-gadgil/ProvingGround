package provingground


import fastparse._
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

import scala.util.{Failure, Success, Try}
import HoTT.{id => _, _}
import org.scalajs.dom.html.Span
import translation._
import scalatags.JsDom.all._

@JSGlobal
@js.native
object katex extends js.Object {
  def renderToString(texString: String): String = js.native
}

object katexSafe{
  def renderToString(texString: String): String = scala.util.Try(katex.renderToString(texString)).getOrElse(texString)



  def teXSpan(t: Term): Span = {
    val s = span().render
    s.innerHTML = renderToString(TeXTranslate(t))
    s
  }
}

object ScratchPad {
  def load(): Unit = {
    Option(dom.document.querySelector("#hott-scratch")).foreach { (ediv) =>
      val editDiv = ediv.asInstanceOf[org.scalajs.dom.html.Div]
      val runButton =
        input(`type` := "button",
              value := "Run (ctrl-B)",
              `class` := "btn btn-success").render

      val viewDiv = div(`class` := "view")().render

      val ed = div(id := "editor", `class` := "panel-body editor")

      editDiv.appendChild(
        div(
          div(`class` := "panel panel-primary")(
            div(`class` := "panel-heading")(h3("HoTT Scratchpad")),
            ed,
            div(`class` := "panel-footer")(runButton)),
          div(h3("Result:"), viewDiv)
        ).render
      )

      val editor = ace.edit("editor")
      editor.setTheme("ace/theme/chrome")
      editor.getSession().setMode("ace/mode/scala")

      val parser = HoTTParser()

      def compile(): Unit = {
        val text = editor.getValue

        val view =
          parse(text, parser.block(_)
        ).fold(
            (_, _, s) =>
              div(
                h3(`class` := "text-danger")("Error"),
                div(s.traced.trace)
            ),
            (bl, _) =>
              div(`class` := "lead")(
                h3(`class` := "text-success")("Success"),
                bl.valueOpt
                  .map { (t) =>
                    val termSpan = span().render
                    val typSpan  = span().render
                    termSpan.innerHTML = katex.renderToString(TeXTranslate(t))
                    typSpan.innerHTML =
                      katex.renderToString(TeXTranslate(t.typ))
                    div(p("Term: ", termSpan), p("Type: ", typSpan))
                  }
                  .getOrElse(div("Empty block"))
            )
          )
        // val view = h4(`class` := "text-primary")(parser.block.parse(text).toString)

        viewDiv.innerHTML = ""
        viewDiv.appendChild(view.render)
      }

      runButton.onclick = (event: dom.Event) => compile()

      editDiv.onkeydown = (e) => {
        if (e.ctrlKey && e.keyCode == 66) compile()
      }

    }
  }
}
