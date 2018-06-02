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

import HoTT.{id => _, _}, translation._

import scala.io.StdIn

@JSExportTopLevel("parser")
object ConstituencyParser{
  @JSExport
  def load() : Unit = {

  val runButton =
    input(`type` := "button",
      value := "Parse",
      `class` := "btn btn-success").render
  val treeDiv = div(`class` := "view")().render
  val exprDiv = div(`class` := "language-scala view")().render
  val logDiv = div()().render
  val parseInput =
    input(`type` := "text", `class` := "form-control").render

  def parse(txt: String) = {

    Ajax.post("/parse", txt).foreach { (xhr) =>
      {
        logDiv.appendChild(p("button clicked").render)
        val answer = xhr.responseText
        logDiv.appendChild(pre(answer).render)
        val js = json.read(answer)
        val tree = js.obj("tree").str.toString
        treeDiv.innerHTML = ""
        treeDiv.appendChild(pre(tree).render)
        val expr = js.obj("expr").str.toString
        exprDiv.innerHTML = ""
        exprDiv.appendChild(
          pre(
            code(`class` := "language-scala")(expr)
          ).render)
        g.hljs.highlightBlock(exprDiv)
        g.hljs.initHighlighting.called = false
        g.hljs.initHighlighting()
      }
    }
  } //parse

  runButton.onclick = (e: dom.Event) => parse(parseInput.value)

  val jsDiv =
    div(
      form(
        div(`class` := "form-group")(
      label("Sentence:"),
      parseInput),
      runButton),
      // logDiv,
      h4("Constituency parsed tree"),
      treeDiv,
      h4("Mathematical Expression"),
      exprDiv
    )

    val pdiv= dom.document.querySelector("#constituency-parser")
      // (pdiv) =>
        val parseDiv = pdiv.asInstanceOf[org.scalajs.dom.html.Div]

          parseDiv.appendChild(jsDiv.render)

      // } // option ma
    } // load
}
