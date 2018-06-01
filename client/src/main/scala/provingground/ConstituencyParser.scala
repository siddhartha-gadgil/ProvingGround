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

object ConstituencyParser{
  def load() : Unit = {
    Option(dom.document.querySelector("#constituency-parser")).map{
      (pdiv) =>
        val parseDiv = pdiv.asInstanceOf[org.scalajs.dom.html.Div]
        val runButton =
          input(`type` := "submit",
            value := "Parse (ctrl-B)",
            `class` := "btn btn-success").render
        val treeDiv = div(`class` := "view bg-secondary")().render
        val exprDiv = div(`class` := "view bg-secondary")().render
        val parseInput =
          input(`type` := "text", `class` := "form-control").render
        val jsDiv =
          div(
            form(
            div(`class` := "form-group")(
            label("Sentence:"),
            parseInput),
            runButton),
            h4("Constituency parsed tree"),
            treeDiv,
            h4("Mathematical Expression"),
            exprDiv
          )
          parseDiv.appendChild(jsDiv.render)
      }
    }
}
