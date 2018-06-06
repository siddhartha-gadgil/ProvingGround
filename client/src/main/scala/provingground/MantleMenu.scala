package provingground

import org.scalajs.dom
import scalajs.js.annotation._
import scalatags.JsDom.all._

import scala.scalajs.js
import org.scalajs.dom
import dom.ext._


@JSExportTopLevel("mantlemenu")
object MantleMenu {
  @JSExport
  def add(): Unit = {
    val navDiv = dom.document.querySelector("#left-nav")
    navDiv.appendChild(
      li(
        a(href := "build", target:="_blank")("Build")
      ).render
    )

    navDiv.appendChild(
      li(
        a(href := "halt", target:="_blank")("Halt")
      ).render
    )

    navDiv.appendChild(
      li(
        a(href := "prover.html")("Prover")
      ).render
    )

    navDiv.appendChild(
      li(
        a(href := "index.html")("Home")
      ).render
    )


  }
}
