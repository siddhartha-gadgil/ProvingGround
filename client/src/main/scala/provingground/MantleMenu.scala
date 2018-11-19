package provingground

import org.scalajs.dom
import scalajs.js.annotation._
import scalatags.JsDom.all._

import scala.scalajs.js
import org.scalajs.dom
import dom.ext._
import scala.concurrent.ExecutionContext.Implicits.global


@JSExportTopLevel("mantlemenu")
object MantleMenu {
  @JSExport
  def add(): Unit = {
    val navDiv = dom.document.querySelector("#left-nav")
    navDiv.appendChild(
      li(
        a(href := "build", target := "_blank")("Build")
      ).render
    )


    navDiv.appendChild(
      li(
        a(href := "prover.html")("Demo Prover")
      ).render
    )

    navDiv.appendChild(
      li(
        a(href := "leanlib.html")("Lean Library")
      ).render
    )

    navDiv.appendChild(
      li(
        a(href := "interactive-prover.html")("Interactive Prover")
      ).render
    )


    navDiv.appendChild(
      li(
        a(href := "scripts/index.html", target := "_blank")("Fiddle")
      ).render
    )

    navDiv.appendChild(
      li(
        a(href := "index.html")("Home")
      ).render
    )

    Ajax.get("/nlp.html").foreach{_ =>
      navDiv.appendChild(
      li(
        a(href := "nlp.html")("NLP")
      ).render
    )
    }

  }
}
