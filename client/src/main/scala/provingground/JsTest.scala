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

object ScalaJSExample {
  def main(): Unit = {
    // import org.scalajs.dom.document._
    dom.document.getElementById("scalajsShoutOut").textContent =
      HoTT.Type.toString

    import org.scalajs.dom.ext._

    val echo = span.render

    // import dom.ext._
    import scala.scalajs.concurrent.JSExecutionContext.Implicits.runNow

    val box = input(`type` := "text", placeholder := "Type here!").render

    val results = div().render

    val ticks = div("Ticks:").render

    val sse = new dom.EventSource("/events")

    import upickle.default._

    sse.onmessage = (event: dom.MessageEvent) => {
      ticks.appendChild(p(event.data.toString).render)
      ticks.appendChild(p("tick").render)
    }

    box.onchange = (e: dom.Event) => {
      echo.textContent = box.value
      Ajax.post("/kernel", box.value).foreach { (xhr) =>
        {
          val answer = xhr.responseText
          results.appendChild(p(answer).render)
        }
      }

    }

    val target = dom.document.getElementById("jsdiv")

    val (animalA, animalB) = ("fox", "dog")

    target.appendChild(
      div(h1("Hello World!"),
          p("The quick brown ",
            b(animalA),
            " jumps over the lazy ",
            i(animalB),
            "."),
          box,
          echo,
          p("ammonite results"),
          results,
          ticks).render)

    // Ace editor code

    CodeEditorJS.main()

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

    val box = input(`type` := "text", placeholder := "Type here!").render

    box.onchange = (e: dom.Event) => {
      echo.textContent = box.value
      Ajax.post("/bounce", box.value)
    }

    val sse = new dom.EventSource("/bouncestream")

    import upickle.default._

    sse.onmessage = (event: dom.MessageEvent) => {
      bouncers.appendChild(p(event.data.toString).render)
      val (a, b) = read[(String, String)](event.data.toString)
      bouncers.appendChild(i(a).render)
    }

    val target = dom.document.getElementById("jsdiv")

    val (animalA, animalB) = ("fox", "dog")

    target.appendChild(
      div(h1("Hello World!"),
          p("The quick brown ",
            b(animalA),
            " jumps over the lazy ",
            i(animalB),
            "."),
          box,
          echo,
          p("bouncers below"),
          bouncers).render)

  }
}
