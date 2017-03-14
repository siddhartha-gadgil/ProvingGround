package provingground

//import scala.scalajs.js
import org.scalajs.dom
//import dom.html
import scalajs.js.annotation.JSExport
import scalatags.JsDom.all._

import HoTT._

object JsTest {
  def jstest(): Unit = {
    dom.document.getElementById("scalajs").textContent = "Hello from Scala-js: " +
    Type

    val bouncers = div.render

    val echo = span.render

    import dom.ext._
    import scala.scalajs.concurrent.JSExecutionContext.Implicits.runNow

    val box = input(
        `type` := "text",
        placeholder := "Type here!"
    ).render

    box.onchange = (e: dom.Event) =>
      {
        echo.textContent = box.value
        Ajax.post("/bounce", box.value)
    }

    val sse = new dom.EventSource("/bouncestream")

    import upickle.default._

    sse.onmessage = (event: dom.MessageEvent) =>
      {
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
