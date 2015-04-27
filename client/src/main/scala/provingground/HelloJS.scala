package provingground

import scala.scalajs.js
import org.scalajs.dom
import dom.html
import scalajs.js.annotation.JSExport
import scalatags.JsDom.all._


import HoTT._

object HelloJS extends js.JSApp {
  def main(): Unit = {
    dom.document.getElementById("scalajs").textContent = "Hello from Scala-js: " + __

    val bouncers = div.render
    
    val sse= new dom.EventSource("../bouncestream")
    
    
    sse.onmessage = (event: dom.MessageEvent) => {
      bouncers.appendChild(
          p(event.data.toString).render
          )
    }
    
    
    val target = dom.document.getElementById("jsdiv")
    
    val (animalA, animalB) = ("fox", "dog")

    target.appendChild(
      div(
        h1("Hello World!"),
        p(
          "The quick brown ", b(animalA),
          " jumps over the lazy ",
          i(animalB), "."
        ),
        bouncers
      ).render
    )
    
  }
}
