package provingground

import scala.scalajs.js
import org.scalajs.dom

object ScalaJSExample extends js.JSApp {
  def main(): Unit = {
    dom.document.getElementById("scalajs").textContent = "Hello from Scala-js"
  }
}
