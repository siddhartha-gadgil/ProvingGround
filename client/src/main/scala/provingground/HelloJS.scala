package provingground

import scala.scalajs.js
import org.scalajs.dom

import HoTT._

object HelloJS extends js.JSApp {
  def main(): Unit = {
    dom.document.getElementById("scalajs").textContent = "Hello from Scala-js" + __
  }
}
