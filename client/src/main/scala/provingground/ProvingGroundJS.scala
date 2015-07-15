package provingground

import scala.scalajs.js
import org.scalajs.dom
import dom.html
import scalajs.js.annotation.JSExport
import scalatags.JsDom.all._
import scala.util.Try

import HoTT._

object ProvingGroundJS extends js.JSApp {
  def main(): Unit = {
    val page = Try(dom.document.getElementById("page")).map (_.textContent).getOrElse("default")

    page match {
      case "andrews-curtis" => AndrewsCurtisJS.andrewscurtisJS()
      case "default" => JsTest.jstest()
    }

//      JsTest.jstest()
  }
}
