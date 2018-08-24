package provingground

import org.scalajs.dom

import scalajs.js.annotation._
import scalatags.JsDom.all._
import org.scalajs.dom.raw._

import scala.scalajs.js
import org.scalajs.dom
import dom.ext._
import provingground._
import HoTT._
import org.scalajs.dom.html.LI
import ujson.{read => _, _}
import upickle.default._
import scala.concurrent.ExecutionContext.Implicits.global

@JSExportTopLevel("leanlib")
object LeanLibClient {
  @JSExport
  def load(): Unit = {

    val leanlibDiv = dom.document.querySelector("#leanlib-div")

    val logList = ul(`class` := "list-group").render

    def addLog(s: String): Node =
      logList.appendChild(li(`class` := "list-group-item")(s).render)

    val logDiv = div(`class` := "panel-body")(logList).render

    val filesList = ul(`class` := "list-inline").render

    val filesDiv = div(`class` := "panel-body")(filesList).render

    val filesBox = input(`type` := "text").render

    def fileLI(name: String): LI = {
      val btn = button(`type` := "button")(name.dropRight(".lean.export".size)).render
      btn.onclick = (_) => filesBox.value = name
      li(btn).render
    }

    leanlibDiv.appendChild(
      div(
        p("""
          | Building a library by exporting from the lean import format.
        """.stripMargin),
        div(`class` := "panel panel-info")(
          div(`class` := "panel-heading")(h4("Files exported from lean:")),
          filesDiv),
        div(`class` := "panel panel-info")(
          div(`class` := "panel-heading")(h4("Logs:")),
          logDiv)
      ).render
    )

    def loadFiles() =
      Ajax
        .get("./files")
        .foreach { (xhr) =>
          val files = read[Vector[String]](xhr.responseText)
          files.foreach((name) => filesList.appendChild(fileLI(name)))
        }

    val chat = new WebSocket(
      s"ws://${dom.document.location.host}/leanlib-websock")

    chat.onopen = { (_: Event) =>
      addLog("connected to WebSocket")
      loadFiles()
    }

    chat.onmessage = { (event: MessageEvent) =>
      val msg            = event.data.toString
      val jsObj          = read[Js.Value](msg).obj
      val msgTyp: String = jsObj("type").str
      msgTyp match {
        case "log" => addLog(jsObj("message").str) // TODO other cases
      }
    }

    def parsePost(name: String, file: String) =
      Ajax.post("./parse", Js.Obj("name" -> Js.Str(name), "file" -> Js.Str(file)).toString())

  }
}
