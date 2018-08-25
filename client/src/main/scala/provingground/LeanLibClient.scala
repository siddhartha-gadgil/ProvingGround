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
import scalatags.JsDom
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

    val logDiv = div(`class` := "panel-body view")(logList).render

    val filesList = ul(`class` := "list-inline").render

    val filesDiv = div(`class` := "panel-body")(filesList).render

    val namesDiv = div(`class` := "panel-body view")(h2("Names of elements in files")).render




    def nameLI(name: String): JsDom.TypedTag[LI] = {
      val btn = button(`type` := "button")(name).render
      btn.onclick = (_) => Ajax.post("/parse", name)
      li(btn)
    }

    def fileLI(name: String): LI = {
      val btn = button(`type` := "button")(name.dropRight(".lean.export".length)).render
      btn.onclick = (_) => loadNames(name)
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
          div(`class` := "panel-heading")(h4("Lean Modifications")),
          namesDiv),
        div(`class` := "panel panel-info")(
          div(`class` := "panel-heading")(h4("Logs:")),
          logDiv)
      ).render
    )

    def loadFiles(): Unit =
      Ajax
        .get("./files")
        .foreach { (xhr) =>
          val files = read[Vector[String]](xhr.responseText)
          addLog(files.mkString(","))
          files.foreach((name) => filesList.appendChild(fileLI(name)))
        }

    def loadNames(file: String): Unit =
      Ajax
        .get(s"./mods/$file")
        .foreach { (xhr) =>
          val mods = read[Js.Value](xhr.responseText).arr.toVector
          val defMods : Vector[String]= mods.collect{case js if js.obj("type").str == "definition" =>
            js.obj("name").str}
          val dl: Seq[JsDom.TypedTag[LI]] = defMods.sortBy(identity).map(nameLI)
          val defList = ul(`class`:= "list-inline")(dl :_*)
          val view =
            div(h3(s"Filename: $file"), h4("definitions"), defList)
          namesDiv.appendChild(view.render)
        }

    val chat: WebSocket = new WebSocket(
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
        case "log" => addLog(jsObj("message").str)
        case "parse-result" =>
          addLog("parser result" + jsObj.toString())
          val res = div().render
          res.innerHTML = katex.renderToString(jsObj("tex").str.replace("$", ""))
          logList.appendChild(li(`class` := "list-group-item")(
            h3("Parsed"), h4(s"${jsObj("name").str} ="), res
          ).render)
        case _ => addLog(s"unparsed websok message: ${jsObj}")
      }
    }



  }
}
