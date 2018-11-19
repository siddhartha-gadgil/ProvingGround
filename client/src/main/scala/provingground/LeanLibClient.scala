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
import org.scalajs.dom.html.{Button, Div, LI, UList}
import scalatags.JsDom
import ujson.{read => _, _}
import upickle.default._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Try
import collection.mutable

@JSExportTopLevel("leanlib")
object LeanLibClient {
  @JSExport
  def load(): Unit = {
    val parseQueue: mutable.Set[String] = mutable.Set()

    val leanlibDiv = dom.document.querySelector("#leanlib-div")

    val logList = ul(`class` := "list-group").render

    val errList = ul(`class` := "list-group").render

    def addLog(s: String): Node =
      logList.appendChild(li(`class` := "list-group-item")(s).render)

    val logDiv = div(`class` := "panel-body view")(logList).render

    def addErr(s: String): Node =
      errList.appendChild(li(`class` := "list-group-item")(s).render)

    val errDiv: Div = div(`class` := "panel-body view")(errList).render

    val resultList = ul(`class` := "list-group").render

    def addResult(s: String): Node =
      resultList.appendChild(li(`class` := "list-group-item")(s).render)

    val resultDiv: Div = div(`class` := "panel-body view")(resultList).render

    val filesList = ul(`class` := "list-inline").render

    val filesDiv = div(`class` := "panel-body view")(filesList).render

    val codeListDiv = div().render

    val memListDiv = div().render

    val cancQuListDiv = div().render

    val namesDiv =
      div(`class` := "panel-body view").render

    val codeGen: Button =
      button(`type` := "button", `class` := "btn btn-danger pull-right")(
        "Generate Code").render

    codeGen.onclick = (_) => Ajax.post("/save-code")


    def nameLI(name: String): JsDom.TypedTag[LI] = {
      val btn = p(`class` := "link")(name).render
      btn.onclick = (_) => {
        Ajax.post("/lean-parse", name)
        parseQueue += name
        loadCanc()
      }
      li(btn)
    }

    def cancLI(name: String): JsDom.TypedTag[LI] = {
      val btn = p(`class` := "danglink")(name).render
      btn.onclick = (_) => {
        Ajax.post("/cancel", name)
        parseQueue -= name
        loadCanc()
      }
      li(btn)
    }

    def inducLI(name: String): JsDom.TypedTag[LI] = {
      val btn = p(`class` := "link")(name).render
      btn.onclick = (_) => Ajax.post("/inductive-definition", name)
      li(btn)
    }


    def fileLI(name: String): LI = {
      val btn =
        p(`class` := "link")(name.dropRight(".lean.export".length)).render
      btn.onclick = (_) => loadNames(name)
      li(btn).render
    }

    leanlibDiv.appendChild(
      div(
        codeGen,
        p(),
        p(),
        h3("""
          | Building a library by exporting from the lean import format.
        """.stripMargin),
        div(`class` := "col-md-8")(
          div(`class` := "panel panel-info")(
            div(`class` := "panel-heading")(h4("Files exported from lean:")),
            filesDiv),
          div(`class` := "panel panel-info")(
            div(`class` := "panel-heading")(
              h4("Lean Modifications, Memory, Code")),
            namesDiv),
          div(`class` := "panel panel-info")(
            div(`class` := "panel-heading")(h4("Results:")),
            resultDiv),
          div(`class` := "panel panel-info")(
            div(`class` := "panel-heading")(h4("Logs:")),
            logDiv)
        ),
        div(`class` := "col-md-4")(
          div(`class` := "panel panel-success")(
            div(`class`:= "panel-heading")("In  Generated Code:"),
            div(`class`:= "panel-body view")(codeListDiv)),
          div(`class` := "panel panel-success")(
            div(`class`:= "panel-heading")("In  Memory:"),
            div(`class`:= "panel-body view")(memListDiv)),
          div(`class` := "panel panel-warning")(
            div(`class`:= "panel-heading")("Currently Parsing:"),
            div(`class`:= "panel-body view")(cancQuListDiv)),
          div(`class` := "panel panel-danger")(
            div(`class` := "panel-heading")(h4("Errors:")),
            errDiv)
          )
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

    def loadCodeGen(): Unit =
      Ajax
        .get("./codegen-defns")
        .foreach { (xhr) =>
          val dfs = read[Vector[String]](xhr.responseText).sortBy(identity)
          addLog(dfs.mkString(","))
          val u = ol(dfs.map((s) => li(s)): _*).render
          codeListDiv.appendChild(u)
        }

    def loadMem(): Unit =
      Ajax
        .get("./mem-defns")
        .foreach { (xhr) =>
          val dfs = read[Vector[(String, String)]](xhr.responseText)
            .map(_._1)
            .sortBy(identity)
          addLog(dfs.mkString(","))
          val u = ol(dfs.map((s) => li(s)): _*).render
          memListDiv.innerHTML = ""
          memListDiv.appendChild(u)
        }

    def loadCanc() =
      {
        val cs: Seq[JsDom.TypedTag[LI]] = parseQueue.toVector.sortBy(identity).map(cancLI)
        val l = ol(cs : _*)
        cancQuListDiv.innerHTML = ""
        cancQuListDiv.appendChild(l.render)
      }

    def loadNames(file: String): Unit =
      Ajax
        .get(s"./mods/$file")
        .foreach { (xhr) =>
          val mods = read[Js.Value](xhr.responseText).arr.toVector
          val defMods: Vector[String] = mods.collect {
            case js if Set("definition", "axiom") contains js.obj("type").str =>
              js.obj("name").str
          }
          val dl: Seq[JsDom.TypedTag[LI]] = defMods.sortBy(identity).map(nameLI)
          val defList                     = ul(`class` := "list-inline")(dl: _*)
          val indMods: Vector[String] = mods.collect {
            case js if "inductive type" == js.obj("type").str =>
              js.obj("name").str
          }
          val il: Seq[JsDom.TypedTag[LI]] =
            indMods.sortBy(identity).map(inducLI)
          val indList = ul(`class` := "list-inline")(il: _*)
          val view =
            div(
              h3(s"Filename: $file"),
              h4("Definitions and Axioms"),
              defList,
              h4("Inductive types"),
              indList
            )
          namesDiv.innerHTML = ""
          namesDiv.appendChild(view.render)
        }

    val chat: WebSocket = new WebSocket(
      s"ws://${dom.document.location.host}/leanlib-websock")

    chat.onopen = { (_: Event) =>
      addLog("connected to WebSocket")
      loadFiles()
      loadCodeGen()
    }

    def getTeX(jsObj: Js.Obj): HTMLElement =
      Try {
        val d = span().render
        d.innerHTML = katex.renderToString(jsObj("tex").str)
        d
      }.getOrElse(h4(jsObj("plain").str).render)

    chat.onmessage = { (event: MessageEvent) =>
      val msg            = event.data.toString
      val jsObj          = read[Js.Value](msg).obj
      val msgTyp: String = jsObj("type").str
      msgTyp match {
        case "log" => addLog(jsObj("message").str)
        case "error" => addErr(jsObj("message").str)
        case "parse-result" =>
          addLog("parser result" + jsObj.toString())
          loadMem()
          parseQueue -= jsObj("name").str
          loadCanc()
          val res: HTMLElement = getTeX(jsObj)
          resultList.appendChild(
            li(`class` := "list-group-item")(
              h3("Parsed"),
              h4(s"${jsObj("name").str} ="),
              res
            ).render)
        case "inductive-definition" =>
          val indName = jsObj("name").str
          val typDiv  = getTeX(jsObj)
          val intros  = jsObj("intros").arr
          val introListItems: Vector[JsDom.TypedTag[LI]] = intros.toVector.map {
            (js) =>
              val name = js.obj("name").str
              val typ  = getTeX(js.obj)
              li(name, " : ", typ)
          }
          val introsList: JsDom.TypedTag[UList] = ul(introListItems: _*)
          val d                                 = div("Name: ", indName, "; type: ", typDiv, introsList).render
          resultList.appendChild(
            li(`class` := "list-group-item")(
              h3("Parsed Inductive type"),
              d
            ).render)
        case _ => addLog(s"unparsed web-socket message: $jsObj")
      }
    }

  }
}
