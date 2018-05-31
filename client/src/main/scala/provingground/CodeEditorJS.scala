package provingground

//import scala.scalajs.js
import org.scalajs.dom
//import dom.html
import scalajs.js.annotation._
import scalatags.JsDom.all._

import scala.scalajs.js
import org.scalajs.dom

// import com.karasiq.highlightjs.HighlightJS

import js.Dynamic.{global => g}

import com.scalawarrior.scalajs.ace._

import dom.ext._
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

import upickle.{Js, json}

import scala.util.{Try, Success, Failure}

import HoTT.{id => _, _}


@JSExportTopLevel("CodeEditorJS")
object CodeEditorJS {

  @JSExport
  def main(): Unit = {
    val editDiv = dom.document
      .getElementById("edit-div")
      .asInstanceOf[org.scalajs.dom.html.Div]

    val runButton =
      input(`type` := "button",
            value := "Run (ctrl-B)",
            `class` := "btn btn-space btn-primary pull-right").render

    val saveButton =
      input(`type` := "button",
            value := "save",
            `class` := "btn btn-space btn-success pull-right").render

    val objButton =
      input(`type` := "button",
            value := "create object",
            `class` := "btn btn-space btn-danger pull-right").render

    val loadButton =
      input(`type` := "button",
            value := "load",
            `class` := "btn btn-space btn-warning pull-right").render

    val insertButton =
      input(`type` := "button",
            value := "insert object",
            `class` := "btn btn-space btn-danger pull-right").render

    val nameInp =
      input(`type` := "text", placeholder := "scriptname", size := 10).render

    def filename = nameInp.value

    val logDiv = div().render

    val viewDiv = div(`class` := "view")().render

    val ed = div(id := "editor", `class` := "panel-body editor")

    editDiv.appendChild(
      div(
        div(`class` := "panel panel-default")(
          ed,
          div(`class` := "panel-footer clearfix")(label("script-name: "),
                                                  nameInp,
                                                  saveButton,
                                                  insertButton,
                                                  loadButton,
                                                  runButton,
                                                  objButton)),
        div("Logs:", logDiv),
        div("Output:", viewDiv)
      ).render)

    val editor = ace.edit("editor")
    editor.setTheme("ace/theme/chrome")
    editor.getSession().setMode("ace/mode/scala")

    // val ed = dom.document.getElementById("editor").asInstanceOf[org.scalajs.dom.html.Div]

    val text = editor.getValue()
    val initCommands =
      "import provingground._\nimport HoTT._\nimport induction.TLImplicits._\nimport shapeless._\nrepl.pprinter.bind(translation.FansiShow.simplePrint)\n\n"
    editor.insert(initCommands)

    def editorAppend(text: String) = {
      val prev  = editor.getValue
      val lines = prev.count(_ == '\n') + (if (prev.endsWith("\n")) 1 else 2)
      editor.gotoLine(lines)
      if (!prev.endsWith("\n")) editor.insert("\n")
      editor.insert(text)
      editor.gotoLine(lines + 2)
    }

    def parseAnswer(text: String): Option[Either[String, String]] =
      if (text == "None") None
      else {
        assert(text.startsWith("Some(") && text.endsWith(")"))
        val e = text.drop(5).dropRight(1)
        if (e.startsWith("Right(")) Some(Right(e.drop(6).dropRight(1)))
        else Some(Left(e.drop(5).dropRight(1)))
      }

    def showAnswer(result: Option[Either[String, String]]) =
      result.foreach { (lr) =>
        lr match {
          case Right(resp) =>
            editorAppend(s"//result: $resp\n\n")
            logDiv.innerHTML = ""
          case Left(err) =>
            logDiv.innerHTML = ""
            logDiv.appendChild(pre(div(`class` := "text-danger")(err)).render)
        }
      }

    def parseEither(s: String): Either[String, String] =
      if (s.startsWith("Right(")) Right(s.drop(6).dropRight(1))
      else Left(s.drop(5).dropRight(1))

    def parseTry(s: String): Either[String, String] =
      if (s.startsWith("Success(")) Right(s.drop(8).dropRight(1))
      else Left(s.drop(7).dropRight(1))

    def showEither(e: Either[String, String]) = {
      logDiv.innerHTML = ""
      e match {
        case Left(err) =>
          logDiv.appendChild(pre(div(`class` := "text-danger")(err)).render)
        case Right(res) =>
          logDiv.appendChild(pre(div(`class` := "text-success")(res)).render)
      }
    }

    def katex(f: String) = {
      val html = g.katex.renderToString(f)
      val d    = div.render
      d.innerHTML = html.toString
      d
    }

    def showLog(js: Js.Value) =
      js.obj.get("log").foreach { (err) =>
        logDiv.appendChild(pre(div(`class` := "text-danger")(err.str)).render)
      }

    def showResult(js: Js.Value) =
      js.obj.get("result").foreach { (resp) =>
        editorAppend(s"//result: $resp\n\n")
        logDiv.innerHTML = ""
      }

    def showTeX(js: Js.Value) = js.obj.get("tex").foreach { (resp) =>
      viewDiv.appendChild(katex(resp.str))
    }

    def compile() = {
      val codetext = editor.getValue()
      Ajax.post("/kernel", codetext).foreach { (xhr) =>
        {
          val answer = xhr.responseText
          logDiv.innerHTML = ""
          val view =
            if (answer.startsWith("--RESULT--\n"))
              answer.drop("--RESULT--\n".length)
            else answer.split("--OUTPUT--\n")(1)
          viewDiv.innerHTML = ""
          val codeDiv = code(`class` := "scala", view).render
          viewDiv.appendChild(pre(codeDiv).render)
          g.renderMathInElement(viewDiv, js.Dynamic.literal(ignoreTags = Seq()))
          if (answer.startsWith("--ERROR--\n")) {
            val err = answer
              .drop("--ERROR--\n".length)
              .split("--OUTPUT--\n")(0)
              .drop("--INFO--\n".length)
            logDiv.appendChild(pre(div(`class` := "text-danger")(err)).render)
          }
          g.hljs.highlightBlock(codeDiv)
          // val js     = json.read(answer)
          // showResult(js)
          // showLog(js)
          // showTeX(js)
        }
      }
    }

    runButton.onclick = (event: dom.Event) => compile()

    editDiv.onkeydown = (e) => {
      if (e.ctrlKey && e.keyCode == 66) compile()
    }

    def scriptListFut = Ajax.get("/list-scripts")

    def loadScriptFut(name: String) =
      Ajax.get(s"/script/$name").map { (xhr) =>
        val resp = parseTry(xhr.responseText)
        showEither(resp.map((_) => s"loaded script $name"))
        resp.foreach { (sc) =>
          editor.setValue(sc)
        }
      }

    def insertObject(name: String) = Ajax.get(s"/load-object/$name").map {
      (xhr) =>
        val resp = parseTry(xhr.responseText)
        showEither(resp.map((_) => s"loaded script $name"))
        resp.foreach { (sc) =>
          editor.insert(sc)
        }
    }

    def saveScript(name: String, body: String) =
      Ajax.post(s"/save-script/$name", body).map { (xhr) =>
        val resp = parseTry(xhr.responseText)
        showEither(resp)
      }

    def createObject(name: String, body: String) =
      Ajax.post(s"/create-object/$name", body).map { (xhr) =>
        val resp = parseTry(xhr.responseText)
        showEither(resp)
      }

    saveButton.onclick = (event: dom.Event) =>
      saveScript(filename, editor.getValue)

    loadButton.onclick = (event: dom.Event) => loadScriptFut(filename)

    objButton.onclick = (event: dom.Event) =>
      createObject(filename, editor.getValue)

    insertButton.onclick = (event: dom.Event) => insertObject(filename)

  }
}
