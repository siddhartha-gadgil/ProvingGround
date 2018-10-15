package provingground

import org.scalajs.dom

import scalajs.js.annotation._
import scalatags.JsDom.all._
import org.scalajs.dom.raw._

import scala.scalajs.js
import org.scalajs.dom
import dom.ext._
import monix.execution.Scheduler.Implicits.global
import monix.eval._
import HoTT.{id => _, _}
import translation._
import learning._
import FineProverTasks._
import com.scalawarrior.scalajs.ace.ace
import monix.execution.CancelableFuture
import ujson._

@JSExportTopLevel("interactiveProver")
object InteractiveProver {
  @JSExport
  def load(): Unit = {
    val proverDivOpt = Option(
      dom.document.querySelector("#interactive-prover-div"))
    proverDivOpt.foreach { proverDiv =>
      val ed = div(id := "editor", `class` := "panel-body editor")

      val viewDiv = div(`class` := "mini-view")().render

      val runButton =
        input(`type` := "button",
              value := "Parse Context (ctrl-B)",
              `class` := "btn btn-success").render

      val contextDiv =
        div(
          div(`class` := "panel panel-primary")(
            div(`class` := "panel-heading")(
              h4("Context Editor"),
              p("The context can be used for convenient definitions for finite distributions and for inductive types.")),
            ed,
            div(`class` := "panel-footer")(runButton)
          ),
          div(h3("Parsed Context:"), viewDiv)
        ).render

      val parser = HoTTParser()

      proverDiv.appendChild(
        div(
          contextDiv
        ).render
      )

      val editor = ace.edit("editor")
      editor.setTheme("ace/theme/chrome")
      editor.getSession().setMode("ace/mode/scala")

      def compile(): Unit = {
        val text = editor.getValue

        val view = parser.context
          .parse(text)
          .fold(
            (_, _, s) =>
              div(
                h5(`class` := "text-danger")("Error"),
                div(s.traced.trace)
            ),
            (bl, _) =>
              div(
                h5(`class` := "text-success")("Context Parsed"),
                bl.valueOpt
                  .map { (t) =>
                    val termSpan = span().render
                    val typSpan  = span().render
                    termSpan.innerHTML = katex.renderToString(TeXTranslate(t))
                    typSpan.innerHTML =
                      katex.renderToString(TeXTranslate(t.typ))
                    div(ul(`class` := "list-inline")(li("Term: ", termSpan),
                                                     li("Type: ", typSpan)),
                        p("Context: ", bl.toString))
                  }
                  .getOrElse(div("Empty Context"))
            )
          )

        viewDiv.innerHTML = ""
        viewDiv.appendChild(view.render)
      }

      runButton.onclick = (event: dom.Event) => compile()

      contextDiv.onkeydown = (e) => {
        if (e.ctrlKey && e.keyCode == 66) compile()
      }

    }
  }
}

@JSExportTopLevel("prover")
object ProverClient {
  @JSExport
  def load(): Unit = {
    val runButton =
      input(`type` := "button",
            value := "Ask Server for proof",
            `class` := "btn btn-primary").render

    val proverDiv = dom.document.querySelector("#prover-div")
    proverDiv.appendChild(
      div(
        p("""
          |This is a demonstration of autonomous proving.
          |The key feature of our approach is that we judge the value of intermediate lemmas in a
          |manner that emphasises parsimony of the generative model. This means that for a
          |term to be considered an interesting lemma:
        """.stripMargin),
        ul(
          li(
            "Its statement must be simple compared to the proof, and moreover"),
          li("the difference in complexity should be enough given a significant cost for noting an additional lemma")
        ),
        p("The example result we have is showing that if left and right identities exist, they are equal"),
        runButton
      ).render
    )

    // val sse = new dom.EventSource("./proof-source")
    //
    // sse.onmessage = (event: dom.MessageEvent) => showProof(event.data.toString)

    def showProof(data: String): Unit =
      if (data.nonEmpty) {
        runButton.value = "Ask Server for proof"
        val answer = data
        val js     = ujson.read(answer)
        val proved = js.obj("proved").bool
        if (proved) {
          val termDiv = div(style := "overflow-x: auto;")().render
          val typDiv  = div(style := "overflow-x: auto;")().render
          termDiv.innerHTML = katex.renderToString(js.obj("term").str)
          typDiv.innerHTML = katex.renderToString(js.obj("type").str)

          proverDiv.appendChild(
            ul(`class` := "list-group")(
              li(`class` := "list-group-item list-group-item-primary")(
                "From the server via WebSocket:"),
              li(`class` := "list-group-item list-group-item-info")("Theorem"),
              li(`class` := "list-group-item")(typDiv),
              li(`class` := "list-group-item list-group-item-success")("Proof"),
              li(`class` := "list-group-item")(termDiv),
            ).render)

          val lemmaSeq = js.obj("lemmas").arr
          def lemmaLI(lm: Js.Value) = {
            val termDivL = div(style := "overflow-x: auto;")().render
            val typDivL  = div(style := "overflow-x: auto;")().render
            termDivL.innerHTML = katex.renderToString(lm.obj("term").str)
            typDivL.innerHTML = katex.renderToString(lm.obj("type").str)
            li(`class` := "list-group-item")(
              ul(`class` := "list-group")(
                li(`class` := "list-group-item list-group-item-info")("Lemma"),
                li(`class` := "list-group-item")(typDivL),
                li(`class` := "list-group-item list-group-item-success")(
                  "Proof"),
                li(`class` := "list-group-item")(termDivL),
              )
            )
          }
          val lemmaLISeq = lemmaSeq.map(lemmaLI)
          proverDiv.appendChild(
            div(
              h3("Lemmas:"),
              ul(`class` := "list-group")(
                // li(`class` := "list-group-item list-group-item-warning")("Lemmas:"),
                lemmaLISeq: _*
              )
            ).render
          )

        } else
          proverDiv.appendChild(h3("could not find proof of theorem").render)

      }

    def queryPost() = {
      runButton.value = "Asking Server"
      Ajax
        .post("./monoid-proof")
        .foreach { (xhr) =>
          runButton.value = xhr.responseText
        }
    }

    runButton.onclick = (e: dom.Event) => queryPost() // change this to query a websocket

    val chat = new WebSocket(
      s"ws://${dom.document.location.host}/monoid-websock")

    chat.onopen = { (event: Event) =>
      runButton.value = "Ask Server (over websocket)"
      runButton.onclick = (e: dom.Event) => {
        runButton.value = "Asking Server (over WebSocket)"
        chat.send("monoid-proof")
      }
    }

    chat.onmessage = { (event: MessageEvent) =>
      val msg = event.data.toString
      if (msg == "monoid-proof") runButton.value = "Server Working"
      else showProof(msg)
    }

    val tv = new TermEvolver(lambdaWeight = 0.0, piWeight = 0.0)

    val seekTask = {
      import library._, MonoidSimple._
      import scala.concurrent.duration._
      theoremSearchTraceTask(dist1,
                             tv,
                             math.pow(10.0, -6),
                             3.minutes,
                             eqM(l)(r),
                             decay = 3)
    }

    val seek: Task[Option[Term]] = {
      import library._, MonoidSimple._
      import scala.concurrent.duration._
      theoremSearchTask(dist1,
                        tv,
                        math.pow(10.0, -6),
                        10.minutes,
                        eqM(l)(r),
                        decay = 3)
    }

    val seekFut: CancelableFuture[Option[Term]] = {
      proverDiv.appendChild(
        div(
          p("""The search has been started on the browser, which is very slow.
          You can query the server for much quicker computation.""")
        ).render
      )
      seek.runAsync
    }

    seekFut.foreach {
      case Some(t) =>
        val termDiv = div(style := "overflow-x: auto;")().render
        val typDiv  = div(style := "overflow-x: auto;")().render
        termDiv.innerHTML = katex.renderToString(TeXTranslate(t))
        typDiv.innerHTML = katex.renderToString(TeXTranslate(t.typ))
        proverDiv.appendChild(
          ul(`class` := "list-group")(
            li(`class` := "list-group-item list-group-item-primary")(
              "From the browser:"),
            li(`class` := "list-group-item list-group-item-info")("Theorem"),
            li(`class` := "list-group-item")(typDiv),
            li(`class` := "list-group-item list-group-item-success")("Proof"),
            li(`class` := "list-group-item")(termDiv),
          ).render)
      case None =>
        proverDiv.appendChild(h3("could not find proof of theorem").render)
    }

  }
}
