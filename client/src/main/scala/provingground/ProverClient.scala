package provingground

import org.scalajs.dom

import scalajs.js.annotation._
import scalatags.JsDom.all._

import scala.scalajs.js
import org.scalajs.dom
import dom.ext._
import monix.execution.Scheduler.Implicits.global
import monix.eval._
import provingground._
import HoTT._
import translation._
import learning._
import ProverTasks._
import monix.execution.CancelableFuture
import ujson._

@JSExportTopLevel("prover")
object ProverClient {
  @JSExport
  def load() = {
    val runButton =
      input(`type` := "button", value := "Query Server", `class` := "btn btn-success").render

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

    def query() = {
      Ajax.post("./monoid-proof").foreach { (xhr) =>
        {

          val answer = xhr.responseText
          val js   = ujson.read(answer)
          val proved = js.obj("proved").bool
          if (proved)
            {
              val termDiv = div(style := "overflow-x: auto;")().render
              val typDiv  = div(style := "overflow-x: auto;")().render
              termDiv.innerHTML = katex.renderToString(js.obj("term").str)
              typDiv.innerHTML = katex.renderToString(js.obj("type").str)
              proverDiv.appendChild(
                div(`class` := "border border-primary")(h2("From the server"),
                  h3("Found proof:"), termDiv, h3(" of the theorem: "), typDiv).render)
            }
          else
            proverDiv.appendChild(h3("could not find proof of theorem").render)
          }
        }
      }

      runButton.onclick = (e: dom.Event) => query()




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
          div(`class` := "border border-primary")(h2("From the browser"),
            h3("Found proof:"), termDiv, h3(" of the theorem: "), typDiv).render
        )
      case None =>
        proverDiv.appendChild(h3("could not find proof of theorem").render)
    }


  }
}
