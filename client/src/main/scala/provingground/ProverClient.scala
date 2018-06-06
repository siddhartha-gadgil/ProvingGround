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



@JSExportTopLevel("prover")
object ProverClient{
  @JSExport
  def load() = {
    val testDiv = dom.document.querySelector("#test-div")
    testDiv.appendChild(p("quick append").render)
    val fut = Task(1+2).runAsync

    fut.foreach{(_) =>
      testDiv.appendChild(p("ran a monix task").render)
    }

    testDiv.appendChild(p(HoTT.Type.toString).render)

    val tv = new TermEvolver(lambdaWeight = 0.0, piWeight = 0.0)

    val seekTask = {
      import library._, MonoidSimple._
      import scala.concurrent.duration._
      theoremSearchTraceTask(dist1, tv, math.pow(10.0, -6), 3.minutes, eqM(l)(r), decay = 3)
    }

    val seek: Task[Option[Term]] =  {
      import library._, MonoidSimple._
      import scala.concurrent.duration._
      theoremSearchTask(dist1, tv, math.pow(10.0, -6), 3.minutes, eqM(l)(r), decay = 3)
    }

    val seekFut: CancelableFuture[Option[Term]] = {
      testDiv.appendChild(p("starting seek task").render)
      seek.runAsync
    }

    seekFut.foreach{
      case Some(t) =>
        val termDiv = div(style := "overflow-x: auto;")().render
        val typSpan = span().render
        termDiv.innerHTML = katex.renderToString(TeXTranslate(t))
        typSpan.innerHTML = katex.renderToString(TeXTranslate(t.typ))
        testDiv.appendChild(div(h3("Found proof:"), termDiv, div(" of theorem: ", typSpan)).render)
      case None => testDiv.appendChild(p("could not find proof of theorem").render)
    }
  }
}
