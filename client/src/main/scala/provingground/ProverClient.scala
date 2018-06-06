package provingground

import org.scalajs.dom
import scalajs.js.annotation._
import scalatags.JsDom.all._

import scala.scalajs.js
import org.scalajs.dom
import dom.ext._
import monix.execution.Scheduler.Implicits.global
import monix.eval._
import provingground._, HoTT._, translation._, learning._
import ProverTasks._

import scala.concurrent.duration._

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
      theoremSearchTraceTask(dist1, tv, math.pow(10.0, -6), 3.minutes, eqM(l)(r), decay = 3)
    }

    val seekFut = {
      testDiv.appendChild(p("starting seek task").render)
      seekTask.runAsync
    }

    seekFut.foreach{
      case Some(t) => testDiv.appendChild(p(s"Found proof $t").render)
      case None => testDiv.appendChild(p("could not find theorem").render)
    }
  }
}
