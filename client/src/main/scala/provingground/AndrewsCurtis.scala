package provingground

import scala.scalajs.js
import org.scalajs.dom
import dom.html
import scalajs.js.annotation.JSExport
import scalatags.JsDom.all._

import Collections._

import HoTT._


object AndrewsCurtis{
  import dom.ext._

  import scala.scalajs
              .concurrent
              .JSExecutionContext
              .Implicits
              .runNow


  def fdDiv[A](fd: FiniteDistribution[A]) = {
    val lst = fd.pmf.toList.sortBy((x) => -x.weight).zipWithIndex
    val title = div(`class`:="atom")(
        span(`class`:="index")("index"),
        span(`class`:="probability")("probability"),
        span(`class`:="entropy")("entropy"),
        span(`class`:="element")("element")
        )

    val nodeList = for ((Weighted(a, x), j) <- lst)
      yield (
          div(`class`:="atom")(
        span(`class`:="index")(j),
        span(`class`:="probability")(x),
        span(`class`:="entropy")(-math.log(x)/math.log(2)),
        span(`class`:="element")(a.toString)
        )
        )
   div(`class`:="finite-distribution")(title,
       div(nodeList : _*)).render

  }
              

}
