package provingground.functionfinder

import provingground.HoTT._
import ScalaRep._
import scala.reflect.runtime.universe.{Try => UnivTry, Function => FunctionUniv, _}

/**
 * Recursion and induction for (dependent) pairs.
 */
object InducPairs {
  val A ="A" :: __
  
  val B = "B" :: __
  
  val C = "C" :: __
  
  val f = "f" :: (A ->: B ->: C)

  val ab = "(a, b)" :: pair(A, B)
  
  val a = ab.first
  
  val b = ab.second
  
  val rec = lambda(f)(
      lambda(ab)(f(a)(b))
      )
}