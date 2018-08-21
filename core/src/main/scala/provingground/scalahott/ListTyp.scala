package provingground.scalahott

import ScalaRep._

import provingground._, HoTT._

/**
  * @author gadgil
  * list of elements with type determined by the scala representation
  */
case class ListTyp[U <: Term with Subs[U], X]()(
    implicit baserep: ScalaRep[U, X])
    extends ScalaTyp[List[X]] {
  val empty = (List(): List[X]).term

  val cons = ((x: X) => (l: List[X]) => x :: l).term
}
