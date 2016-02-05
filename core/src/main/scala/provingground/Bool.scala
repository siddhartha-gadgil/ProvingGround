package provingground

import HoTT._
import ScalaRep._

object Bool extends ScalaTyp[Boolean]{

  val not = ((x: Boolean) => !x).term

  val and = ((x: Boolean) => (y: Boolean) => x && y).term

  val or = ((x: Boolean) => (y: Boolean) => x || y).term

  def ifThenElse[U<: Term with Subs[U], X](pos: X, neg: X)(implicit xrep: ScalaRep[U, X]) =
    ((x: Boolean) => (if (x) pos else neg)).term

  val isTrue = ((x: Boolean) => if (x) (One: Typ[Term]) else Zero).term
}
