package provingground.scalahott

import provingground._, HoTT._

import ScalaRep._

case class IntVector(dim: Int) extends ScalaTyp[Vector[Int]]

object IntVector {
  import NatTypLong.rep

  import ScalaRep.UnivRep

  val u = implicitly[ScalaRep[Typ[Term], Typ[Term]]]

  import ScalaPolyRep._

  implicit object IntVecRep
      extends ScalaPolyRep[ScalaTerm[Vector[Int]], Vector[Int]] {
    def apply(typ: Typ[Term])(elem: Vector[Int]) = typ match {
      case tp @ IntVector(dim) if dim == elem.size => Some(tp.rep(elem))
    }

    def unapply(term: ScalaTerm[Vector[Int]]) = term.typ match {
      case IntVector(dim) => IntVector(dim).rep.unapply(term)
      case _              => None
    }

    def subs(x: Term, y: Term) = this
  }

  val Vec = ScalaPolyTerm((n: Long) => IntVector(n.toInt): Typ[Term])
}
