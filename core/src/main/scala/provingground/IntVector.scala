package provingground

import HoTT._

import ScalaRep._

case class IntVector(dim: Int) extends ScalaTyp[Vector[Int]]

object IntVector{
  import Nat.rep
 
  import ScalaRep.UnivRep

  val u = implicitly[ScalaRep[Typ[Term], Typ[Term]]]
   
  val Vec = ((n: Long) => IntVector(n.toInt) : Typ[Term]).term

  import ScalaPolyRep._

  implicit object IntVecRep extends ScalaPolyRep[Term, Vector[Int]]{
    def apply(typ: Typ[Term])(elem : Vector[Int]) = typ match {
      case tp @ IntVector(dim) if dim ==elem.size => Some(tp.rep(elem))
    }

   def unapply(term : Term) = term.typ match {
     case IntVector(dim) => IntVector(dim).rep.unapply(term)
     case _ => None
   }

    def subs(x: Term, y: Term) = this

  }

}
