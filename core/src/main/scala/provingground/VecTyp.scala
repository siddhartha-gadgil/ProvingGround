package provingground

import HoTT._

import ScalaPolyRep._

import ScalaRep._

/**
 * @author gadgil
 */
case class VecTyp[U<: Term with Subs[U], X](basetyp: Typ[U], dim: Int) extends ScalaTyp[Vector[X]]{
  
}

object VecTyp{
  case class VecPolyRep[U <: Term with Subs[U], X]()(implicit baserep: ScalaRep[U, X]) extends ScalaPolyRep[Term, Vector[X]]{
    def apply(typ: Typ[Term])(elem: Vector[X]) = typ match{
      case tp @ VecTyp(basetyp, dim) if dim == elem.size => {
        Some(tp.rep(elem))        
      }
      case _ => None
    }
    
    def unapply(term: Term) = term.typ match {
      case tp : VecTyp[_, X] => tp.rep.unapply(term)
      case _ => None
    }
    
    def subs(x: Term, y: Term) = this
  }
  
  implicit def polyRep[U <: Term with Subs[U], X](implicit baserep: ScalaRep[U, X]) : ScalaPolyRep[Term, Vector[X]] = VecPolyRep[U, X]

  val f = VecTyp(Nat, 1) ->: VecTyp(Nat, 1)
   
  import Nat.rep
    
  implicit val r = polyRep[Term, Long]
  
  val v = Vector(1.toLong)
  
  ScalaPolyTerm(v)
  
  v.hott(VecTyp(Nat, 1))
  
  
}