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
   
  import Nat._

  val n = "n" :: Nat
  
  implicit val vecrep = polyRep[Term, Long]
  
//  3.toLong.hott(Nat)

  val Vec = (((n: Long) => (VecTyp[Term, Long](Nat, n.toInt) : Typ[Term])).hott(Nat ->: __)).get 
  
  private val ltyp = n ~>: (Vec(n) ->: Nat)
  
  private val vsize = (n : Long) => 
    (v : Vector[Long]) => {
      assert(v.size ==n ,"domain mismatch in Pi-type")
      n    
  }
    
  val size = vsize.hott(ltyp)

  private val vsucc = 
    (n : Long) => 
      (a : Long) =>
        (v : Vector[Long]) => {
          assert(v.size ==n ,"domain mismatch in Pi-type")
          a +: v  
          }
        
  private val succtyp  = n  ~>: Nat ->: (Vec(n) ->: Vec(Nat.succ(n)))
  
//  val succ = vsucc.hott(succtyp)
}