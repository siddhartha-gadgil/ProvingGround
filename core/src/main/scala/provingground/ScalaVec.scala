package provingground

import HoTT._

import ScalaRep._

//import ScalaRep._

import ScalaPolyRep._

/**
 * @author gadgil
 */
class ScalaVec[X](val basetyp: Typ[Term])(implicit baserep: ScalaPolyRep[Term, X]) {
  case class VecTyp(dim: Int) extends ScalaTyp[Vector[Long]]
  
  implicit object Rep extends ScalaPolyRep[Term, Vector[X]]{
    def apply(typ: Typ[Term])(elem: Vector[X]) = typ match{
      case tp @ VecTyp(dim) if dim == elem.size => {
        val pattern = new ScalaSym[Term, Vector[X]](tp)
        Some(pattern(elem))        
      }
      case _ => None
    }
    
    def unapply(term: Term) = term.typ match {
      case tp : VecTyp => {
        val pattern = new ScalaSym[Term, Vector[X]](tp)
        pattern.unapply(term)
      }
      case _ => None
    }
    
    def subs(x: Term, y: Term) = this
  }
  
  
  implicit val nrep = poly(Nat.rep)
  
  implicit val urep = poly(ScalaRep.UnivRep)
  
  private val a = basetyp.Var

  
  private val n = "n" :: Nat
  
  
  val Vec = ((n: Long) => (VecTyp(n.toInt) : Typ[Term])).hott(Nat ->: __).get
  
  private val ltyp = n ~>: (Vec(n) ->: Nat)
  
  private val vsize = (n : Long) => 
    (v : Vector[X]) => {
      assert(v.size ==n ,"domain mismatch in Pi-type")
      n    
  }
    
  val size = vsize.hott(ltyp)
  
  
  private val vcons = 
    (n : Long) => 
      (a : X) =>
        (v : Vector[X]) => {
          assert(v.size ==n ,"domain mismatch in Pi-type")
          a +: v  
          }
        
  private val constyp  = n  ~>: Nat ->: (Vec(n) ->: Vec(Nat.succ(n)))
  
  import Nat.rep
  
  
  
//  implicit val r = depFuncPolyRep(implicitly[ScalaPolyRep[RepTerm[Long], Long]], 
//      implicitly[ScalaPolyRep[FuncLike[RepTerm[Long], FuncLike[RepTerm[Vector[Long]], RepTerm[Vector[Long]]]], X => Vector[X] => Vector[X]]])
  
//  private val term = ScalaPolyTerm[FuncLike[Term, FuncLike[Term, FuncLike[Term, Term]]], Long => (X => (Vector[X] => Vector[X]))](vcons)
  
//  val succ = term.hott(constyp).get
  
//  val empty = Vector.empty[X].hott(VecTyp(0)).get
}