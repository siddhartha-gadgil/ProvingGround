package provingground

import HoTT._

import ScalaRep.{ScalaSym}

//import ScalaRep._

import ScalaPolyRep._

/**
 * @author gadgil
 */
class ScalaVec[X](val basetyp: Typ[Term])(implicit baserep: ScalaPolyRep[RepTerm[X], X]) {
  case class VecTyp(dim: Int) extends ScalaTyp[Vector[X]]
  
  implicit object Rep extends ScalaPolyRep[RepTerm[Vector[X]], Vector[X]]{
    def apply(typ: Typ[Term])(elem: Vector[X]) = typ match{
      case tp @ VecTyp(dim) if dim == elem.size => {
        val pattern = new ScalaSym[RepTerm[Vector[X]], Vector[X]](tp)
        Some(pattern(elem))        
      }
      case _ => None
    }
    
    def unapply(term: RepTerm[Vector[X]]) = term.typ match {
      case tp : VecTyp => {
        val pattern = new ScalaSym[Term, Vector[X]](tp)
        pattern.unapply(term)
      }
      case _ => None
    }
    
    def subs(x: Term, y: Term) = this
  }
  
  
  
  implicit val nrep = poly(NatTyp.rep)
  
  implicit val urep = poly(ScalaRep.UnivRep)
  
  private val a = basetyp.Var

  
  private val n = "n" :: NatTyp
  
  
  val Vec = ((n: Long) => (VecTyp(n.toInt) : Typ[Term])).hott(NatTyp ->: __).get
  
  private val ltyp = n ~>: (Vec(n) ->: NatTyp)
  
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
        
  val m : RepTerm[Long] = NatTyp.succ(n)      
        
  private val constyp  = n  ~>: NatTyp ->: (Vec(n) ->: Vec(m))
  
  import NatTyp.rep
  
  implicitly[ScalaPolyRep[RepTerm[Long], Long]]
  
  implicitly[ScalaPolyRep[RepTerm[Vector[X]], Vector[X]]]

  
  implicit val r = depFuncPolyRep(implicitly[ScalaPolyRep[RepTerm[Long], Long]], 
      implicitly[ScalaPolyRep[FuncLike[RepTerm[X], FuncLike[RepTerm[Vector[X]], RepTerm[Vector[X]]]], X => Vector[X] => Vector[X]]])    
      
  val succ = ScalaPolyTerm(vcons)(r).hott(constyp).get        

  
  val empty = Vector.empty[X].hott(VecTyp(0)).get
}