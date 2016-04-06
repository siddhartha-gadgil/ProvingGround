package provingground

import HoTT._

object MereProposition {
  def isPropn[U <: Term with Subs[U]](typ: Typ[U]) = 
  {
    val x = typ.Var
    val y = typ.Var
    pi(x)(pi(y)(x =:=y))
  }
  
  // Should refine to type of unit
  case class Truncation[U <: Term with Subs[U]](base: Typ[U]) extends Typ[Term]{
    lazy val typ = base.typ
    
    def subs(x: Term, y: Term) = Truncation(base.replace(x, y))
    
    def newobj = Truncation(base.newobj)
    
    def symbObj(name: AnySym) = SymbObj(name, this)
    
    type Obj = Term 
    
    lazy val witness = isPropn(this)
  }
  
  case class Instance[U <: Term with Subs[U]](base: Typ[U]) extends Func[U, Term]{
    lazy val dom = base
    
    lazy val codom = Truncation(base)
    
    lazy val typ = dom ->: codom
    
  
    def act(arg: U): provingground.HoTT.Term = codom.symbObj(ApplnSym(this, arg))   
    
    def subs(x: provingground.HoTT.Term,y: provingground.HoTT.Term) = Instance(base.replace(x, y))      
    
    def newobj = Instance(base.newobj)
    
  }
  
  case class Factorise[U <: Term with Subs[U], V<: Typ[V] with Subs[V]](
      A: Typ[U], B: Typ[V]) extends Func[Term, Func[Func[U, V], Func[Term, V]]] with Subs[Factorise[U, V]]{
    
    lazy val dom =  isPropn(B)
    
    lazy val codom = (A ->: B) ->: (Truncation(A) ->: B)
    
    lazy val typ = dom ->: codom
    
    def subs(x: Term, y: Term) = Factorise(A.replace(x,y), B.replace(x, y))
    
    def newobj = Factorise(A.newobj, B.newobj)
    
    def act(arg: Term)= codom.symbObj(ApplnSym(this, arg))
    
  }
}