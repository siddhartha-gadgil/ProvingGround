package provingGround

import provingGround.HoTT._
import scala.reflect.runtime.universe.{Try => UnivTry, Function => FunctionUniv, _}
import annotation._

object Contexts{
  trait DefnEquality{
    val lhs : Term
    val rhs: Term
    val freevars: List[Term]
    
    def map(f: Term => Term): DefnEquality
   
  }
  
  case class DefnEqual(lhs: Term, rhs: Term, freevars : List[Term] = List()) extends DefnEquality{
    def map(f : Term => Term) = DefnEqual(f(lhs), f(rhs), freevars map (f))
    
    def apply(substitutions : Term => Option[Term]) = DefnEqual.instantiate(substitutions)(this)
    
    def apply(substitutions : PartialFunction[Term, Term]) = DefnEqual.instantiate(substitutions.lift)(this)
  }
  
  object DefnEqual{
    def instantiate(substitutions : Term => Option[Term])(dfn : DefnEqual) : Option[DefnEquality] = {
      if (dfn.freevars.isEmpty) Some(dfn) 
      else for (headvar <- dfn.freevars.headOption; 
      value <- substitutions(headvar); 
      recval <- instantiate(substitutions)(DefnEqual(dfn.lhs.subs(headvar, value), dfn.rhs.subs(headvar, value), dfn.freevars.tail))) 
        yield recval
    }
  }
  
  case class Defn(lhsname : AnySym, typ : Typ[Term], rhs: Term, freevars: List[Term]) extends DefnEquality{    
    val lhs = typ.symbObj(lhsname)
    
    def map(f : Term => Term, F : Typ[Term]=> Typ[Term]) = Defn(lhsname, F(typ), f(rhs), freevars map (f))
    
    def map(f: Term => Term) = Defn(lhsname, f(rhs))
  }
  
  object Defn{
    def infer(name: AnySym, rhs : Term) : Defn = Defn(name, rhs.typ, rhs, List())
    
    def apply(name: AnySym, rhs : Term) = infer(name, rhs)
  }
  
  trait Context[+U <: Term , V <: Term] extends TypSeq[U, V]{
//    val typ: Typ[PtnType]
    
    def tail: Context[Term, V]
    
    def newtail(newtail: Context[Term, V]): Context[Term, V]
    
    type PtnType <: U
    
    def foldin : Typ[V] => PtnType => V
    
    def foldinSym(tp: Typ[V])(name: AnySym) = foldin(tp)(apply(tp).symbObj(name))
    
    def symblist(tp: Typ[V])(varnames: List[AnySym]): List[Term]
    
    def constants : Seq[Term]
    
    def defns : Seq[Defn]
    
    def defnEqualities : Seq[DefnEquality]
    
    def eliminate(isVar : Term => Boolean): Term => Term
    
    val elim : Term => Term
    
    type ArgType
    
    val instantiate : ArgType => Term => Term
    
    def lmbda(x : Term) = LambdaMixin(x, this)
    
    def kappa(x : Term) = KappaMixin(x, this)
    
    def deplmbda(x : Term) = LambdaMixin(x, this, true)
    
    def cnst(x: Term) = KappaMixin(x, this)
    
    def dfn(x: Term, y: Term, local: Boolean = true) = DefnMixin(Defn(x, y), this)
    
    def dfneql(lhs: Term, rhs: Term, simp: Boolean = false) = DefnEqualityMixin(DefnEqual(lhs, rhs), this)
    
    def globaldfn(x: Term, y: Term, local: Boolean = true) = GlobalDefnMixin(Defn(x, y), this)
    
    def simpeql(lhs: Term, rhs: Term, simp: Boolean = false) = SimpEqualityMixin(DefnEqual(lhs, rhs), this)
  }
  
  object Context{
    case class empty[U <: Term : TypeTag]() extends Context[U, U]{
    	def tail: Nothing = 
    			throw new NoSuchElementException("tail of empty context") 
      
    	def newtail(newtail: Context[Term, U]) = this
    	
    	type PtnType = U

    	def apply(tp : Typ[U]) : Typ[PtnType] = tp
    	
    	def foldin : Typ[U] => PtnType => U = _ => (t) => t
    	
    	def symblist(tp: Typ[U])(varnames: List[AnySym]) = List()
    	
    	def constants : Seq[Term] = List.empty
    
    	def defns : Seq[Defn] = List.empty
    
    	def defnEqualities : Seq[DefnEquality] = List.empty
    
    	def eliminate(isVar : Term => Boolean): Term => Term = (x) => x
    	
    	val elim = (t: Term) => t
    	
    	type ArgType = Unit
    	
    	val instantiate : ArgType => Term => Term = _ => (t) => t
      
    }
    
    def apply[U <: Term : TypeTag] = empty[U]
    
    
  }
  

  
  
  case class LambdaMixin[+U<: Term, V <: Term](variable: Term, tail: Context[U, V], dep: Boolean = false) extends Context[FuncTerm[Term,U], V]{
    type PtnType = FuncTerm[Term, tail.PtnType]
    
    def newtail(newtail: Context[Term, V]) = LambdaMixin(variable, newtail, dep)
    
    def foldin : Typ[V] => PtnType => V = (tp) => (f) => tail.foldin(tp)(f(variable))
    
    def symblist(tp: Typ[V])(varnames: List[AnySym]) = apply(tp).symbObj(varnames.head) :: tail.symblist(tp)(varnames.tail)
    
    def constants = variable +: tail.constants
    
    def eliminator(y : Term) = Lambda(variable, y)
    
    def defns : Seq[Defn] = tail.defns map ((dfn) => dfn.map(optlambda(variable)))
    
    def defnEqualities : Seq[DefnEquality] = tail.defnEqualities map ((dfn) => dfn.map(optlambda(variable)))
    
    def eliminate(isVar : Term => Boolean): Term => Term = (y) => 
      if (isVar(variable)) Lambda(variable, tail.eliminate(isVar)(y))
      else tail.eliminate(isVar)(y)
    
      
    val elim = (y: Term) => Lambda(variable, tail.elim(y))
    
    type ArgType = (Term, tail.ArgType)
    
    val instantiate : ArgType => Term => Term = (fstrest) => (t) => {
      val rest: tail.ArgType = fstrest._2
      val fst = fstrest._1
      val func = Lambda(variable,tail.instantiate(rest)(t))
      func(fst)
    }
    
    // If tail.typ is a function of variable, we get a Pi-type instead.
    /*
    val typ = if (dep) {
      val fibre = (t : Term) => tail.typ subs (x, t)
	    
	    val family = typFamilyDefn[Term, tail.PtnType](variable.typ, MiniVerse(tail.typ), fibre)
	    PiTyp(family)
    }
    else FuncTyp(variable.typ, tail.typ)
    */
    
    def apply(tp : Typ[V]) : Typ[PtnType] = if (dep) {
      val fibre = (t : Term) => tail(tp) subs (variable, t)
	    
	    val family = typFamilyDefn[Term, tail.PtnType](variable.typ, MiniVerse(tail(tp)), fibre)
	    PiTyp(family)
    }
    else FuncTyp(variable.typ, tail(tp))
    
    
  }
  
  case class KappaMixin[+U<: Term, V <: Term](const : Term, tail: Context[U, V]) extends Context[U, V]{
    type PtnType = tail.PtnType
    
    def newtail(newtail: Context[Term, V]) = KappaMixin(const, newtail)
    
    def foldin : Typ[V] => PtnType => V = tail.foldin
    
    def symblist(tp: Typ[V])(varnames: List[AnySym]) = tail.symblist(tp)(varnames)
    
    def constants = const +: tail.constants
    
    def defns : Seq[Defn] = tail.defns
    
    def defnEqualities : Seq[DefnEquality] = tail.defnEqualities
    
    def eliminate(isVar : Term => Boolean): Term => Term = tail.eliminate(isVar)
    
    val elim = tail.elim
    
    type ArgType = tail.ArgType
    
    val instantiate : ArgType => Term => Term = _ => (t) => t
    
//    val typ = tail.typ
    
    def apply(tp : Typ[V]) : Typ[PtnType] =tail(tp)
  }
  
  case class DefnMixin[+U<: Term, V <: Term](dfn : Defn, tail: Context[U, V], dep: Boolean = false) extends Context[FuncTerm[Term,U], V]{
    type PtnType = FuncTerm[Term, tail.PtnType]
    
    def newtail(newtail: Context[Term, V]) = DefnMixin(dfn, newtail, dep)
    
    def foldin : Typ[V] => PtnType => V = (tp) => (f) => tail.foldin(tp)(f(dfn.lhs))
    
    def symblist(tp : Typ[V])(varnames: List[AnySym]) = apply(tp).symbObj(varnames.head) :: tail.symblist(tp)(varnames.tail)
    
    def constants : Seq[Term] = tail.constants
    
    def defns : Seq[Defn] = dfn +: tail.defns
    
    def defnEqualities : Seq[DefnEquality] = dfn +: tail.defnEqualities
    
    def eliminate(isVar : Term => Boolean): Term => Term = tail.eliminate(isVar)
    
    val elim  =  tail.elim andThen ((t : Term) => t.subs(dfn.lhs, dfn.rhs))
    
    type ArgType = (Term, tail.ArgType)
    
    val instantiate : ArgType => Term => Term = (fstrest) => (t) => {
      val rest: tail.ArgType = fstrest._2
      val fst = fstrest._1
      (tail.instantiate(rest)(t)).subs(dfn.lhs, dfn.rhs)
    }
    								
    /*
    val typ = if (dep) {
      val fibre = (t : Term) => tail.typ subs (x, t)
	    
	    val family = typFamilyDefn[Term, tail.PtnType](dfn.lhs.typ, MiniVerse(tail.typ), fibre)
	    PiTyp[Term, tail.PtnType](family)
    }
    else FuncTyp(dfn.lhs.typ, tail.typ)
    */
    
    def apply(tp : Typ[V]) : Typ[PtnType] = if (dep) {
      val fibre = (t : Term) => tail(tp) subs (dfn.lhs, t)
	    
	    val family = typFamilyDefn[Term, tail.PtnType](dfn.lhs.typ, MiniVerse(tail(tp)), fibre)
	    PiTyp[Term, tail.PtnType](family)
    }
    else FuncTyp(dfn.lhs.typ, tail(tp))
  } 
  
  case class GlobalDefnMixin[+U <: Term, V <: Term](dfn : Defn, tail: Context[U, V]) extends Context[U, V]{
    type PtnType = tail.PtnType
    
    def newtail(newtail: Context[Term, V]) = GlobalDefnMixin(dfn, newtail)
    
    def foldin : Typ[V] => PtnType => V = tail.foldin
    
    def symblist(tp : Typ[V])(varnames: List[AnySym]) = tail.symblist(tp)(varnames)
    
    def constants : Seq[Term] = tail.constants
    
    def defns : Seq[Defn] = dfn +: tail.defns
    
    def defnEqualities : Seq[DefnEquality] = dfn +: tail.defnEqualities
    
    def eliminate(isVar : Term => Boolean): Term => Term = tail.eliminate(isVar)
    
    val elim  = tail.elim
    
    type ArgType = tail.ArgType
    
    val instantiate : ArgType => Term => Term = _ => (t) => t
    
//    val typ = tail.typ		
    
    def apply(tp : Typ[V]) : Typ[PtnType] = tail(tp)
  }
  
  case class DefnEqualityMixin[+U <: Term, V <: Term](eqlty : DefnEquality, tail: Context[U, V]) extends Context[U, V]{
    type PtnType = tail.PtnType
    
    def newtail(newtail: Context[Term, V]) = DefnEqualityMixin(eqlty, newtail)
    
    def apply(tp : Typ[V]) : Typ[PtnType] = tail(tp)
    
    def foldin : Typ[V] => PtnType => V = tail.foldin
    
    def symblist(tp: Typ[V])(varnames: List[AnySym]) = tail.symblist(tp)(varnames)
    
    def constants : Seq[Term] = tail.constants
    
    def defns : Seq[Defn] = tail.defns
    
    def defnEqualities : Seq[DefnEquality] = eqlty +: tail.defnEqualities
    
    def eliminate(isVar : Term => Boolean): Term => Term = tail.eliminate(isVar)
    
    val elim  = tail.elim
    
    type ArgType = tail.ArgType
    
    val instantiate : ArgType => Term => Term = _ => (t) => t
//    val typ = tail.typ								
  }
  
  case class SimpEqualityMixin[+U <: Term, V <: Term](eqlty : DefnEquality, tail: Context[U, V], dep : Boolean = false) extends Context[FuncTerm[Term,U], V]{
    type PtnType = FuncTerm[Term, tail.PtnType]
    
    def newtail(newtail: Context[Term, V]) = SimpEqualityMixin(eqlty, newtail, dep)
    
    def foldin : Typ[V] => PtnType => V = (tp) => (f) => tail.foldin(tp)(f(eqlty.lhs))
    
    def symblist(tp : Typ[V])(varnames: List[AnySym]) = apply(tp).symbObj(varnames.head) :: tail.symblist(tp)(varnames.tail)
    
    def constants : Seq[Term] = tail.constants
    
    def defns : Seq[Defn] = tail.defns
    
    def defnEqualities : Seq[DefnEquality] = eqlty +: tail.defnEqualities
    
    def eliminate(isVar : Term => Boolean): Term => Term = tail.eliminate(isVar)
    
    val elim  =  tail.elim andThen ((t : Term) => t.subs(eqlty.lhs, eqlty.rhs))
    
    type ArgType = (Term, tail.ArgType)
    
    val instantiate : ArgType => Term => Term = (fstrest) => (t) => {
      val rest: tail.ArgType = fstrest._2
      val fst = fstrest._1
      (tail.instantiate(rest)(t)).subs(eqlty.lhs, eqlty.rhs)
    }
    
    /*
    val typ = if (dep) {
      val fibre = (t : Term) => tail.typ subs (x, t)
	    
	    val family = typFamilyDefn[Term, tail.PtnType](eqlty.lhs.typ, MiniVerse(tail.typ), fibre)
	    PiTyp(family)
    }
    else FuncTyp(eqlty.lhs.typ, tail.typ)
    */
    
    def apply(tp : Typ[V]) : Typ[PtnType] = if (dep) {
      val fibre = (t : Term) => tail(tp) subs (eqlty.lhs, t)
	    
	    val family = typFamilyDefn[Term, tail.PtnType](eqlty.lhs.typ, MiniVerse(tail(tp)), fibre)
	    PiTyp(family)
    }
    else FuncTyp(eqlty.lhs.typ, tail(tp))
  }
  
  
  def extract[V <: Term](inner: Term, ctx: Context[Term, V], maps: PartialFunction[Term, Term]) : Option[Term] = ctx match {
    case _ : Context.empty[_] => Some(inner)
    case LambdaMixin(x, tail, _) => for(y <-maps.lift(x); t <- extract(inner.subs(x,y), tail, maps)) yield t
    case SimpEqualityMixin(eql, tail, _) => extract(inner, tail, Map(eql.lhs -> eql.rhs) orElse maps)
    case comp => extract(inner, comp.tail, maps)
  }

  def immerse[V <: Term](inner: Context[Term, V]): Context[Term, V] => Context[Term, V] ={
    case _ : Context.empty[_] => inner
    case outer => inner.newtail(immerse(inner)(outer.tail))
  }
  
  
}

