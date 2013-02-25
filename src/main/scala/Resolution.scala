package provingGround

import provingGround.Logic._

import provingGround.Structures._

/** A basic resolution theorem prover */
object Resolution{
	def subTerms(t: Term): List[Term] = t match {
		case RecTerm(f, params) => params flatMap (subTerms(_))
		case _ => List(t)
	}


	def optionUnion[T](xs: List[Option[T]]) : Option[List[T]] = if (xs.isEmpty) Some(List()) else {
		val rest = optionUnion(xs.tail)
		for (x<- xs.head; ys <- rest) yield x::ys 						
	}
	
	def optionSet[T](x: Option[T]): Set[T] = (for (a <- x) yield a).toSet

	def notOccurs(x: Var, t: Term) = !(subTerms(t) contains x)

	def mgu(s: Term, t: Term): Option[Map[Var, Term]] = (s, t) match {
		case (RecTerm(f, paramf), RecTerm(g, paramg)) if (f == g) =>
			optionUnion((paramf, paramg).zipped.map(mgu)) map ((l: List[Map[Var, Term]]) => l reduce (_ ++ _)) 
		case (x: Var, t: Term) if notOccurs(x, t) => Some(Map(x -> t))
		case (t: Term, x: Var) if notOccurs(x, t) => Some(Map(x -> t))
		case _ => None 
	}

	trait Literal{
	  val p: AtomFormula
	  def apply(p: AtomFormula): Literal
	}

	case class PosLit(p: AtomFormula) extends Literal{
	  def apply(p: AtomFormula): Literal = PosLit(p)
	}

	case class NegLit(p: AtomFormula) extends Literal{
	  def apply(p: AtomFormula): Literal = NegLit(p)
	}

	def mguFmla(f: AtomFormula, g: AtomFormula): Option[Map[Var, Term]] = {
		if (f.p != g.p) None else {
			val optU = optionUnion((f.params, g.params).zipped.map(mgu))
			optU map ((l: List[Map[Var, Term]]) => l reduce (_ ++ _))
		}
	}

	case class Clause(ls: Set[Literal]){
	  def |(that: Clause) = Clause(this.ls ++ that.ls)
	}
	
	case class CNF(clauses: Set[Clause]){
	  def &(that: CNF) = CNF(this.clauses ++ that.clauses)
	  
	  def |(that: CNF) ={
	    CNF(for (a <- this.clauses; b <- that.clauses) yield (a | b))
	  }
	}

	case class SplitClause(one: Literal, rest: Set[Literal])

	def splitClause(c: Clause) = c.ls map ((l: Literal) => SplitClause(l, c.ls - l))

	def idVar: PartialFunction[Var, Term] = {case x: Var => x}
	
	def subs(f: Map[Var,Term], l: Literal) = l match {
	  case PosLit(p) => PosLit((p subs (f orElse idVar)).asInstanceOf[AtomFormula])
	  case NegLit(p) => NegLit((p subs (f orElse idVar)).asInstanceOf[AtomFormula])
	}

	  
	def unify(a: SplitClause, b: SplitClause) = {
	  mguFmla(a.one.p, b.one.p) map ((f: Map[Var, Term]) => (a.rest map (subs(f, _))) union (b.rest map (subs(f, _))))
	}
	
	def newClauses(c: CNF) = {
	  for (x <- c.clauses; y <- c.clauses; a <- splitClause(x); b <- splitClause(y)) yield unify(a,b)
	}

  case class Skolem(x: Var, d: Int) extends Func(d)

  def cnf(fmla: Formula) : CNF = fmla match {
    case p: AtomFormula => CNF(Set(Clause(Set(PosLit(p)))))
    case p: Eq => CNF(Set(Clause(Set(PosLit(p)))))
    case ConjFormula(p, "&", q) => cnf(p) & cnf(q)
    case ConjFormula(p, "|", q) => cnf(p) | cnf(q)
    case ConjFormula(p, "=>", q) => cnf(!p) | cnf(q)
    case ConjFormula(p, "<=>", q) => (cnf(p) & cnf(q)) | (cnf(!p) & cnf(!q))
    case NegFormula(NegFormula(p)) => cnf(p)
    case NegFormula(p: AtomFormula) => CNF(Set(Clause(Set(NegLit(p))))) 
    case UnivQuantFormula(_, p) => cnf(p)
    case ExQuantFormula(x, p) => 
      val vars = p.freeVars.toList
      val t = Skolem(x, vars.length)(vars)
      val skolemP = p subs (Map(x->t) orElse idVar)
      cnf(skolemP)
  }
  
  def paraSubs(e: Formula, t: Term): Option[Term] = e match{
    case AtomFormula(BinRel("="), List(a, b)) =>
      mgu(b, t) map ((m: Map[Var, Term]) => a subs (m orElse idVar))
    case _ => None
  }
  
  def paramodList(e: Formula, ts: List[Term]): Set[List[Term]] = if (ts.isEmpty) Set() else {
    val fromRest = paramodList(e, ts.tail) map ((xs: List[Term]) => ts.head :: xs) 
    fromRest union (paramodTerm(e, ts.head) map (_ :: ts.tail))
  }
  
  def paramodTerm(e: Formula, t: Term): Set[Term] = t match {
    case x: Var => Set()
    case RecTerm(f, ts) => 
      (paramodList(e, ts) map (RecTerm(f, _)): Set[Term]) union optionSet(paraSubs(e, RecTerm(f, ts)))
    case x: Term => optionSet(paraSubs(e, x))
  }
  
  def paraModulation(e: Formula, l: Literal): Set[Literal] = {
    paramodList(e, l.p.params) map ((ts: List[Term]) => l(l.p.p(ts)))
  }
}



