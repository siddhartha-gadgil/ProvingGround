package provingground.fol

import provingground.fol.Logic._
import scala.util._
import provingground.dynamics.Structures._
import scala.Option.option2Iterable

/** A basic resolution theorem prover */
object Resolution {

  case class SplitList[A](head: List[A], cursor: A, tail: List[A])

  def splitList[A](l: List[A]): List[SplitList[A]] = l match {
    case List()  => List()
    case List(a) => List(SplitList(List(), a, List()))
    case (a) :: (l: List[A]) =>
      val tailList = for (sl <- splitList(l))
        yield (SplitList(a :: sl.head, sl.cursor, sl.tail))
      SplitList(List.empty, a, l) :: tailList
  }

  def subTerms(t: Term): List[Term] = t match {
    case RecTerm(f, params) => params flatMap (subTerms(_))
    case _                  => List(t)
  }

  case class SubTermSubsTerm(term: Term, onSubs: Term => Term) {
    def apply(t: Term) = onSubs(t)
    def subs(m: PartialFunction[Var, Term]) =
      SubTermSubsTerm(term subs (m), (t: Term) => onSubs(t).subs(m))
  }

  case class SubTermSubsLit(term: Term, onSubs: Term => Literal) {
    def apply(t: Term) = onSubs(t)
    def subs(m: PartialFunction[Var, Term]) =
      SubTermSubsLit(term subs (m), (t: Term) => onSubs(t).subs(m))
    def unify(t: Term)(s: Term) = mgu(t, term) map (subs(_)(s))
  }

  def subTermSubsRec(t: Term): List[SubTermSubsTerm] = t match {
    case RecTerm(f, params) => params flatMap (subTermSubsRec(_))
    case s: Term            => List(SubTermSubsTerm(s, (p: Term) => p))
  }

  def subTermSubs(t: Term) =
    SubTermSubsTerm(t, (p: Term) => p) :: subTermSubsRec(t)

  def subTermSubsLit(l: Literal): List[SubTermSubsLit] = {
    for (SplitList(head, t, tail) <- splitList(l.p.params);
         st                       <- subTermSubs(t))
      yield
        SubTermSubsLit(st.term,
                       (s: Term) => l(l.p.pred(head ::: List(st(t)) ::: tail)))
  }

  def paramodSet(eql: Literal, l: Literal): Set[Literal] = eql.p match {
    case AtomFormula(BinRel("="), List(s: Term, t: Term)) =>
      (for (st <- subTermSubsLit(l); lit <- st.unify(t)(s)) yield (lit)).toSet
    case Eq(s, t) =>
      (for (st <- subTermSubsLit(l); lit <- st.unify(t)(s)) yield (lit)).toSet
    case _ => Set.empty
  }

  /* def optionUnion[T](xs: List[Option[T]]) : Option[List[T]] = if (xs.isEmpty) Some(List()) else {
		val rest = optionUnion(xs.tail)
		for (x<- xs.head; ys <- rest) yield x::ys 						
	}*/

//	def optionSet[T](x: Option[T]): Set[T] = (for (a <- x) yield a).toSet

  def notOccurs(x: Var, t: Term) = !(subTerms(t) contains x) && (x != t)

  def mgu(s: Term, t: Term): Option[Map[Var, Term]] = (s, t) match {
    case (RecTerm(f, paramf), RecTerm(g, paramg)) if (f == g) =>
      mguList(paramf, paramg)
    case (x: Var, t: Term) if notOccurs(x, t) => Some(Map(x -> t))
    case (t: Term, x: Var) if notOccurs(x, t) => Some(Map(x -> t))
    case _                                    => None
  }

  def intersection[A](a: Set[A], b: Set[A]) = {
    for (x <- a; y <- b if x == y) yield x
  }

  def mguList(ss: List[Term], ts: List[Term]): Option[Map[Var, Term]] =
    if (ss.size != ts.size) None
    else
      ss match {
        case List()        => Some(Map.empty)
        case List(s: Term) => mgu(s, ts.head)
        case shead :: stail =>
          for (tailMap <- mguList(stail, ts.tail);
               headMap <- mgu(shead subs tailMap, ts.head.subs(tailMap))
               if (intersection(tailMap.keySet, headMap.keySet).isEmpty))
            yield (tailMap ++ headMap)
        case _ => None
      }

  trait Literal {
    val p: AtomicFormula
    def apply(p: AtomicFormula): Literal
    def subs(m: PartialFunction[Var, Term]): Literal
  }

  case class PosLit(p: AtomicFormula) extends Literal {
    def apply(p: AtomicFormula): Literal = PosLit(p)
    def subs(m: PartialFunction[Var, Term]): Literal =
      PosLit(p.subs(m).asInstanceOf[AtomicFormula])
  }

  case class NegLit(p: AtomicFormula) extends Literal {
    def apply(p: AtomicFormula): Literal = NegLit(p)
    def subs(m: PartialFunction[Var, Term]): Literal =
      NegLit(p.subs(m).asInstanceOf[AtomicFormula])
  }

  def oppSnse(a: Literal, b: Literal): Boolean = (a, b) match {
    case (PosLit(x), NegLit(y)) => true
    case (NegLit(x), PosLit(y)) => true
    case _                      => false
  }

  def mguFmla(f: AtomicFormula,
              g: AtomicFormula): Option[PartialFunction[Var, Term]] = {
    if (f.pred != g.pred) None
    else mguList(f.params, g.params)
  }

  case class Clause(ls: Set[Literal]) {
    def |(that: Clause) = Clause(this.ls ++ that.ls)
    def subs(m: PartialFunction[Var, Term]) =
      Clause(ls map ((l: Literal) => l.subs(m)))
  }

  case class CNF(clauses: Set[Clause]) {
    def &(that: CNF) = CNF(this.clauses ++ that.clauses)

    def |(that: CNF) = {
      CNF(for (a <- this.clauses; b <- that.clauses) yield (a | b))
    }

    def subs(m: PartialFunction[Var, Term]) =
      CNF(clauses map ((l: Clause) => l.subs(m)))
  }

  case class SplitClause(one: Literal, rest: Set[Literal])

  def splitClause(c: Clause) =
    c.ls map ((l: Literal) => SplitClause(l, c.ls - l))

  def idVar: PartialFunction[Var, Term] = { case x: Var => x }

  def unify(a: SplitClause, b: SplitClause) = {
    mguFmla(a.one.p, b.one.p) map
      ((f: PartialFunction[Var, Term]) =>
         (a.rest map (_.subs(f))) union (b.rest map (_.subs(f))))
  }

  def newResolutionClauses(c: CNF) = {
    for (x <- c.clauses; y <- c.clauses; a <- splitClause(x);
         b <- splitClause(y) if oppSnse(a.one, b.one)) yield unify(a, b)
  }

  def negate(fmla: Formula): Formula = fmla match {
    case p: AtomicFormula         => NegFormula(p)
    case NegFormula(p)            => p
    case ConjFormula(p, "&", q)   => negate(p) | negate(q)
    case ConjFormula(p, "|", q)   => negate(p) & negate(q)
    case ConjFormula(p, "=>", q)  => p & negate(q)
    case ConjFormula(p, "<=>", q) => (p & negate(q)) | (q & negate(p))
    case ExQuantFormula(x, p)     => UnivQuantFormula(x, negate(p))
    case UnivQuantFormula(x, p)   => ExQuantFormula(x, negate(p))
  }

  class Skolem(x: Var, d: Int) extends Func(d)

  def cnf(fmla: Formula, outerVars: List[Var]): CNF = fmla match {
    case p: AtomicFormula        => CNF(Set(Clause(Set(PosLit(p)))))
    case ConjFormula(p, "&", q)  => cnf(p, outerVars) & cnf(q, outerVars)
    case ConjFormula(p, "|", q)  => cnf(p, outerVars) | cnf(q, outerVars)
    case ConjFormula(p, "=>", q) => cnf(!p, outerVars) | cnf(q, outerVars)
    case ConjFormula(p, "<=>", q) =>
      (cnf(p, outerVars) & cnf(q, outerVars)) |
        (cnf(!p, outerVars) & cnf(!q, outerVars))
    case NegFormula(NegFormula(p))    => cnf(p, outerVars)
    case NegFormula(p: AtomicFormula) => CNF(Set(Clause(Set(NegLit(p)))))
    case NegFormula(p: Formula)       => cnf(negate(p), outerVars)

    case UnivQuantFormula(x: Var, p) => cnf(p, x :: outerVars)
    case ExQuantFormula(x, p) =>
      val vars = outerVars
      val t    = new Skolem(x, vars.length)(vars)
      cnf(p, outerVars) subs (Map(x -> t) orElse idVar)
  }

  def cnf(fmla: Formula): CNF = cnf(fmla, fmla.freeVars.toList)

  def paraSubs(e: Formula, t: Term): Option[Term] = e match {
    case AtomFormula(BinRel("="), List(a, b)) =>
      mgu(b, t) map ((m: PartialFunction[Var, Term]) => a subs m)
    case Eq(a, b) =>
      mgu(b, t) map ((m: PartialFunction[Var, Term]) => a subs m)
    case _ => None
  }

  def paramodList(e: Formula, ts: List[Term]): Set[List[Term]] =
    if (ts.isEmpty) Set()
    else {
      val fromRest =
        paramodList(e, ts.tail) map ((xs: List[Term]) => ts.head :: xs)
      fromRest union (paramodTerm(e, ts.head) map (_ :: ts.tail))
    }

  def paramodTerm(e: Formula, t: Term): Set[Term] = t match {
    case x: Var => Set()
    case RecTerm(f, ts) =>
      (paramodList(e, ts) map (RecTerm(f, _)): Set[Term]) union
        (paraSubs(e, RecTerm(f, ts)).toSet)
    case x: Term => paraSubs(e, x).toSet
  }

  def paraModulation(e: Formula, l: Literal): Set[Literal] = {
    paramodList(e, l.p.params) map ((ts: List[Term]) => l(l.p.pred(ts)))
  }
}
