package provingground.fol

import scala.util._
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

  def subTermSubsLit(l: Literal): List[SubTermSubsLit] = l.p match {
    case af: AtomicFormula =>
      for (SplitList(head, t, tail) <- splitList(af.params);
           st                       <- subTermSubs(t))
        yield
          SubTermSubsLit(
            st.term,
            (s: Term) =>
              l.replaceFormula(af.pred(head ::: List(st(t)) ::: tail))
          )
    case _ => List()
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
    case _ => None
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
               if (tailMap.keySet.intersect(headMap.keySet).isEmpty))
            yield (tailMap ++ headMap)
        case _ => None
      }

  def mguFmla(
      x: Formula,
      y: Formula
  ): Option[PartialFunction[Var, Term]] = (x, y) match {
    case (f: AtomicFormula, g: AtomicFormula) =>
      if (f.pred != g.pred) None
      else mguList(f.params, g.params)
    case _ => None
  }

  case class SplitClause(one: Literal, rest: Set[Literal])

  def splitClause(c: Clause) =
    c.ls map ((l: Literal) => SplitClause(l, c.ls - l))

  def unify(a: SplitClause, b: SplitClause) = {
    mguFmla(a.one.p, b.one.p) map
      (
          (f: PartialFunction[Var, Term]) =>
            (a.rest map (_.subs(f))) union (b.rest map (_.subs(f)))
        )
  }

  def newResolutionClauses(c: CNF) = {
    for (x <- c.clauses; y <- c.clauses; a <- splitClause(x);
         b <- splitClause(y) if a.one.isPositive != b.one.isPositive)
      yield unify(a, b)
  }

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

  def paraModulation(e: Formula, l: Literal): Set[Literal] = l.p match {
    case af: AtomicFormula => paramodList(e, af.params) map (
        (ts: List[Term]) => l.replaceFormula(af.pred(ts))
    )
    case _ => Set()
  }
}
