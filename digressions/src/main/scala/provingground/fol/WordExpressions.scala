package provingground.fol

import provingground.fol.Logic._
import provingground.dynamics.Structures._
import provingground.fol.Theory._

/** Extends the class of terms and formulas to include various generic and specific classes of words, lists etc.
		*/
object WordExpressions {

  case class Word(ts: List[Term]) extends Term {
    def subs(xt: Var => Term) = Word(ts map (_.subs(xt)))
    val freeVars              = (ts map (_.freeVars)) reduce (_ union _)
  }

  case class Sequence(ts: Stream[Term]) extends Term {
    def subs(xt: Var => Term) = Sequence(ts map (_.subs(xt)))
    val freeVars =
      if (ts.hasDefiniteSize) (ts map (_.freeVars) reduce (_ union _))
      else (Set.empty: Set[Var])
  }

  case class RepSeq(term: Term, n: Term) extends Term {
    def subs(xt: Var => Term) = RepSeq(term subs xt, n)
    val freeVars              = term.freeVars
    def this(term: Term) = this(term, new Var)
  }

  case class FoldSeq(r: RepSeq, b: BinOp) extends Term {
    def subs(xt: Var => Term) = FoldSeq(r subs xt, b)
    val freeVars              = r.freeVars
  }

  class VarSeq {
    val stream: Stream[Var] = (Stream.from(0)) map (_ => new Var)
    def apply(n: Int)       = stream(n)
    def take(n: Int)        = stream.take(n).toList
    def apply(n: Term)      = VarIndx(this, n)
  }

  case class VarSeqSym(name: String) extends VarSeq

  case class VarIndx(xs: VarSeq, index: Term) extends Term {
    def subs(xt: Var => Term) = VarIndx(xs, index subs xt)
    val freeVars              = index.freeVars
  }

  case class OneOf(ts: Set[Term]) extends Term {
    def subs(xt: Var => Term) = OneOf(ts map (_.subs(xt)))
    val freeVars              = (ts map (_.freeVars)) reduce (_ union _)
  }

  private def subsWordFirst[A](l: List[A],
                               first: List[A],
                               second: List[A]): Set[List[A]] = {
    val n = first.length
    if (l.take(n) == first) Set((second ::: (l drop n))) else Set.empty
  }

  def subsWord[A](l: List[A], first: List[A], second: List[A]): Set[List[A]] = {
    val tailSet: Set[List[A]] =
      if (l.isEmpty) Set() else subsWord(l.tail, first, second)
    subsWordFirst(l, first, second) union tailSet
  }

  def subsWordSet[A](l: List[A], subsSet: Set[(List[A], List[A])]) = {
    val setSet = for ((a, b) <- subsSet) yield (subsWord(l, a, b))
    setSet reduce (_ union _)
  }

  def cancellationSet(genSet: Set[Term])(implicit u: UnOp) = {
    (for (gen <- genSet)
      yield
        (
          Set((List(gen, u(gen)), List.empty[Term]),
              (List(u(gen), gen), List.empty[Term]))
        )) reduce (_ union _)
  }

  def inverseSet(genSet: Set[Term])(implicit u: UnOp) = {
    (for (gen <- genSet)
      yield
        (
          Set((List(gen, u(gen)), List.empty[Term]),
              (List(u(gen), gen), List.empty[Term]),
              (List.empty[Term], List(gen, u(gen))),
              (List.empty[Term], List(u(gen), gen)))
        )) reduce (_ union _)
  }

  def relationSet(rels: Set[List[Term]]) = {
    rels flatMap
      ((r: List[Term]) =>
        Set((r, List.empty: List[Term]), (List.empty: List[Term], r)))
  }

  def equalitySet(eqns: Set[(List[Term], List[Term])]) =
    eqns union symSet(eqns)
}
