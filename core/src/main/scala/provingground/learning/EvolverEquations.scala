package provingground.learning
import provingground._, HoTT._

import spire.algebra._
import spire.implicits._

import provingground.{FiniteDistribution => FD, ProbabilityDistribution => PD}

object EvolverEquations {
  def termsInContext(terms: Set[Term], context: Vector[Term]): Set[Term] =
    context match {
      case Vector() => terms
      case init :+ last =>
        termsInContext(terms, init).collect {
          case l: LambdaTerm[u, v] if l.variable.typ == last.typ =>
            l.value.replace(l.variable, last)
          case pd: PiDefn[u, v] if pd.variable.typ == last.typ =>
            pd.value.replace(pd.variable, last)
        }
    }

  def typsInContext(typs: Set[Typ[Term]],
                    context: Vector[Term]): Set[Typ[Term]] =
    context match {
      case Vector() => typs
      case init :+ last =>
        typsInContext(typs, init).collect {
          case pd: PiDefn[u, v] if pd.variable.typ == last.typ =>
            pd.value.replace(pd.variable, last)
        }
    }

  def contextsOfTerm(term: Term): Vector[Vector[Term]] =
    term match {
      case l: LambdaTerm[u, v] =>
        val tail = contextsOfTerm(l.value)
        Vector(l.variable) +: tail.map { (ctx) =>
          l.variable +: ctx
        }
      case l: PiDefn[u, v] =>
        val tail = contextsOfTerm(l.value)
        Vector(l.variable) +: tail.map { (ctx) =>
          l.variable +: ctx
        }
      case t => Vector()
    }

  def equalContexts(ctx1: Vector[Term], ctx2: Vector[Term]): Boolean =
    (ctx1.size == ctx2.size) && {
      (ctx1, ctx2) match {
        case (Vector(), Vector()) => true
        case (head1 +: tail1, head2 +: tail2) =>
          (head1.typ == head2.typ) &&
            equalContexts(tail1, tail2.map(_.replace(head2, head1)))
        case _ => false
      }
    }

  def appendContext(ctxs: Vector[Vector[Term]], context: Vector[Term]) =
    if (ctxs.exists(equalContexts(_, context))) ctxs else ctxs :+ context
}

import EvolverEquations._

/**
  * variables for probabilities and equations for consistency
  */
abstract class EvolverEquations[F](implicit field: Field[F]) {
  val termSet: Set[Term]

  val typSet: Set[Typ[Term]]

  lazy val allContexts: Vector[Vector[Term]] =
    termSet.toVector
      .flatMap(contextsOfTerm)
      .foldLeft[Vector[Vector[Term]]](Vector())(appendContext(_, _))

  def termSetInContext(context: Vector[Term]) = termsInContext(termSet, context)

  def typSetInContext(context: Vector[Term]) = typsInContext(typSet, context)

  /**
    * probability of `t` in the initial distribution
    */
  def initProb(t: Term): F

  /**
    * probability of `t` in the final distribution given a context
    */
  def finalProb(t: Term, context: Vector[Term]): F

  /**
    * probability that a term has given type in the final distribution of a context
    */
  def hasTyp(typ: Typ[Term], context: Vector[Term]): F

  /**
    * probability that a term is a function in the final distribution of a context
    */
  def isFuncP(context: Vector[Term]): F

  def unApp: F

  def appl: F

  def lambdaWeight: F

  def piWeight: F

  def varWeight: F

  def initContextProb(t: Term, context: Vector[Term]): F =
    context match {
      case Vector() => initProb(t)
      case init :+ last =>
        if (t == last) varWeight
        else initContextProb(t, init) * (1 - varWeight)
    }

  def totFinalProb(terms: Set[Term], context: Vector[Term]) =
    terms.map(finalProb(_, context)).foldRight[F](field.zero)(_ + _)

  def totProbOne(context: Vector[Term]): F =
    totFinalProb(termSetInContext(context), context) - 1

  def hasTypProb(typ: Typ[Term], context: Vector[Term]): (F, F) =
    totFinalProb(termSetInContext(context).filter(_.typ == typ), context) -> hasTyp(
      typ,
      context)

  def isFuncProb(context: Vector[Term]): (F, F) =
    totFinalProb(termSetInContext(context).filter((t) => isFunc(t)), context) -> isFuncP(
      context)

}
