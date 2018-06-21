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

  def termOptContext(term: Term, context: Vector[Term]): Option[Term] =
    context match {
      case Vector() => Some(term)
      case init :+ last =>
        termOptContext(term, init).collect {
          case l: LambdaTerm[u, v] if l.variable.typ == last.typ =>
            l.value.replace(l.variable, last)
          case pd: PiDefn[u, v] if pd.variable.typ == last.typ =>
            pd.value.replace(pd.variable, last)
        }
    }

  @annotation.tailrec
  def termInSomeContext(term: Term,
                        contexts: Vector[Vector[Term]],
                        accum: Option[(Term, Vector[Term])] = None)
    : Option[(Term, Vector[Term])] =
    if (accum.nonEmpty) accum
    else
      contexts match {
        case Vector() => None
        case head +: tail =>
          termInSomeContext(term,
                            tail,
                            termOptContext(term, head).map((t) => t -> head))
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

  /**
    * optionally project a term in the first context onto the second context
    */
  def projectContext(ctx1: Vector[Term], ctx2: Vector[Term])(
      term: Term): Option[Term] = {
    (ctx1, ctx2) match {
      case (Vector(), Vector()) => Some(term)
      case (head1 +: tail1, head2 +: tail2) =>
        projectContext(tail1.map(_.replace(head1, head2)), tail2)(
          term.replace(head1, head2))
      case _ => None
    }
  }

  def projectSomeContext(ctxs: Vector[Vector[Term]])(
      term: Term,
      context: Vector[Term]): Option[(Term, Vector[Term])] =
    ctxs.find(equalContexts(_, context)).flatMap { (ctx2) =>
      projectContext(context, ctx2)(term).map((t) => (t, ctx2))
    }
}

import EvolverEquations._

/**
 * Support for evolver
 */
trait EvolverSupport{
  val termSet: Set[Term]

  val typSet: Set[Typ[Term]]

  lazy val baseContexts: Vector[Vector[Term]] =
    termSet.toVector
      .flatMap(contextsOfTerm)
      .foldLeft[Vector[Vector[Term]]](Vector())(appendContext(_, _))

  def inBaseContext(term: Term, context: Vector[Term]) =
    projectSomeContext(baseContexts)(term, context)

  def termSetInContext(context: Vector[Term]) = termsInContext(termSet, context)

  def typSetInContext(context: Vector[Term]) = typsInContext(typSet, context)

  lazy val contextTermSet : Vector[(Term, Vector[Term])] =
    baseContexts.flatMap{
      (ctx) => termSetInContext(ctx).toVector.map((t) => t -> ctx)
    }

}

/**
  * variables for probabilities and equations for consistency
  */
abstract class EvolverEquations[F](implicit val field: Field[F]) extends EvolverSupport {

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

  def isTypP(context: Vector[Term]): F

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

  def isTypProb(context: Vector[Term]): (F, F) =
    totFinalProb(termSetInContext(context).filter((t) => isTyp(t)), context) -> isTypP(
      context)

}
