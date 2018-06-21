package provingground.learning
import provingground._, HoTT._

import spire.algebra._
import spire.implicits._

import provingground.{FiniteDistribution => FD, ProbabilityDistribution => PD}

import monix.eval._

/**
 * Adding equations from a simple generative model to [[EvolverEquations]]
 */
abstract class TermLearner[F: Field] extends EvolverEquations[F]{
  def unApp: F

  def appl: F

  def lambdaWeight: F

  def piWeight: F

  def varWeight: F

  def applInv(term: Term, context: Vector[Term]): Set[(ExstFunc, Term)]

  def unAppInv(term: Term, context: Vector[Term]): Set[(ExstFunc, Term)]

  def initContextProb(t: Term, context: Vector[Term]): F =
    context match {
      case Vector() => initProb(t)
      case init :+ last =>
        if (t == last) varWeight
        else initContextProb(t, init) * (1 - varWeight)
    }

  def fromAppl(t: Term, context: Vector[Term]) : F =
    applInv(t, context).map{
      case (f, x) =>
      (finalProb(f.term, context)/isFuncP(context)) *
      (finalProb(x, context)/hasTyp(f.dom, context) ) *
      appl
    }.fold[F](field.zero)(_ + _)

  def fromUnApp(t: Term, context: Vector[Term]) : F =
    unAppInv(t, context).map{
      case (f, x) =>
      (finalProb(f.term, context)/isFuncP(context)) *
      finalProb(x, context) *
      unApp
    }.fold[F](field.zero)(_ + _)

  def fromIsland(t: Term, context: Vector[Term]) : F =
    t match {
      case l : LambdaTerm[u, v] =>
        finalProb(l.value, context :+ l.variable) *
        finalProb(l.variable.typ, context) / isTypP(context)
      case l : PiDefn[u, v] =>
        (finalProb(l.value, context :+ l.variable)/isTypP(context :+ l.variable) ) *
        finalProb(l.variable.typ, context) / isTypP(context)
      case _ => field.zero
    }

  def recValue(t: Term, context: Vector[Term]) : F =
    initContextProb(t, context) + fromAppl(t, context) + fromUnApp(t, context) + fromIsland(t, context)

}
