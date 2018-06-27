package provingground.learning
import provingground._, HoTT._

import spire.algebra._
import spire.math._
import spire.implicits._

import provingground.{FiniteDistribution => FD, ProbabilityDistribution => PD}

object EvolverEquations {
  def klDiff[F: Field : Trig](p: F, q: F) =
    p * log(p/q)

  /**
    * terms of the form `x :-> y` or `x ~>: y` imported into a context of the form
    * `x' :-> _`  as `x' :-> y'` if `x` and `x'` have the same type;
    * this should be done recursively
    * as in a context `x:-> w :-> _`, the type of `w` may depend on `x`.
    */
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

  /**
    * if term is of the form `x :-> y`, it is imported into a context of the form
    * `x' :-> _`  as `x' :-> y'` if `x` and `x'` have the same type;
    * this should be done recursively
    * as in a context `x:-> w :-> _`, the type of `w` may depend on `x`.
    */
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

  /**
    * import term into some context in a vector of contexts if it matches.
    */
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

  /**
    * typs of the form `x ~>: y` imported into a context of the form
    * `x' :-> _`  as `x' ~>: y'` if `x` and `x'` have the same type;
    * this should be done recursively
    * as in a context `x:-> w :-> _`, the type of `w` may depend on `x`.
    */
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

  /**
    * all the contexts in which a term lies, including the empty one.
    */
  def contextsOfTerm(term: Term): Vector[Vector[Term]] =
    term match {
      case l: LambdaTerm[u, v] =>
        val tail = contextsOfTerm(l.value)
        Vector() +: tail.map { (ctx) =>
          l.variable +: ctx
        }
      case l: PiDefn[u, v] =>
        val tail = contextsOfTerm(l.value)
        Vector() +: tail.map { (ctx) =>
          l.variable +: ctx
        }
      case t => Vector(Vector())
    }

  /**
    * returns whether contexts are equal up to change of variables
    */
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

  /**
    * appends a context avoiding duplication up to change of variables
    */
  def appendContext(ctxs: Vector[Vector[Term]], context: Vector[Term]) =
    if (ctxs.exists(equalContexts(_, context))) ctxs else ctxs :+ context

  /**
    * optionally project a term in the first context onto the second context
    */
  def projectContext(fromCtx: Vector[Term], toCtx: Vector[Term])(
      term: Term): Option[Term] = {
    (fromCtx, toCtx) match {
      case (Vector(), Vector()) => Some(term)
      case (head1 +: tail1, head2 +: tail2) if (head1.typ == head2.typ) =>
        projectContext(tail1.map(_.replace(head1, head2)), tail2)(
          term.replace(head1, head2))
      case _ => None
    }
  }

  /**
    * optionally project a given term with a given context to one of a vector of
    * target contexts.
    *
    * @return optional pair  (term in new context, new context)
    */
  def projectSomeContext(toCtxs: Vector[Vector[Term]])(
      term: Term,
      fromCtx: Vector[Term]): Option[(Term, Vector[Term])] =
    toCtxs.find(equalContexts(_, fromCtx)).flatMap { (toCtx) =>
      projectContext(fromCtx, toCtx)(term).map((t) => (t, toCtx))
    }

  /**
    * sum of elements in a field
    */
  def sum[F](seq: Iterable[F])(implicit field: Field[F]) =
    seq.foldRight[F](field.zero)(_ + _)

}

import EvolverEquations._

/**
  * Support of the final distribution of an `evolver`, i.e., a generative model for terms
  */
trait EvolverSupport {

  /**
    * terms in the final distribution
    */
  val termSet: Set[Term]

  /**
    * types in the final distribution
    */
  val typSet: Set[Typ[Term]]

  /**
    * terms in the initial distribution
    */
  val genTermSet: Set[Term]

  /**
    * the set of theorems, i.e., types (in the final distribution) that are inhabited
    */
  lazy val thmSet: Set[Typ[Term]] =
    termSet.map(_.typ).intersect(typSet).filterNot(isUniv)

  /**
    * a basis for contexts, i.e., enumeration up to equality under change of variables
    */
  lazy val baseContexts: Vector[Vector[Term]] =
    termSet.toVector
      .flatMap(contextsOfTerm)
      .foldLeft[Vector[Vector[Term]]](Vector())(appendContext(_, _))

  /**
    * project term onto some sontext in the basis.
    *
    * @return optional pair  (term in basis context, basis context)
    */
  def inBaseContext(term: Term,
                    context: Vector[Term]): Option[(Term, Vector[Term])] =
    projectSomeContext(baseContexts)(term, context)

  def termSetInContext(context: Vector[Term]) = termsInContext(termSet, context)

  def typSetInContext(context: Vector[Term]) = typsInContext(typSet, context)

  /**
    * vector of terms in the basis contexts
    */
  lazy val contextTermVec: Vector[(Term, Vector[Term])] =
    baseContexts.flatMap { (ctx) =>
      termSetInContext(ctx).toVector.map((t) => t -> ctx)
    }

  import EvolverVariables._

  /**
    * vector of (formal) variables; assumes standard evolver for simplicity
    */
  lazy val variablesVector: Vector[EvolverVariables] =
    Vector(Appl, UnApp, LambdaWeight, PiWeight, VarWeight) ++
      genTermSet.toVector.map(InitProb(_)) ++
      contextTermVec.map { case (t, ctx)                  => FinalProb(t, ctx) } ++
      contextTermVec.collect { case (typ: Typ[Term], ctx) => HasTyp(typ, ctx) } ++
      baseContexts.flatMap((ctx) => Vector(IsFuncP(ctx), IsTypP(ctx)))

  /**
    * map from formal variable to its index
    */
  lazy val variableIndex: Map[EvolverVariables, Int] =
    variablesVector.zipWithIndex.toMap

// ------------------------------------------
// Code below is spire specific

  implicit val dim = JetDim(variablesVector.size)

  implicit val jetField = implicitly[Field[Jet[Double]]]

  /**
    * exponential multiplicative tangent jet of values of evolver variables
    */
  def spireProb(
      p: Map[EvolverVariables, Double]): Map[EvolverVariables, Jet[Double]] =
    variablesVector.zipWithIndex.map {
      case (v, n) =>
        v -> (p.getOrElse(v, 0.0) * exp(Jet.h[Double](n)))
    }.toMap

  /**
    * [[TermLearner]] for evolution with spire.
    */
  def spireLearner(p: Map[EvolverVariables, Double], apIn: ApplnInverse) =
    TermLearner(this, spireProb(p), apIn)

}

/**
  * inverses under application with an without unification
  */
trait ApplnInverse {
  def applInv(term: Term, context: Vector[Term]): Set[(ExstFunc, Term)]

  def unAppInv(term: Term, context: Vector[Term]): Set[(ExstFunc, Term)]
}

object ApplnInverse {

  /**
    * inverts only formal applications
    */
  case object Formal extends ApplnInverse {
    def applInv(term: Term, context: Vector[Term]) =
      (for {
        (f, x) <- FormalAppln.unapply(term)
        fn     <- ExstFunc.opt(f)
      } yield (fn, x)).toSet

    def unAppInv(term: Term, context: Vector[Term]) = applInv(term, context)
  }
}

/**
  * Variables in an evolver; not all need to be used in a give case
  */
sealed trait EvolverVariables

object EvolverVariables {

  /**
    * probability in the initial distribution
    */
  case class InitProb(term: Term) extends EvolverVariables

  /**
    * probability in the final distribution of a context
    * @param term term in the specific context
    * @param context the context
    */
  case class FinalProb(term: Term, context: Vector[Term])
      extends EvolverVariables

  /**
    * the probability of the event that a term in the
    * final distribution of a context has a specified type
    */
  case class HasTyp(typ: Typ[Term], context: Vector[Term])
      extends EvolverVariables

  /**
    * the probability that a term in the final distribution of a context is a function.
    */
  case class IsFuncP(context: Vector[Term]) extends EvolverVariables

  /**
    * the probability that a term in the final distribution of a context is a type.
    */
  case class IsTypP(context: Vector[Term]) extends EvolverVariables

  /**
    * weight of the initial distribution in the generative model
    */
  case object InitWeight extends EvolverVariables

  /**
    * weight of unified application in the generative model
    */
  case object UnApp extends EvolverVariables

  /**
    * weight of function application in the generative model
    */
  case object Appl extends EvolverVariables

  /**
    * weight of lambda-islands in the generative model
    */
  case object LambdaWeight extends EvolverVariables

  /**
    * weight of pi-islands in the generative model
    */
  case object PiWeight extends EvolverVariables

  /**
    * probability of the new variable in the initial distribution of an island.
    */
  case object VarWeight extends EvolverVariables
}

import EvolverVariables._

/**
  * variables for probabilities and equations for consistency
  */
class EvolverEquations[F](supp: EvolverSupport, prob: EvolverVariables => F)(
    implicit val field: Field[F],
    trig: Trig[F]) {

  import supp._

  /**
    * probability of `t` in the initial distribution
    */
  def initProb(t: Term): F = prob(InitProb(t))

  /**
    * probability of `t` in the final distribution given a context, this
    * gives the image of the variable obtained by normalizing to a basis context
    */
  def finalProb(t: Term, context: Vector[Term]): F =
    inBaseContext(t, context)
      .map {
        case (term, bc) => prob(FinalProb(term, bc))
      }
      .getOrElse(field.zero)

  /**
    * probability that a term has given type in the final distribution of a context
    */
  def hasTyp(typ: Typ[Term], context: Vector[Term]): F =
    prob(HasTyp(typ, context))

  /**
    * probability that a term is a function in the final distribution of a context
    */
  def isFuncP(context: Vector[Term]): F = prob(IsFuncP(context))

  /**
    * probability that a term is a type in the final distribution of a context
    */
  def isTypP(context: Vector[Term]): F = prob(IsTypP(context))

  /**
    * total final probability in context
    */
  def totFinalProb(terms: Set[Term], context: Vector[Term]) =
    sum(terms.map(finalProb(_, context)))

  // The equations

  /**
    * the equation saying that the total probability in the final distribution of
    * a context is 1.
    */
  def totProbOne(context: Vector[Term]): (F, F) =
    totFinalProb(termSetInContext(context), context) -> field.one

  /**
    * the equation saying that the total initial probability is 1.
    */
  lazy val totInitProbOne =
    sum(genTermSet.map(initProb)) -> field.one

  /**
    * equation saying that the sum of the probabilities of terms with a given type
    * is the probability of the event variable for terms having this type
    * in the probability distribution of a context
    */
  def hasTypProb(typ: Typ[Term], context: Vector[Term]): (F, F) =
    totFinalProb(termSetInContext(context).filter(_.typ == typ), context) -> hasTyp(
      typ,
      context)

  /**
    * equation saying that the sum of the probabilities of terms that are functions
    * is the probability of the event variable for terms being functions
    * in the probability distribution of a context
    */
  def isFuncProb(context: Vector[Term]): (F, F) =
    totFinalProb(termSetInContext(context).filter((t) => isFunc(t)), context) -> isFuncP(
      context)

  /**
    * equation saying that the sum of the probabilities of terms that are types
    * is the probability of the event variable for terms being types
    * in the probability distribution of a context
    */
  def isTypProb(context: Vector[Term]): (F, F) =
    totFinalProb(termSetInContext(context).filter((t) => isTyp(t)), context) -> isTypP(
      context)

  /**
    * all the equations for consistency of the final distribution for a context
    */
  def contextConsistency(context: Vector[Term]) =
    typSetInContext(context)
      .map(
        (typ) => hasTypProb(typ, context)
      )
      .toVector :+ isFuncProb(context) :+ isTypProb(context) :+ totProbOne(
      context)

  /**
    * all the equations for consistency of final probabilities in all contexts
    * as well as the total initial probability being 1.
    */
  lazy val consistencyEquations: Vector[(F, F)] =
    baseContexts.flatMap(contextConsistency) :+ totInitProbOne

  /**
    * a cost associated to a lack of consistency of the probabilities.
    */
  lazy val consistencyCost: F =
    sum(consistencyEquations
      .map { case (a, b) => klDiff(a, b) })


  /**
    * total probability of the proof of a theorem
    */
  def proofProb(thm: Typ[Term]) =
    totFinalProb(termSet.filter(_.typ == thm), Vector())

  /**
    * total probability of all theorems
    */
  lazy val totThmProb =
    totFinalProb(thmSet.map((t) => t: Term), Vector())

  /**
    * probability of a theorem as the probability of its type conditioned on
    * the type being inhabited
    */
  def thmProb(thm: Typ[Term]) =
    finalProb(thm, Vector()) / totThmProb

  /**
    * the Kullback-Liebler divergence of the proof probabilities from the theorem probabilities
    */
  lazy val kullbackLeibler: F =
    sum(thmSet.map { (thm) =>
      proofProb(thm) * log(proofProb(thm) / thmProb(thm))
    })

  /**
    * the entropy of the generating distribution
    */
  lazy val genEntropy: F =
    sum(
      genTermSet.map { (t) =>
        -initProb(t) / log(initProb(t))
      }
    )
}

/**
  * Adding equations from a simple generative model to [[EvolverEquations]]
  */
case class TermLearner[F: Field: Trig](supp: EvolverSupport,
                                       prob: EvolverVariables => F,
                                       apInv: ApplnInverse)
    extends EvolverEquations[F](supp, prob) {
  import EvolverVariables._

  import apInv._

  def initWeight = prob(InitWeight)

  def unApp: F = prob(UnApp)

  def appl: F = prob(Appl)

  def lambdaWeight: F = prob(LambdaWeight)

  def piWeight: F = prob(PiWeight)

  def varWeight: F = prob(VarWeight)

  /**
    * probability of a term in the initial distribution in a context
    */
  def initContextProb(t: Term, context: Vector[Term]): F =
    context match {
      case Vector() => initProb(t)
      case init :+ last =>
        if (t == last) varWeight
        else initContextProb(t, init) * (1 - varWeight)
    }

  /**
    * recursive probability that a term is generated by function application
    */
  def fromAppl(t: Term, context: Vector[Term]): F =
    sum(
      applInv(t, context)
        .map {
          case (f, x) =>
            (finalProb(f.term, context) / isFuncP(context)) *
              (finalProb(x, context) / hasTyp(f.dom, context)) *
              appl
        }
    )

  /**
    * recursive probability that a term is generated by unified application
    */
  def fromUnApp(t: Term, context: Vector[Term]): F =
    sum(
      unAppInv(t, context)
        .map {
          case (f, x) =>
            (finalProb(f.term, context) / isFuncP(context)) *
              finalProb(x, context) *
              unApp
        }
    )

  /**
    * recursive probability that a term is generated from an island, either lambda or pi;
    * the normalization of the context is done in [[finalProb]]
    */
  def fromIsland(t: Term, context: Vector[Term]): F =
    t match {
      case l: LambdaTerm[u, v] =>
        finalProb(l.value, context :+ l.variable) *
          finalProb(l.variable.typ, context) / isTypP(context)
      case l: PiDefn[u, v] =>
        (finalProb(l.value, context :+ l.variable) / isTypP(
          context :+ l.variable)) *
          finalProb(l.variable.typ, context) / isTypP(context)
      case _ => field.zero
    }

  /**
    * total recursive probability from a simple generative model
    */
  def recValue(t: Term, context: Vector[Term]): F =
    (initContextProb(t, context) * initWeight) + fromAppl(t, context) + fromUnApp(
      t,
      context) + fromIsland(t, context)

  /**
    * equations for final probabilities being as generated
    */
  lazy val evolutionEquations: Vector[(F, F)] =
    supp.contextTermVec.map {
      case (t, context) => finalProb(t, context) -> recValue(t, context)
    }

  /**
    * cost due to mismatch in the recursive generation equations.
    */
  lazy val evolutionCost: F =
    sum(
      evolutionEquations
      .map { case (a, b) => klDiff(a, b)}
    )


  /**
    * the total cost, including consistency and entropies
    */
  def cost(cnstWt: Double, evWt: Double, klWt: Double, hWt: Double): F =
    (consistencyCost * cnstWt) +
      (evolutionCost * evWt) +
      (kullbackLeibler * klWt) +
      (genEntropy * hWt)

}
