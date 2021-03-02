package provingground.learning
import provingground._
import provingground.{FiniteDistribution => FD, ProbabilityDistribution => PD}
import translation.{TeXTranslate, TermLang => TL}
import translation.Translator.unmatched


// import cats._
import cats.implicits._

import monix.eval._
// import monix.cats._

// import monix.execution.Scheduler.Implicits.global
import monix.reactive._

import HoTT._

/**
  * A refined deducer, i.e., evolution of terms and derivatives of evolution.
  * Various evolutions are defined mutually recursively - of functions, of types,
  * of terms of a type and of all terms.
  * Derivatives are defined mutually recursively with the evolutions.
  *
  * This is refined so that, for example, arguments are chosen conditionally from the domain of a function.
  */
object FineDeducer {
  type SomeFunc = FuncLike[u, v] forSome {
    type u <: Term with Subs[u]; type v <: Term with Subs[v]
  }

  def termClosure(vars: Vector[Term])(fd: FD[Term]) =
    fd map (lambdaClosure(vars))

  def typClosure(vars: Vector[Term])(fd: FD[Typ[Term]]) =
    fd map (piClosure(vars))

  def asFuncs(pd: PD[Term]): PD[SomeFunc] = pd map {
    case fn: FuncLike[u, v] => fn
  }

  def asTyps(pd: PD[Term]): PD[Typ[Term]] = pd map {
    case tp: Typ[u] => tp
  }

  def unif(vars: Term*)(terms: Term*)(axioms: Typ[Term]*) =
    FD.uniform(
      vars.toVector ++ terms ++ // axioms.map(lambdaClosure(vars.toVector)) ++
        axioms.map((t) =>
          s"""axiom_{${TeXTranslate(t)}}""" :: piClosure(vars.toVector)(t)))

  /**
    * evolution by function application with  unification
    */
  def unifApplnEv(funcEvolve: => (FD[Term] => PD[SomeFunc]),
                  argEvolve: => (FD[Term] => PD[Term]))(p: FD[Term]) =
    (funcEvolve(p) product argEvolve(p)) map {
      case (func, arg) => Unify.appln(func, arg)
    }

  /**
    * evolution by function application,
    * to be used by choosing function at random and then argument conditionally in its domain.
    */
  def simpleApplnEv(
      funcEvolve: => (FD[Term] => PD[SomeFunc]),
      argEvolve: => (Typ[Term] => FD[Term] => PD[Term]))(p: FD[Term]) =
    (funcEvolve(p) .fibProduct (_.dom, (tp: Typ[Term]) =>
      argEvolve(tp)(p))) map {
      case (func: FuncLike[u, v], arg) => func(arg.asInstanceOf[u])
      case (x, y)                      => unmatched(x, y)
    }

  /**
    * a lambda-island for evolution:
    *
    * @param typEvolve evolution of types, from which a variable is chosen
    * @param valueEvolve evolution of terms, given initial distribution
    * @param varWeight weight of the variable created for a lambda
    * @param p initial distribution
    *
    * A type is picked at random, a variable `x` with this type is generated and mixed in with weight `varWeight`
    * into the  initial distribution `p`. The evolved distribution is mapped by `x :-> _`.
    */
  def lambdaEv(varweight: Double)(
      typEvolve: => (FD[Term] => PD[Term]),
      valueEvolve: => (Term => FD[Term] => PD[Term]))(
      p: FD[Term]): PD[Option[Term]] =
    typEvolve(p) flatMap
      ({
        case tp: Typ[u] =>
          val x    = tp.Var
          val newp = (p * (1 - varweight)) ++ (FD.unif[Term](x) * varweight)
          (valueEvolve(x)(newp)) map
            ((y: Term) =>
              if (!isUniv(y)) TL.lambda(x, y)
              else None)
        case _ => FD.unif(None)
      })

  /**
    * a lambda-island for evolution:
    *
    * @param typEvolve evolution of types, from which a variable is chosen
    * @param valueEvolve evolution of terms, given initial distribution
    * @param varWeight weight of the variable created for a lambda
    * @param p initial distribution
    *
    * A type is picked at random, a variable `x` with this type is generated and mixed in with weight `varWeight`
    * into the  initial distribution `p`. The evolved distribution is mapped by `pi(x)(_)`.
    */
  def piEv(varweight: Double)(typEvolve: => (FD[Term] => PD[Term]),
                              valueEvolve: => (Term => FD[Term] => PD[Term]))(
      p: FD[Term]): PD[Option[Term]] =
    typEvolve(p) flatMap
      ({
        case tp: Typ[u] =>
          val x    = tp.Var
          val newp = (p * (1 - varweight)) ++ (FD.unif[Term](x) * varweight)
          (valueEvolve(x)(newp)) map ((y: Term) => TL.pi(x, y))
        case _ => FD.unif(None)
      })
}

/**
  * A refined evolver, i.e., evolution of terms and subclasses such as types.
  * Various evolutions are defined mutually recursively - of functions, of types,
  * of terms of a type, of type families and of all terms.
  * Derivatives are defined mutually recursively with the evolutions.
  *
  * This is refined so that, for example, arguments are chosen conditionally from the domain of a function.
  *
  * @param varWeight weight of a variable inside a lambda
  */
class FineEvolver(applnWeight: Double = 0.1,
                  lambdaWeight: Double = 0.1,
                  piWeight: Double = 0.1,
                  varWeight: Double = 0.3,
                  unifyWeight: Double = 0.5) { fine =>
  import FineDeducer._

  /**
    * FineEvolver with `varWeight` rescaled so that the weight of different variables does not depend on the order
    * in which they were introduced in lambda or pi islands.
    */
  lazy val varScaled =
    new FineEvolver(applnWeight,
                    lambdaWeight,
                    piWeight,
                    fine.varWeight / (1 + fine.varWeight),
                    unifyWeight)

  /**
    * evolution of terms, by combining various operations and islands
    */
  def evolve(fd: FD[Term]): PD[Term] =
    fd.<+?>(unifApplnEv(evolvFuncs, evolve)(fd), applnWeight * unifyWeight)
      .<+>(simpleApplnEv(evolvFuncs, evolveWithTyp)(fd),
           applnWeight * (1 - unifyWeight))
      .<+?>(lambdaEv(varWeight)(evolveTyp, (t) => varScaled.evolve)(fd),
            lambdaWeight)
      .<+?>(piEv(varWeight)(evolveTyp, (t) => varScaled.evolveTyp)(fd),
            piWeight)

  // def domTerms(f: SomeFunc): FD[Term] => PD[Term] =
  //   (fd: FD[Term]) =>
  //     evolve(fd) <+> (evolveWithTyp(f.dom)(fd), 1 - unifyWeight)

  /**
    * evolution of functions, used for function application.
    */
  def evolvFuncs(fd: FD[Term]): PD[SomeFunc] =
    asFuncs {
      fd.conditioned(isFunc)
        .<+?>(unifApplnEv(evolvFuncs, evolve)(fd), applnWeight * unifyWeight)
        .<+>(simpleApplnEv(evolvFuncs, evolveWithTyp)(fd),
             applnWeight * (1 - unifyWeight))
        .conditioned(isFunc)
      // .<+?>(lambdaEv(varWeight)(evolveTyp, (t) => varScaled.evolve)(fd),
      //       lambdaWeight)
    }

  // import spire.algebra._
  // import spire.implicits._

  /**
    * evolution of type families, to be used for function application to generate types.
    * lambda evolution uses only types and type families as values.
    */
  def evolvTypFamilies(fd: FD[Term]): PD[SomeFunc] =
    asFuncs {
      fd.<+?>(unifApplnEv(evolvTypFamilies, evolve)(fd),
              applnWeight * unifyWeight)
        .<+>(simpleApplnEv(evolvTypFamilies, evolveWithTyp)(fd),
             applnWeight * (1 - unifyWeight))
        .conditioned(isTypFamily)
      // .<+?>(lambdaEv(varWeight)(
      //         evolveTyp,
      //         (t) =>
      //           (d) =>
      //             varScaled.evolveTyp(d) <+>
      //               (varScaled.evolvTypFamilies(d).map((f) => f: Term), 0.5)
      //       )(fd),
      //       lambdaWeight)
    }

  /**
    * evolution of a term with a given type
    */
  def evolveWithTyp(tp: Typ[Term])(fd: FD[Term]): PD[Term] = {
    val p = (t: Term) => t.typ == tp
    val rawBase = fd
      .<+?>(unifApplnEv(evolvFuncs, evolve)(fd), applnWeight * unifyWeight)
      .<+>(simpleApplnEv(evolvFuncs, evolveWithTyp)(fd),
           applnWeight * (1 - unifyWeight))
    val base = rawBase.conditioned(p)
    tp match {
      case FuncTyp(dom: Typ[u], codom: Typ[v]) =>
        base.<+?>(
          lambdaEv(varWeight)((_) => FD.unif[Term](dom),
                              (t) => varScaled.evolveWithTyp(codom))(fd),
          lambdaWeight)
      case gf: GenFuncTyp[u, v] =>
        base.<+?>(
          lambdaEv(varWeight)(
            (_) => FD.unif[Term](gf.domain),
            (t) => varScaled.evolveWithTyp(gf.fib(t.asInstanceOf[u])))(fd),
          lambdaWeight)
      case Universe(_) =>
        rawBase
          .<+?>(piEv(varWeight)(evolveTyp, (t) => varScaled.evolveTyp)(fd),
                piWeight)
          .conditioned(p)
      case _ => base
    }
  }

  /**
    * evolution of types
    */
  def evolveTyp(fd: FD[Term]): PD[Term] = {
    fd.<+?>(unifApplnEv(evolvTypFamilies, evolve)(fd),
            applnWeight * unifyWeight)
      .<+>(simpleApplnEv(evolvTypFamilies, evolveWithTyp)(fd),
           applnWeight * (1 - unifyWeight))
      .conditioned(isTyp)
      .<+?>(piEv(varWeight)(evolveTyp, (t) => varScaled.evolveTyp)(fd),
            piWeight)
  }

}

/**
  * A refined deducer, i.e., evolution of terms and derivatives of evolution.
  * Various evolutions are defined mutually recursively - of functions, of types,
  * of terms of a type, of type families and of all terms.
  * Derivatives are defined mutually recursively with the evolutions.
  *
  * This is refined so that, for example, arguments are chosen conditionally from the domain of a function.
  *
  * @param varWeight weight of a variable inside a lambda
  */
case class FineDeducer(applnWeight: Double = 0.1,
                       lambdaWeight: Double = 0.1,
                       piWeight: Double = 0.1,
                       varWeight: Double = 0.3,
                       unifyWeight: Double = 0.5)
    extends FineEvolver(applnWeight,
                        lambdaWeight,
                        piWeight,
                        varWeight,
                        unifyWeight) { fine =>
  // import Deducer.{asFuncs}

  import FineDeducer._

  override lazy val varScaled =
    this.copy(varWeight = fine.varWeight / (1 + fine.varWeight))

  //  case class Derivative(evolved: FD[Term], evolvedFuncs: FD[SomeFunc], evolvedWithTyp: Typ[Term] => FD[Term])
  /**
    * derivative of evolution [[evolve]] of terms
    */
  def Devolve(fd: FD[Term], tang: FD[Term]): PD[Term] =
    tang
      .<+?>(DunifApplnFunc(fd, tang), applnWeight * unifyWeight)
      .<+>(DsimpleApplnFunc(fd, tang), applnWeight * (1 - unifyWeight))
      .<+?>(DunifApplnArg(fd, tang), applnWeight * unifyWeight)
      .<+>(DsimpleApplnArg(fd, tang), applnWeight * (1 - unifyWeight))
      .<+?>(DlambdaVar(fd, tang), lambdaWeight)
      .<+?>(DlambdaVal(fd, tang), lambdaWeight)
      .<+?>(DpiVar(fd, tang), piWeight)
      .<+?>(DpiVal(fd, tang), piWeight)

  // def DapplnFunc(fd: FD[Term], tang: FD[Term]): PD[Option[Term]] =
  //   DevolvFuncs(fd, tang) flatMap
  //     ((f) => domTerms(f)(fd) map (Unify.appln(f, _)))

  /**
    * partial derivative of unified function application with respect to the function term.
    */
  def DunifApplnFunc(fd: FD[Term], tang: FD[Term]): PD[Option[Term]] =
    DevolvFuncs(fd, tang) product evolve(fd) map {
      case (f, arg) => Unify.appln(f, arg)
    }

  /**
    * partial derivative of unified function application with respect to arguments.
    */
  def DunifApplnArg(fd: FD[Term], tang: FD[Term]): PD[Option[Term]] =
    evolvFuncs(fd) product Devolve(fd, tang) map {
      case (f, arg) => Unify.appln(f, arg)
    }

  /**
    * partial derivative of function application (without unification) with respect to functions
    */
  def DsimpleApplnFunc(fd: FD[Term], tang: FD[Term]): PD[Term] =
    (DevolvFuncs(fd, tang) .fibProduct
      (_.dom, (tp: Typ[Term]) => evolveWithTyp(tp)(fd))) map {
      case (func: FuncLike[u, v], arg) => func(arg.asInstanceOf[u])
      case (x, y)                      => unmatched(x, y)
    }

  /**
    * partial derivative of function application without unification
    * with respect to arguments.
    */
  def DsimpleApplnArg(fd: FD[Term], tang: FD[Term]): PD[Term] =
    (evolvFuncs(fd) .fibProduct
      (_.dom, (tp: Typ[Term]) => DevolveWithType(tp)(fd, tang))) map {
      case (func: FuncLike[u, v], arg) => func(arg.asInstanceOf[u])
      case (x, y)                      => unmatched(x, y)
    }

  /**
    * partial derivative of unified function application for type families with respect to type families
    */
  def DunifApplnTypFamilies(fd: FD[Term], tang: FD[Term]): PD[Option[Term]] =
    DevolvTypFamilies(fd, tang) product evolve(fd) map {
      case (f, arg) => Unify.appln(f, arg)
    }

  /**
    * partial derivative of unified function application for type families with respect to arguments
    */
  def DunifApplnTypArg(fd: FD[Term], tang: FD[Term]): PD[Option[Term]] =
    evolvTypFamilies(fd) product Devolve(fd, tang) map {
      case (f, arg) => Unify.appln(f, arg)
    }

  /**
    * partial derivative of function application without unification for type families with respect to type families
    */
  def DsimpleApplnTypFamilies(fd: FD[Term], tang: FD[Term]): PD[Term] =
    (DevolvTypFamilies(fd, tang) .fibProduct
      (_.dom, (tp: Typ[Term]) => evolveWithTyp(tp)(fd))) map {
      case (func: FuncLike[u, v], arg) => func(arg.asInstanceOf[u])
      case (x, y)                      => unmatched(x, y)
    }

  /**
    * partial derivative of function application without unification
    * for type families with respect to arguments.
    */
  def DsimpleApplnTypArg(fd: FD[Term], tang: FD[Term]): PD[Term] =
    (evolvTypFamilies(fd) .fibProduct
      (_.dom, (tp: Typ[Term]) => DevolveWithType(tp)(fd, tang))) map {
      case (func: FuncLike[u, v], arg) => func(arg.asInstanceOf[u])
      case (x, y)                      => unmatched(x, y)
    }

  // def DapplnArg(fd: FD[Term], tang: FD[Term]): PD[Option[Term]] =
  //   evolvFuncs(fd) flatMap
  //     ((f) => DdomTerms(f)(fd, tang) map (Unify.appln(f, _)))
  //
  // def DdomTerms(f: SomeFunc)(fd: FD[Term], tang: FD[Term]): PD[Term] =
  //   Devolve(fd, tang) <+> (DevolveWithType(f.dom)(fd, tang), 1 - unifyWeight)

  /**
    * partial derivative of lambda islands with respect to the weight  of the variable (really the type)
    */
  def DlambdaVar(fd: FD[Term], tang: FD[Term]): PD[Option[Term]] =
    DevolveTyp(fd, tang) flatMap
      ({
        case tp: Typ[u] =>
          val x    = tp.Var
          val newp = (fd * (1 - varWeight)) ++ (FD.unif[Term](x) * varWeight)
          (varScaled.evolve(newp)) map
            ((y: Term) =>
              if (!isUniv(y)) TL.lambda(x, y)
              else None)
        case _ => FD.unif(None)
      })

  def DlambdaTypVar(fd: FD[Term], tang: FD[Term]): PD[Option[Term]] =
    DevolveTyp(fd, tang) flatMap
      ({
        case tp: Typ[u] =>
          val x    = tp.Var
          val newp = (fd * (1 - varWeight)) ++ (FD.unif[Term](x) * varWeight)
          (varScaled.evolveTyp(newp) .<+> (varScaled
            .evolvTypFamilies(newp)
            .map((f) => f: Term), 0.5)).map((y: Term) =>
            if (!isUniv(y)) TL.lambda(x, y)
            else None)
        case _ => FD.unif(None)
      })

  /**
    * partial derivative of lambda islands with resepect to the value exported
    */
  def DlambdaVal(fd: FD[Term], tang: FD[Term]): PD[Option[Term]] =
    evolveTyp(fd) flatMap
      ({
        case tp: Typ[u] =>
          val x    = tp.Var
          val newp = (fd * (1 - varWeight)) ++ (FD.unif[Term](x) * varWeight)
          (varScaled.Devolve(newp, tang * (1 - varWeight))) map
            ((y: Term) =>
              if (!isUniv(y)) TL.lambda(x, y)
              else None)
        case _ => FD.unif(None)
      })

  def DlambdaTypVal(fd: FD[Term], tang: FD[Term]): PD[Option[Term]] =
    evolveTyp(fd) flatMap
      ({
        case tp: Typ[u] =>
          val x    = tp.Var
          val newp = (fd * (1 - varWeight)) ++ (FD.unif[Term](x) * varWeight)
          (varScaled.DevolveTyp(newp, tang .* (1 - varWeight)) .<+> (varScaled
            .DevolvTypFamilies(newp, tang * (1 - varWeight))
            .map((f) => f: Term), 0.5)).map((y: Term) =>
            if (!isUniv(y)) TL.lambda(x, y)
            else None)
        case _ => FD.unif(None)
      })

  /**
    * partial derivative of pi islands with resepect to the weight  of the variable (really the type)
    */
  def DpiVar(fd: FD[Term], tang: FD[Term]): PD[Option[Term]] =
    DevolveTyp(fd, tang) flatMap
      ({
        case tp: Typ[u] =>
          val x    = tp.Var
          val newp = (fd * (1 - varWeight)) ++ (FD.unif[Term](x) * varWeight)
          (varScaled.evolveTyp(newp)) map ((y: Term) => TL.pi(x, y))
        case _ => FD.unif(None)
      })

  /**
    * partial derivative of pi islands with resepect to the value exported
    */
  def DpiVal(fd: FD[Term], tang: FD[Term]): PD[Option[Term]] =
    evolveTyp(fd) flatMap
      ({
        case tp: Typ[u] =>
          val x    = tp.Var
          val newp = (fd * (1 - varWeight)) ++ (FD.unif[Term](x) * varWeight)
          (varScaled.DevolveTyp(newp, tang * (1 - varWeight))) map
            ((y: Term) => TL.pi(x, y))
        case _ => FD.unif(None)
      })

  /**
    * derivative of evolution of functions (adding components)
    */
  def DevolvFuncs(fd: FD[Term], tang: FD[Term]): PD[SomeFunc] =
    asFuncs {
      tang
        .<+?>(DunifApplnFunc(fd, tang), applnWeight * unifyWeight)
        .<+>(DsimpleApplnFunc(fd, tang), applnWeight * (1 - unifyWeight))
        .<+?>(DunifApplnArg(fd, tang), applnWeight * unifyWeight)
        .<+>(DsimpleApplnArg(fd, tang), applnWeight * (1 - unifyWeight))
        .conditioned(isFunc)
      // .<+?>(DlambdaVar(fd, tang), lambdaWeight)
      // .<+?>(DlambdaVal(fd, tang), lambdaWeight)
    }

  /**
    * derivative of evolution of type families (adding components)
    * FIXME the lambda terms should be specific to type families.
    */
  def DevolvTypFamilies(fd: FD[Term], tang: FD[Term]): PD[SomeFunc] =
    asFuncs {
      tang
        .<+?>(DunifApplnTypFamilies(fd, tang), applnWeight * unifyWeight)
        .<+>(DsimpleApplnTypFamilies(fd, tang), applnWeight * (1 - unifyWeight))
        .<+?>(DunifApplnTypArg(fd, tang), applnWeight * unifyWeight)
        .<+>(DsimpleApplnTypArg(fd, tang), applnWeight * (1 - unifyWeight))
        .conditioned(isTypFamily)
      // .<+?>(DlambdaTypVar(fd, tang), lambdaWeight)
      // .<+?>(DlambdaTypVal(fd, tang), lambdaWeight)
    }

  /**
    * derivative of evolution of terms with a fixed type
    */
  def DevolveWithType(tp: Typ[Term])(fd: FD[Term], tang: FD[Term]): PD[Term] = {
    val p = (t: Term) => t.typ == tp
    val rawBase = tang
      .<+?>(DunifApplnFunc(fd, tang), applnWeight * unifyWeight)
      .<+>(DsimpleApplnFunc(fd, tang), applnWeight * (1 - unifyWeight))
      .<+?>(DunifApplnArg(fd, tang), applnWeight * unifyWeight)
      .<+>(DsimpleApplnArg(fd, tang), applnWeight * (1 - unifyWeight))
    val base = rawBase.conditioned(p)
    tp match {
      case FuncTyp(dom: Typ[u], codom: Typ[v]) =>
        val x = dom.Var
        base.<+>(DevolveWithType(codom)(fd, tang) map ((y) => x :-> y),
                 lambdaWeight)
      case gf: GenFuncTyp[u, v] =>
        val x = gf.domain.Var
        base.<+>(DevolveWithType(gf.fib(x))(fd, tang) map ((y) => x :-> y),
                 lambdaWeight)
      case Universe(_) =>
        rawBase
          .<+?>(DpiVar(fd, tang), piWeight)
          .<+?>(DpiVal(fd, tang), piWeight)
          .conditioned(p)
      case _ => base
    }
  }

  /**
    * derivative of evolution of types
    */
  def DevolveTyp(fd: FD[Term], tang: FD[Term]): PD[Term] =
    tang
      .<+?>(DunifApplnTypFamilies(fd, tang), applnWeight * unifyWeight)
      .<+>(DsimpleApplnTypFamilies(fd, tang), applnWeight * (1 - unifyWeight))
      .<+?>(DunifApplnTypArg(fd, tang), applnWeight * unifyWeight)
      .<+>(DsimpleApplnTypArg(fd, tang), applnWeight * (1 - unifyWeight))
      .conditioned(isTyp)
      .<+?>(DpiVar(fd, tang), piWeight)
      .<+?>(DpiVal(fd, tang), piWeight)
}

/**
  * feedback based on the term-type map as well as ensuring total weight of theorems is not  small;
  * various steps are explicit for exploration and debugging
  */
case class TheoremFeedback(fd: FD[Term],
                           tfd: FD[Typ[Term]],
                           vars: Vector[Term] = Vector(),
                           scale: Double = 1.0,
                           thmScale: Double = 0.3,
                           thmTarget: Double = 0.2) {
  import FineDeducer._, math._

  /**
    * terms mapped to  lambdas with resepect to the variables
    */
  lazy val lfd = termClosure(vars)(fd)

  /**
    * types mapped to pis with respect to the variables.
    */
  lazy val pfd = typClosure(vars)(tfd)

  /**
    * distribution of theorems (inhabited types) weighted by proofs.
    */
  lazy val byProof =
    (lfd filter ((t) => !isTyp(t)) map (_.typ: Typ[Term])).flatten
      .normalized()

  /**
    * distribution of theorems (inhabited  types) weighted as in the type distribution, not normalized.
    */
  lazy val byStatementUnscaled = FD(byProof.pmf map {
    case Weighted(x, _) => Weighted(x: Typ[Term], pfd(x))
  }).flatten

  /**
    * total weight of theorems
    */
  lazy val thmTotal = byStatementUnscaled.total

  /**
    * amount to shift to ensure theorems have a minimum total weight
    */
  lazy val thmShift =
    if (thmTotal > 0 && thmTotal < thmTarget)
      log(thmTarget / thmTotal) * thmScale
    else 0

  /**
    * set of theorems
    */
  lazy val thmSet = byStatement.supp.toSet

  /**
    * distribution of theorems (inhabited  types) weighted as in the type distribution and normalized.
    */
  lazy val byStatement =
    if (thmTotal > 0) byStatementUnscaled * (1.0 / thmTotal)
    else FD.empty[Typ[Term]]

  /**
    * for each theorem, the pair of  entropies with respect to the distribution
    *   - as statement weight
    *   - as proof weight
    *
    */
  lazy val entropyPairs =
    byProof.pmf collect {
      case Weighted(x, _) if byStatement(x) > 0 =>
        x -> ((-log(byStatement(x)), -log(byProof(x))))
    }

  /**
    * term-type map feedback vector with entries pairs `(typ, p)`
    */
  lazy val feedbackVec =
    entropyPairs map { case (x, (a, b)) => (x, b - a) } sortBy {
      case (x, e)                       => -e
    }

  /**
    * term-type map feedback map ie [[feedbackVec]] as a map
    */
  lazy val feedbackMap = feedbackVec.toMap

  /**
    * term-type map feedback as function of type
    */
  def feedbackFunction(x: Typ[Term]) = max(0.0, feedbackMap.getOrElse(x, 0.0))

  /**
    * feedback as function of distribution on types
    */
  def feedbackTypDist(fd: FD[Typ[Term]]) = fd.integral(feedbackFunction)

  /**
    * feedback based on total weight of theorems
    */
  def thmFeedbackFunction(x: Term) = x match {
    case tp: Typ[u] =>
      if (thmSet contains (tp)) thmShift else 0.0
    case _ => 0.0
  }

  /**
    * feedback based on distributions on terms and on types,
    * by backward propagation from [[feedbackTypDist]] and [[thmFeedbackFunction]]
    */
  def feedbackTermDist(fd: FD[Term], typfd: FD[Typ[Term]]) = {
    val ltang = termClosure(vars)(fd).normalized()
    val tpfd = FiniteDistribution {
      typfd.pmf collect {
        case Weighted(tp: Typ[u], p) => Weighted[Typ[Term]](tp, p)
      }
    }
    val ptang = typClosure(vars)(tpfd).normalized()
    feedbackTypDist(typClosure(vars)((ltang) map (_.typ: Typ[Term]))) +
      ptang.integral(thmFeedbackFunction)
  }
}

object FineDeducerStep {
  case class Param(vars: Vector[Term] = Vector(),
                   size: Int = 10000,
                   derTotalSize: Int = 100000,
                   inertia: Double = 0.9,
                   scale: Double = 1.0,
                   thmScale: Double = 0.3,
                   thmTarget: Double = 0.2)

  /**
    * Monix observable of the full evolution for the distribution of terms,
    * for debugging etc.
    */
  def obserEv(p: FD[Term],
              fd: FineDeducer = new FineDeducer(),
              param: Param = Param())(implicit ms: MonixSamples) =
    Observable
      .fromAsyncStateAction[FineDeducerStep[Task], FineDeducerStep[Task]](
        (st: FineDeducerStep[Task]) =>
          st.succ.map((x) => (x, x)))(new FineDeducerStep(p, fd, param)(ms))

  /**
    * Monix observable for the  evolution of a distribution of terms
    */
  def observable(p: FD[Term],
                 fd: FineDeducer = new FineDeducer(),
                 param: Param = Param())(implicit ms: MonixSamples) =
    Observable.fromAsyncStateAction[FineDeducerStep[Task], FD[Term]](
      (st: FineDeducerStep[Task]) =>
        st.succ.map((x) => (x.p, x)))(new FineDeducerStep(p, fd, param)(ms))
}

/**
  * Monad based step for deducer, to be used, for example, with Monix `Task`
  * @tparam X[_] the monad such as Task
  * @param p the initial distribution
  * @param ded the actual deducer
  * @param param the parameters for learning
  * @param samp sampler, ie object to efficiently extract samples
  */
class FineDeducerStep[X[_]](val p: FD[Term],
                            ded: FineDeducer = FineDeducer(),
                            val param: FineDeducerStep.Param = FineDeducerStep
                              .Param())(implicit val samp: TangSamples[X]) {
  import samp._, param._, FineDeducer._

  /**
    * evolved probability distribution
    */
  lazy val init = ded.evolve(p)

  /**
    * task for finite distribution on terms after forward evolution and sampling
    */
  lazy val nextFD =
    for (samp <- sampFD(init, size))
      yield samp * (1.0 - inertia) ++ (p * inertia)

  /**
    * task for finite distribution on types after forward evolution and sampling
    */
  lazy val nextTypFD = sampFD(ded.evolveTyp(p), size).map((fd) =>
    fd.map { case u: Typ[u] => u: Typ[Term] })

  /**
    * task for [[TheoremFeedback]]
    */
  lazy val thmFeedback =
    for {
      nFD  <- nextFD
      ntFD <- nextTypFD
    } yield TheoremFeedback(nFD, ntFD, vars, scale, thmScale, thmTarget)

  /**
    * derivative of evolution as probability distribution
    */
  def derivativePD(tang: FD[Term]): PD[Term] = ded.Devolve(p, tang)

  /**
    * derivative of evolution sampled, as task
    */
  def derivativeFD(tang: FD[Term], n: Int) = sampFD(derivativePD(tang), n)

  /**
    * derivative of evolution of types sampled, as task
    */
  def derivativeTypsFD(tang: FD[Term], n: Int) =
    sampFD(asTyps(ded.DevolveTyp(p, tang)), n)

  /**
    * tangent sizes for sampling for backward propagation (as task)
    */
  lazy val tangSamples: X[Vector[(FD[Term], Int)]] =
    for (nfd <- nextFD; ts <- tangSizes(derTotalSize)(nfd)) yield ts

  /**
    * tangent samples forward evolved by derivatives as function of samples.
    */
  def derFDX(vec: Vector[(FD[Term], Int)]) =
    sequence {
      for {
        (fd, n) <- vec
      } yield
        for {
          dfd  <- derivativeFD(fd, n) map (_ * (n.toDouble / derTotalSize.toDouble))
          dtfd <- derivativeTypsFD(fd, n) map (_ * (n.toDouble / derTotalSize.toDouble))
        } yield (fd, (dfd, dtfd))
    }

  /**
    * tangent samples forward evolved by derivatives (as task).
    */
  lazy val derivativeFDs: X[Vector[(FD[Term], (FD[Term], FD[Typ[Term]]))]] =
    tangSamples.flatMap(derFDX)

  /**
    * tangents and feedbacks for them
    */
  lazy val feedBacks: X[Vector[(FD[Term], Double)]] =
    for {
      derFDs <- derivativeFDs
      thmFb  <- thmFeedback
    } yield
      for { (x, (tfd, tpfd)) <- derFDs } yield
        (x, thmFb.feedbackTermDist(tfd, tpfd))

  /**
    * the initial distribution for the next step
    */
  lazy val succFD =
    for {
      fbs <- feedBacks
      nfd <- nextFD
    } yield
      fbs
        .foldRight(nfd) {
          case ((t, w), fd) => fd ++ (t * (w * t.integral(nfd(_))))
        }
        .flatten
        .normalized()

  def newp(np: FD[Term]) = new FineDeducerStep(np, ded, param)

  /**
    * the next [[FineDeducerStep]]
    */
  lazy val succ = succFD.map(newp)

  /**
    * the next [[FineDeducerStep]]
    */
  def next = succ

}
