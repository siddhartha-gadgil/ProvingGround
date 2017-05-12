package provingground
import provingground.{
  FiniteDistribution => FD,
  ProbabilityDistribution => PD,
  TermLang => TL
}

import scala.language.existentials

import HoTT._

/**
  * A refined deducer, i.e., evolution of terms and derivatives of evolution.
  * Various evolutions are defined mutually recursively - of functions, of types, of terms of a type and of all terms.
  * Derivatives are defined mutually recursively with the evolutions.
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
        axioms.map("axiom" :: _).map(lambdaClosure(vars.toVector))
    )

  // @deprecated("use products", "8/2/2017")
  // def applnEv(funcEvolve: => (FD[Term] => PD[SomeFunc]),
  //             argEvolve: => (SomeFunc => FD[Term] => PD[Term]))(p: FD[Term]) =
  //   funcEvolve(p) flatMap ((f) => argEvolve(f)(p) map (Unify.appln(f, _)))

  def unifApplnEv(funcEvolve: => (FD[Term] => PD[SomeFunc]),
                  argEvolve: => (FD[Term] => PD[Term]))(p: FD[Term]) =
    (funcEvolve(p) product argEvolve(p)) map {
      case (func, arg) => Unify.appln(func, arg)
    }

  def simpleApplnEv(
      funcEvolve: => (FD[Term] => PD[SomeFunc]),
      argEvolve: => (Typ[Term] => FD[Term] => PD[Term]))(p: FD[Term]) =
    (funcEvolve(p) fibProduct (_.dom, (tp: Typ[Term]) =>
      argEvolve(tp)(p))) map {
      case (func: FuncLike[u, v], arg) => func(arg.asInstanceOf[u])
    }

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

case class FineDeducer(applnWeight: Double = 0.1,
                       val lambdaWeight: Double = 0.1,
                       piWeight: Double = 0.1,
                       varWeight: Double = 0.3,
                       unifyWeight: Double = 0.5) { fine =>
  // import Deducer.{asFuncs}

  import FineDeducer._

  lazy val varScaled =
    this.copy(varWeight = this.varWeight / (1 + this.varWeight))

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

  def evolvFuncs(fd: FD[Term]): PD[SomeFunc] =
    asFuncs {
      fd.<+?>(unifApplnEv(evolvFuncs, evolve)(fd), applnWeight * unifyWeight)
        .<+>(simpleApplnEv(evolvFuncs, evolveWithTyp)(fd),
             applnWeight * (1 - unifyWeight))
        .conditioned(isFunc)
        .<+?>(lambdaEv(varWeight)(evolveTyp, (t) => varScaled.evolve)(fd),
              lambdaWeight)
    }

  def evolvTypFamilies(fd: FD[Term]): PD[SomeFunc] =
    asFuncs {
      fd.<+?>(unifApplnEv(evolvTypFamilies, evolve)(fd),
              applnWeight * unifyWeight)
        .<+>(simpleApplnEv(evolvTypFamilies, evolveWithTyp)(fd),
             applnWeight * (1 - unifyWeight))
        .conditioned(isTypFamily)
        .<+?>(lambdaEv(varWeight)(evolveTyp, (t) => varScaled.evolve)(fd),
              lambdaWeight)
    }

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

  def evolveTyp(fd: FD[Term]): PD[Term] = {
    fd.<+?>(unifApplnEv(evolvTypFamilies, evolve)(fd),
            applnWeight * unifyWeight)
      .<+>(simpleApplnEv(evolvTypFamilies, evolveWithTyp)(fd),
           applnWeight * (1 - unifyWeight))
      .conditioned(isTyp)
      .<+?>(piEv(varWeight)(evolveTyp, (t) => varScaled.evolveTyp)(fd),
            piWeight)
  }

//  case class Derivative(evolved: FD[Term], evolvedFuncs: FD[SomeFunc], evolvedWithTyp: Typ[Term] => FD[Term])
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

  def DunifApplnFunc(fd: FD[Term], tang: FD[Term]): PD[Option[Term]] =
    DevolvFuncs(fd, tang) product evolve(fd) map {
      case (f, arg) => Unify.appln(f, arg)
    }

  def DunifApplnArg(fd: FD[Term], tang: FD[Term]): PD[Option[Term]] =
    evolvFuncs(fd) product Devolve(fd, tang) map {
      case (f, arg) => Unify.appln(f, arg)
    }

  def DsimpleApplnFunc(fd: FD[Term], tang: FD[Term]): PD[Term] =
    (DevolvFuncs(fd, tang) fibProduct
      (_.dom, (tp: Typ[Term]) => evolveWithTyp(tp)(fd))) map {
      case (func: FuncLike[u, v], arg) => func(arg.asInstanceOf[u])
    }

  def DunifApplnTypFamilies(fd: FD[Term], tang: FD[Term]): PD[Option[Term]] =
    DevolvTypFamilies(fd, tang) product evolve(fd) map {
      case (f, arg) => Unify.appln(f, arg)
    }

  def DunifApplnTypArg(fd: FD[Term], tang: FD[Term]): PD[Option[Term]] =
    evolvTypFamilies(fd) product Devolve(fd, tang) map {
      case (f, arg) => Unify.appln(f, arg)
    }

  def DsimpleApplnTypFamilies(fd: FD[Term], tang: FD[Term]): PD[Term] =
    (DevolvTypFamilies(fd, tang) fibProduct
      (_.dom, (tp: Typ[Term]) => evolveWithTyp(tp)(fd))) map {
      case (func: FuncLike[u, v], arg) => func(arg.asInstanceOf[u])
    }

  def DsimpleApplnTypArg(fd: FD[Term], tang: FD[Term]): PD[Term] =
    (evolvTypFamilies(fd) fibProduct
      (_.dom, (tp: Typ[Term]) => DevolveWithType(tp)(fd, tang))) map {
      case (func: FuncLike[u, v], arg) => func(arg.asInstanceOf[u])
    }

  def DsimpleApplnArg(fd: FD[Term], tang: FD[Term]): PD[Term] =
    (evolvFuncs(fd) fibProduct
      (_.dom, (tp: Typ[Term]) => DevolveWithType(tp)(fd, tang))) map {
      case (func: FuncLike[u, v], arg) => func(arg.asInstanceOf[u])
    }

  // def DapplnArg(fd: FD[Term], tang: FD[Term]): PD[Option[Term]] =
  //   evolvFuncs(fd) flatMap
  //     ((f) => DdomTerms(f)(fd, tang) map (Unify.appln(f, _)))
  //
  // def DdomTerms(f: SomeFunc)(fd: FD[Term], tang: FD[Term]): PD[Term] =
  //   Devolve(fd, tang) <+> (DevolveWithType(f.dom)(fd, tang), 1 - unifyWeight)

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

  def DpiVar(fd: FD[Term], tang: FD[Term]): PD[Option[Term]] =
    DevolveTyp(fd, tang) flatMap
      ({
        case tp: Typ[u] =>
          val x    = tp.Var
          val newp = (fd * (1 - varWeight)) ++ (FD.unif[Term](x) * varWeight)
          (varScaled.evolveTyp(newp)) map ((y: Term) => TL.pi(x, y))
        case _ => FD.unif(None)
      })

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

  def DevolvFuncs(fd: FD[Term], tang: FD[Term]): PD[SomeFunc] =
    asFuncs {
      tang
        .<+?>(DunifApplnFunc(fd, tang), applnWeight * unifyWeight)
        .<+>(DsimpleApplnFunc(fd, tang), applnWeight * (1 - unifyWeight))
        .<+?>(DunifApplnArg(fd, tang), applnWeight * unifyWeight)
        .<+>(DsimpleApplnArg(fd, tang), applnWeight * (1 - unifyWeight))
        .conditioned(isFunc)
        .<+?>(DlambdaVar(fd, tang), lambdaWeight)
        .<+?>(DlambdaVal(fd, tang), lambdaWeight)
    }

  def DevolvTypFamilies(fd: FD[Term], tang: FD[Term]): PD[SomeFunc] =
    asFuncs {
      tang
        .<+?>(DunifApplnTypFamilies(fd, tang), applnWeight * unifyWeight)
        .<+>(DsimpleApplnTypFamilies(fd, tang),
             applnWeight * (1 - unifyWeight))
        .<+?>(DunifApplnTypArg(fd, tang), applnWeight * unifyWeight)
        .<+>(DsimpleApplnTypArg(fd, tang), applnWeight * (1 - unifyWeight))
        .conditioned(isTypFamily)
        .<+?>(DlambdaVar(fd, tang), lambdaWeight)
        .<+?>(DlambdaVal(fd, tang), lambdaWeight)
    }

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

case class TheoremFeedback(fd: FD[Term],
                           tfd: FD[Typ[Term]],
                           vars: Vector[Term] = Vector(),
                           scale: Double = 1.0,
                           thmScale: Double = 0.3,
                           thmTarget: Double = 0.2) {
  import FineDeducer._, math._

  lazy val lfd = termClosure(vars)(fd)

  lazy val pfd = typClosure(vars)(tfd)

  lazy val byProof =
    (lfd filter ((t) => !isTyp(t)) map (_.typ: Typ[Term])).flatten
      .normalized()

  lazy val byStatementUnscaled = FD(byProof.pmf map {
    case Weighted(x, _) => Weighted(x: Typ[Term], pfd(x))
  }).flatten

  lazy val thmTotal = byStatementUnscaled.total

  lazy val thmShift =
    if (thmTotal > 0 && thmTotal < thmTarget)
      log(thmTarget / thmTotal) * thmScale
    else 0

  lazy val thmSet = byStatement.supp.toSet

  lazy val byStatement =
    if (thmTotal > 0) byStatementUnscaled * (1.0 / thmTotal)
    else FD.empty[Typ[Term]]

  lazy val entropyPairs =
    byProof.pmf collect {
      case Weighted(x, _) if byStatement(x) > 0 =>
        x -> ((-log(byStatement(x)), -log(byProof(x))))
    }

  lazy val feedbackVec =
    entropyPairs map { case (x, (a, b)) => (x, b - a) } sortBy {
      case (x, e)                       => -e
    }

  lazy val feedbackMap = feedbackVec.toMap

  def feedbackFunction(x: Typ[Term]) = max(0.0, feedbackMap.getOrElse(x, 0.0))

  def feedbackTypDist(fd: FD[Typ[Term]]) = fd.integral(feedbackFunction)

  def thmFeedbackFunction(x: Term) = x match {
    case tp: Typ[u] =>
      if (thmSet contains (tp)) thmShift else 0.0
    case _ => 0.0
  }

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
