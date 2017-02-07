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

  def asFuncs(pd: PD[Term]): PD[SomeFunc] = pd map {
    case fn: FuncLike[u, v] => fn
  }

  def applnEv(funcEvolve: => (FD[Term] => PD[SomeFunc]),
              argEvolve: => (SomeFunc => FD[Term] => PD[Term]))(p: FD[Term]) =
    funcEvolve(p) flatMap ((f) => argEvolve(f)(p) map (Unify.appln(f, _)))

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
                       unifyWeight: Double = 0.5,
                       val vars: Vector[Term] = Vector()) { fine =>
  // import Deducer.{asFuncs}

  import FineDeducer._

  lazy val varScaled =
    this.copy(varWeight = this.varWeight / (1 + this.varWeight))

  def evolve(fd: FD[Term]): PD[Term] =
    fd.<+?>(applnEv(evolvFuncs, (f: SomeFunc) => domTerms(f))(fd), applnWeight)
      .<+?>(lambdaEv(varWeight)(evolveTyp, (t) => varScaled.evolve)(fd),
            lambdaWeight)
      .<+?>(piEv(varWeight)(evolveTyp, (t) => varScaled.evolveTyp)(fd),
            piWeight)

  def domTerms(f: SomeFunc): FD[Term] => PD[Term] =
    (fd: FD[Term]) =>
      evolve(fd) <+> (evolveWithTyp(f.dom)(fd), 1 - unifyWeight)

  def evolvFuncs(fd: FD[Term]): PD[SomeFunc] =
    asFuncs {
      fd.<+?>(applnEv(evolvFuncs, (f: SomeFunc) => domTerms(f))(fd),
              applnWeight)
        .conditioned(isFunc)
        .<+?>(lambdaEv(varWeight)(evolveTyp, (t) => varScaled.evolve)(fd),
              lambdaWeight)
    }

  def evolveWithTyp(tp: Typ[Term])(fd: FD[Term]): PD[Term] = {
    val p = (t: Term) => t.typ == tp
    val rawBase = fd.<+?>(
      applnEv(evolvFuncs, (f: SomeFunc) => domTerms(f))(fd),
      applnWeight)
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
    fd.<+?>(applnEv(evolvFuncs, (f: SomeFunc) => domTerms(f))(fd), applnWeight)
      .conditioned(isTyp)
      .<+?>(piEv(varWeight)(evolveTyp, (t) => varScaled.evolveTyp)(fd),
            piWeight)
  }

//  case class Derivative(evolved: FD[Term], evolvedFuncs: FD[SomeFunc], evolvedWithTyp: Typ[Term] => FD[Term])
  def Devolve(fd: FD[Term], tang: FD[Term]): PD[Term] =
    tang
      .<+?>(DapplnFunc(fd, tang), applnWeight)
      .<+?>(DapplnArg(fd, tang), applnWeight)
      .<+?>(DlambdaVar(fd, tang), lambdaWeight)
      .<+?>(DlambdaVal(fd, tang), lambdaWeight)
      .<+?>(DpiVar(fd, tang), piWeight)
      .<+?>(DpiVal(fd, tang), piWeight)

  def DapplnFunc(fd: FD[Term], tang: FD[Term]): PD[Option[Term]] =
    DevolvFuncs(fd, tang) flatMap
      ((f) => domTerms(f)(fd) map (Unify.appln(f, _)))

  def DapplnArg(fd: FD[Term], tang: FD[Term]): PD[Option[Term]] =
    evolvFuncs(fd) flatMap
      ((f) => DdomTerms(f)(fd, tang) map (Unify.appln(f, _)))

  def DdomTerms(f: SomeFunc)(fd: FD[Term], tang: FD[Term]): PD[Term] =
    Devolve(fd, tang) <+> (DevolveWithType(f.dom)(fd, tang), 1 - unifyWeight)

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
        .<+?>(DapplnFunc(fd, tang), applnWeight)
        .<+?>(DapplnArg(fd, tang), applnWeight)
        .conditioned(isFunc)
        .<+?>(DlambdaVar(fd, tang), lambdaWeight)
        .<+?>(DlambdaVal(fd, tang), lambdaWeight)
    }

  def DevolveWithType(tp: Typ[Term])(fd: FD[Term], tang: FD[Term]): PD[Term] = {
    val p = (t: Term) => t.typ == tp
    val rawBase = tang
      .<+?>(DapplnFunc(fd, tang), applnWeight)
      .<+?>(DapplnArg(fd, tang), applnWeight)
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
      .<+?>(DapplnFunc(fd, tang), applnWeight)
      .<+?>(DapplnArg(fd, tang), applnWeight)
      .conditioned(isTyp)
      .<+?>(DpiVar(fd, tang), piWeight)
      .<+?>(DpiVal(fd, tang), piWeight)
}
