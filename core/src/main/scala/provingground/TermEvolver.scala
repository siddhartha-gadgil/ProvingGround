package provingground
import provingground.{
  FiniteDistribution => FD,
  ProbabilityDistribution => PD,
  TangVec => T
}

import TangVec.{liftLinear => lin, liftBilinear => bil}

// import scala.language.existentials

import HoTT._

// import FineDeducer._

// import spire.algebra._
import spire.implicits._

object TermEvolver {
  implicit class TPOps[A](pd: T[PD[A]]) {

    /**
      * generates from the mixed in distribution with probability _weight_,
      * otherwise defaults to this distribution;
      * as the mixed in distribution is called by name, it may depend on the present one.
      */
    def <+>(mixin: => T[PD[A]], weight: Double) =
      T(pd.point <+> (mixin.point, weight), pd.vec <+> (mixin.vec, weight))

    /**
      * generates from the mixed in optional valued distribution with probability `weight`,
      * otherwise, or if the optional returns None, defaults to this distribution;
      * the mixed in distribution is call by name, so may depend on this distribution.
      */
    def <+?>(mixin: => T[PD[Option[A]]], weight: Double) =
      T(pd.point <+?> (mixin.point, weight), pd.vec <+?> (mixin.vec, weight))

    def map[B](f: A => B) = lin((p: PD[A]) => p.map(f))(pd)

    def flatMap[B](f: A => T[PD[B]]): T[PD[B]] =
      TangVec(pd.point.flatMap((a) => f(a).point),
              pd.vec.flatMap((a) => f(a).vec))

    // def flatMapped[B](f: A => PD[B]) : T[PD[B]] = ???

    def condMap[B](f: A => Option[B]) = lin((p: PD[A]) => p.condMap(f))(pd)

    def conditioned(pred: A => Boolean) =
      lin((p: PD[A]) => p.conditioned(pred))(pd)
  }

  def typOpt(x: Term) = x match {
    case typ: Typ[_] => Some(typ: Typ[Term])
    case _           => None
  }

  def justTerm[U <: Term with Subs[U]](x: U) = x: Term
}

trait ExstFunc {
  type U <: Term with Subs[U]

  type V <: Term with Subs[V]

  val func: FuncLike[U, V]

  val term: Term = func

  val dom: Typ[Term] = func.dom

  def apply(arg: Term): Option[Term] =
    if (arg.typ == func.dom) Some(func.asInstanceOf[U]) else None
}

object ExstFunc {
  def apply[X <: Term with Subs[X], Y <: Term with Subs[Y]](
      fn: FuncLike[X, Y]) = new ExstFunc {
    type U = X
    type V = Y

    val func = fn
  }

  def opt(t: Term): Option[ExstFunc] = t match {
    case fn: FuncLike[u, v] => Some(ExstFunc(fn))
    case _                  => None
  }
}

class TermEvolver(unApp: Double = 0.1,
                  appl: Double = 0.1,
                  lambdaWeight: Double = 0.1,
                  piWeight: Double = 0.1,
                  varWeight: Double = 0.3) {
  import TermEvolver._

  val evolve: T[FD[Term]] => T[PD[Term]] =
    (init: T[FD[Term]]) =>
      (init: T[PD[Term]]) <+?> (TunifAppln(evolveFuncs(init) && evolve(init)),
      unApp) <+?> (
        Tappln(
          evolveFuncs(init) && evolveWithTyp(init)
        ),
        appl
      ) <+>
        (lambdaMix(init), lambdaWeight) <+>
        (piMix(init).map(justTerm[Typ[Term]]), piWeight)

  val evolveFuncs: T[FD[Term]] => T[PD[ExstFunc]] =
    (init: T[FD[Term]]) => {
      evolve(init) <+?> (TunifAppln(evolveFuncs(init) && evolve(init)),
      unApp) <+?> (
        Tappln(
          evolveFuncs(init) && evolveWithTyp(init)
        ),
        appl
      ) <+> (lambdaMix(init), lambdaWeight)
    }.condMap(ExstFunc.opt)

  val evolveWithTyp: T[FD[Term]] => T[Typ[Term] => PD[Term]] =
    (tfd: T[FD[Term]]) =>
      TangVec(
        (tp: Typ[Term]) => evolveAtTyp(tp)(tfd).point,
        (tp: Typ[Term]) => evolveAtTyp(tp)(tfd).vec
    )

  val evolveTyps: T[FD[Term]] => T[PD[Typ[Term]]] =
    (init: T[FD[Term]]) => {
      evolve(init) <+?> (TunifAppln(evolveFuncs(init) && evolve(init)),
      unApp) <+?> (
        Tappln(
          evolveFuncs(init) && evolveWithTyp(init)
        ),
        appl
      )
    }.condMap(typOpt) <+>
      (piMix(init), piWeight)

  def evolveAtTyp(typ: Typ[Term]): T[FD[Term]] => T[PD[Term]] =
    (tfd: T[FD[Term]]) => {
      evolve(tfd)
    }.conditioned(_.typ == typ)

  def unifAppln(x: PD[ExstFunc], y: PD[Term]) = (x product y).map {
    case (fn, arg) => Unify.appln(fn.func, arg)
  }

  val TunifAppln = bil(unifAppln)

  def simpleAppln(funcs: PD[ExstFunc], args: Typ[Term] => PD[Term]) =
    (funcs fibProduct (_.func.dom, args)).map {
      case (fn, arg) => fn(arg)
    }

  val Tappln = bil(simpleAppln)

  def lambdaMixVar(x: Term,
                   wt: Double,
                   base: => (T[FD[Term]] => T[PD[Term]])) =
    (tfd: T[FD[Term]]) => {
      val dist =
        lin((fd: FD[Term]) => fd * (1 - wt) + (x, wt))(tfd)
      base(dist).map((y) => x :~> y: Term)
    }

  def piMixVar(x: Term,
               wt: Double,
               base: => (T[FD[Term]] => T[PD[Typ[Term]]])) =
    (tfd: T[FD[Term]]) => {
      val dist =
        lin((fd: FD[Term]) => fd * (1 - wt) + (x, wt))(tfd)
      base(dist).map((y) => x ~>: y: Typ[Term])
    }

  def lambdaMixTyp(typ: Typ[Term],
                   wt: Double,
                   base: => (T[FD[Term]] => T[PD[Term]])) = {
    val x = typ.Var
    lambdaMixVar(x, wt, base)
  }

  def piMixTyp(typ: Typ[Term],
               wt: Double,
               base: => (T[FD[Term]] => T[PD[Typ[Term]]])) = {
    val x = typ.Var
    piMixVar(x, wt, base)
  }

  def lambdaMix(fd: T[FD[Term]]) =
    evolveTyps(fd).flatMap((tp) => lambdaMixTyp(tp, varWeight, evolve)(fd))

  def lambdaForTyp(typ: Typ[Term])(fd: T[FD[Term]]): T[PD[Term]] = typ match {
    case FuncTyp(dom: Typ[u], codom: Typ[v]) =>
      lambdaMixTyp(dom, lambdaWeight, evolveAtTyp(codom))(fd)
    case PiDefn(variable: Term, value: Typ[u]) =>
      val x: Term = variable.typ.Var
      val cod     = value.replace(variable, x)
      lambdaMixTyp(variable.typ, lambdaWeight, evolveAtTyp(cod))(fd)
    case _ =>
      TangVec(FD.empty[Term], FD.empty[Term])
  }

  def piMix(fd: T[FD[Term]]) =
    evolveTyps(fd).flatMap((tp) => piMixTyp(tp, varWeight, evolveTyps)(fd))
}
