package provingground
import provingground.{
  FiniteDistribution => FD,
  ProbabilityDistribution => PD,
  TangVec => T
}

import TangVec.{liftLinear => lin, liftBilinear => bil, _}

import scala.language.existentials

import HoTT._

// import FineDeducer._

import spire.algebra._
import spire.implicits._

object TermEvolver{
  implicit class TPOps[A](pd: T[PD[A]]){
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

    def map[B](f: A => B) = TangVec(pd.point.map(f), pd.vec.map(f))

    def flatMap[B](f: A => T[PD[B]]) = TangVec(pd.point.flatMap( (a) => f(a).point), pd.vec.flatMap((a) => f(a).vec))

    def mapOpt[B](f: A => Option[B]) = TangVec(pd.point.mapOpt(f), pd.vec.mapOpt(f))
  }
}

trait ExstFunc{
  type U <: Term with Subs[U]

  type V <: Term with Subs[V]

  val func: FuncLike[U, V]

  val term: Term = func

  def apply(arg: Term) : Option[Term] = if (arg.typ == func.dom) Some(func.asInstanceOf[U]) else None
}

object ExstFunc{
  def apply[X<: Term with Subs[X], Y <: Term with Subs[Y]](fn: FuncLike[X, Y]) = new ExstFunc{
    type U = X
    type V = Y

    val func = fn
  }

  def opt(t: Term) : Option[ExstFunc] = t match {
    case fn: FuncLike[u, v] => Some(ExstFunc(fn))
    case _ => None
  }
}

class TermEvolver(unApp: Double = 0.1, appl : Double = 0.1, lambdaWeight : Double= 0.1, varWeight: Double = 0.3 ){
  import TermEvolver._

  val evolve : T[FD[Term]] => T[PD[Term]] =
    (init: T[FD[Term]]) =>
      (init : T[PD[Term]]) <+?> (
        TunifAppln(
          evolveFuncs(init) && evolve(init)) ,
        unApp) <+?> (
          Tappln(
            evolveFuncs(init) && evolveWithTyp(init)
          ),
          appl
        ) <+> (lambdaMix(init), lambdaWeight)

  val evolveFuncs : T[FD[Term]] => T[PD[ExstFunc]] =
    (init: T[FD[Term]]) =>
      evolve(init).mapOpt(ExstFunc.opt)

  val evolveWithTyp : T[FD[Term]] => T[Typ[Term] => PD[Term]] = ???

  val evolveTyps : T[FD[Term]] => T[PD[Typ[Term]]] = ???

  def evolveAtTyp(typ: Typ[Term]) =
    (tfd: T[FD[Term]]) => {
      val res = evolveWithTyp(tfd)
      TangVec(res.point(typ), res.vec(typ))
    }


  def unifAppln(x: PD[ExstFunc], y: PD[Term]) = (x product y).map{
    case (fn, arg) => Unify.appln(fn.func, arg)
  }

  val TunifAppln = bil(unifAppln)

  def simpleAppln(funcs: PD[ExstFunc], args: Typ[Term] => PD[Term]) =
    (funcs fibProduct  (_.func.dom , args)).map{
    case (fn, arg) => fn(arg)
  }

  val Tappln = bil(simpleAppln)

  def lambdaMixVar(x: Term, wt: Double, base: => (T[FD[Term]] => T[PD[Term]])) = (tfd: T[FD[Term]]) => {
    val dist = TangVec(
      tfd.point * (1- wt) + (x, wt),
      tfd.vec * (1- wt) + (x, wt))
    base(dist).map ((y) => x :~> y : Term)
  }

  def lambdaMixTyp(typ: Typ[Term], wt: Double, base: => (T[FD[Term]] => T[PD[Term]])) =
    {
      val x = typ.Var
      lambdaMixVar(x, wt, base)
    }

  def lambdaMix(fd: T[FD[Term]]) =  evolveTyps(fd).flatMap((tp) => lambdaMixTyp(tp, varWeight, evolve)(fd))
}
