package provingground
import provingground.{
  FiniteDistribution => FD,
  ProbabilityDistribution => PD,
  TermLang => TL,
  TangVec => T,
}

import TangVec.{liftLinear => lin, liftBilinear => bil, _}

import scala.language.existentials

import HoTT._

import FineDeducer._

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
  }
}

class TermEvolver(unApp: Double = 0.2){
  import TermEvolver._

  val evolve : T[FD[Term]] => T[PD[Term]] =
    (init: T[FD[Term]]) =>
      (init : T[PD[Term]]) <+?> (
        TunifAppln(tangProd(evolveFuncs(init), evolve(init))) ,
        unApp)

  val evolveFuncs : T[FD[Term]] => T[PD[SomeFunc]] = ???

  val evolveWithTyp : T[FD[Term]] => T[Typ[Term] => PD[Term]] = ???

  val evolveTyps : T[FD[Term]] => T[PD[Typ[Term]]] = ???


  def unifAppln(x: PD[SomeFunc], y: PD[Term]) = (x product y).map{
    case (func: Term, arg) => Unify.appln(func, arg)
  }

  val TunifAppln = bil(unifAppln)

  def simpleAppln(funcs: PD[SomeFunc], args: Typ[Term] => PD[Term]) =
    (funcs fibProduct  (_.dom , args)).map{
    case (func: FuncLike[u, v], arg) => func(arg.asInstanceOf[u])
  }

  val Tappln = bil(simpleAppln)


}
