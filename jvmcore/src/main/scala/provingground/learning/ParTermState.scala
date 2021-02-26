package provingground.learning

import provingground.HoTT._
import provingground.{FiniteDistribution => FD, _}
import shapeless._
import induction._
import provingground.learning.GeneratorNode.{Map, MapOpt}

import scala.language.higherKinds
import GeneratorNode._
import TermRandomVars._
import scala.collection.parallel.immutable._

import TermGeneratorNodes._
import scala.math.Ordering.Double.TotalOrdering
import provingground.FiniteDistribution

case class ParTermState(ts: TermState) {
  import ts._

  import ParTermState._

  lazy val termDistMap: ParMap[Term, Double] = fdToParMap(terms)

  lazy val typDistMap: ParMap[Typ[Term], Double] = fdToParMap(typs)

  lazy val funcDistMap: ParMap[ExstFunc, Double] = {
    val base = termDistMap.flatMap {
      case (x, w) =>
        ExstFunc.opt(x).map { fn =>
          fn -> w
        }
    }
    val total = base.values.sum
    if (total > 0) base.mapValues(_ * (1 / total)).to(ParMap)
    else ParMap.empty
  }

  lazy val thmsSet: ParSet[Typ[Term]] = termDistMap.keySet
    .map(_.typ)
    .intersect(typDistMap.keySet union (goalSet))
    .to(ParSet)

  lazy val thmsByStParMap: ParMap[Typ[Term], Double] = {
    val base  = typDistMap.filterKeys(thmsSet.contains(_))
    val total = base.values.sum
    if (total > 0) base.mapValues(_ * (1 / total)).to(ParMap)
    else ParMap.empty
  }

  lazy val thmsByPfParMap: ParMap[Typ[Term], Double] =
    thmsSet.map(tp => tp -> termsWithTypsMap(tp).map(_._2).sum).to(ParMap)

  lazy val typFamilyDistMap: ParMap[ExstFunc, Double] = {
    val base = termDistMap.flatMap {
      case (x, w) =>
        TypFamilyOpt(x).map { fn =>
          fn -> w
        }
    }
    val total = base.values.sum
    if (total > 0) base.mapValues(_ * (1 / total)).to(ParMap)
    else ParMap.empty
  }

  lazy val termsWithTypsMap
      : ParMap[HoTT.Typ[HoTT.Term], ParMap[HoTT.Term, Double]] =
    termDistMap.groupBy(_._1.typ: Typ[Term]).map {
      case (typ, m) =>
        val total = m.values.sum
        typ -> m.map { case (x, p) => (x, p / total) }
    }

  lazy val funcsWithDomsMap =
    funcDistMap.groupBy(_._1.dom: Typ[Term]).map {
      case (typ, m) =>
        val total = m.values.sum
        typ -> m.map { case (x, p) => (x, p / total) }
    }

  lazy val lemmasPar: Vector[(Typ[Term], Option[Term], Double)] = thmsSet
    .map { tp =>
      val pfs = termsWithTypsMap(tp)
      val q   = pfs.map(_._2).sum
      (
        tp,
        Some(pfs.maxBy(_._2)._1),
        -(thmsByStParMap(tp) / (q * math.log(q)))
      )
    }
    .to(Vector)
    .sortBy(_._3)
    .reverse

  lazy val domTotals = funcDistMap.groupBy(_._1).mapValues(_.values.sum)

  import TermRandomVars._

  def elemVal(elem: Any, randomVar: RandomVar[_]): Double =
    (elem, randomVar) match {
      case (x: Term, Terms)           => termDistMap.getOrElse(x, 0)
      case (x: Typ[u], Typs)          => typDistMap.getOrElse(x, 0)
      case (x: Typ[u], TargetTyps)    => typDistMap.getOrElse(x, 0)
      case (x: ExstFunc, Funcs)       => funcDistMap.getOrElse(x, 0)
      case (x: ExstFunc, TypFamilies) => typFamilyDistMap.getOrElse(x, 0)
      case (x: Term, TypsAndFamilies) =>
        ExstFunc
          .opt(x)
          .map(fn => typFamilyDistMap.getOrElse(fn, 0.0))
          .getOrElse(0.0) +
          typOpt(x).map(typ => typDistMap.getOrElse(typ, 0.0)).getOrElse(0.0)
      case (x: ExstInducDefn, InducDefns)    => inds(x)
      case (x: ExstInducStrucs, InducStrucs) => inds.map(_.ind)(x)
      case (x: Typ[Term], Goals)             => goals(x)
      case (x: Typ[Term], IsleDomains)       => typDistMap.getOrElse(x, 0)
      case (x, RandomVar.AtCoord(fmly: RandomVarFamily[u, t], arg)) =>
        (x, fmly, arg) match {
          case (x: Term, TermsWithTyp, (typ: Typ[u]) :: HNil) =>
            termsWithTypsMap.get(typ).map(_.getOrElse(x, 0.0)).getOrElse(0.0)
          case (x: ExstFunc, FuncsWithDomain, (typ: Typ[u]) :: HNil) =>
            funcsWithDomsMap.get(typ).map(_.getOrElse(x, 0.0)).getOrElse(0.0)
          case (x: Term, FuncForCod, (cod: Typ[u]) :: HNil) =>
            terms
              .condMap(Unify.targetCodomain(_, cod))(x)
          case _ =>
            throw new IllegalArgumentException(
              s"cannot find valueAt of TermState for $fmly at $arg"
            )
        }

    }

}

object ParTermState {
  import scala.collection.parallel.CollectionConverters._

  def fdToParMap[T](fd: FD[T]): ParMap[T, Double] =
    fd.pmf.par.groupBy(_.elem).mapValues(_.map(_.weight).sum).to(ParMap)
}
