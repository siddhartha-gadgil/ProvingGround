package provingground.learning

import provingground._, HoTT._, induction._

import scala.collection.parallel._
import shapeless._, HList._

import TermRandomVars._

case class ParMapState(
    termDist: ParMap[Term, Double],
    typDist: ParMap[Typ[Term], Double],
    vars: Vector[Term] = Vector(),
    inds: ParMap[ExstInducDefn, Double] = ParMap(),
    goalDist: ParMap[Typ[Term], Double] = ParMap(),
    context: Context = Context.Empty
) {
  lazy val funcDist: ParMap[HoTT.ExstFunc, Double] = {
    val base = termDist.flatMap {
      case (x, w) => ExstFunc.opt(x).map { _ -> w }
    }
    val total = base.values.sum
    if (total > 0) base.mapValues(_ / total).to(ParMap) else ParMap.empty[ExstFunc, Double]
  }

  lazy val typFamilyDist: ParMap[HoTT.ExstFunc, Double] = {
    val base = termDist.flatMap {
      case (x, w) => TypFamilyOpt(x).map { _ -> w }
    }
    val total = base.values.sum
    if (total > 0) base.mapValues(_ / total).to(ParMap) else ParMap.empty[ExstFunc, Double]
  }

  lazy val (termWithTypDist, typTotalMap) = {
      val base = termDist.groupBy(_._1.typ : Typ[Term]).to(ParMap)
      val totalMap = base.mapValues(_.values.sum)
      (base.map{case (typ, m) => (typ, m.mapValues(_ / totalMap(typ)).to(ParMap))}, totalMap)
  }

  lazy val funcWithDomDist = {
      val base = funcDist.groupBy(_._1.dom : Typ[Term]).to(ParMap)
      val totalMap = base.mapValues(_.values.sum)
      base.map{case (typ, m) => (typ, m.mapValues(_ / totalMap(typ)).to(ParMap))}
  }

  lazy val inducStrucs = inds.map{case (ind, w) => ind.ind -> w}

  def value[T](randomVar: RandomVar[T]) : ParMap[T, Double] = randomVar match {
      case Terms => termDist.to(ParMap)
      case Typs => typDist.to(ParMap)
      case TargetTyps => typDist.to(ParMap)
      case Goals => goalDist.to(ParMap)
      case Funcs => funcDist.to(ParMap)
      case TypFamilies => typFamilyDist.to(ParMap)
      case TypsAndFamilies => typFamilyDist.map{case (x, w) => (x.func : Term, w)}.to(ParMap)
      case IsleDomains => typDist.to(ParMap)
      case InducDefns => inds.to(ParMap)
      case InducStrucs => inducStrucs.to(ParMap)
      case RandomVar.AtCoord(fmly, arg) => valueAt(fmly, arg)
  }

  def valueAt[Dom <: HList, T](randomVarFmly: RandomVarFamily[Dom, T], fullArg: Dom): ParMap[T, Double] = 
    (randomVarFmly, fullArg) match {
        case (TermsWithTyp, typ:: HNil) => termWithTypDist(typ).to(ParMap)
        case (FuncsWithDomain, dom :: HNil) => funcWithDomDist(dom).to(ParMap)
        case (FuncForCod, cod:: HNil) => 
        val base = termDist.flatMap{case (x, w) => Unify.targetCodomain(x, cod).map(_ -> w)}
            val total = base.values.sum
            if (total > 0) base.mapValues(_ / total).to(ParMap) else ParMap.empty[T, Double]
        case _ => throw new IllegalArgumentException(
              s"cannot find valueAt of ParMapState for $randomVarFmly at $fullArg"
            )
    }


}
