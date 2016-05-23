package provingground

import provingground.{TruncatedDistribution => TD}

class TruncatedDistributionLang[E: ExprLang]
    extends ExprLang[TruncatedDistribution[E]] {
  val l = implicitly[ExprLang[E]]

  def variable[S](name: S, typ: TD[E]): Option[TD[E]] =
    TD.optF(TD.map(typ)(l.variable(name, _)))

  def typVariable[S](name: S): Option[TD[E]] =
    l.typVariable(name) map ((e: E) => TD.atom(e))

  /**
    * anonymous variable
    */
  def anonVar(typ: TD[E]): Option[TD[E]] =
    TD.optF(TD.map(typ)(l.anonVar))

  /**
    * meta-variable of a given type, i.e., whose value must be inferred
    * (elaborated in lean's terminology).
    */
  def metaVar(typ: TD[E]): Option[TD[E]] =
    TD.optF(TD.map(typ)(l.metaVar))

  def lambda(variable: TD[E], value: TD[E]): Option[TD[E]] =
    TD.optF(TD.mapOp(variable, value)(l.lambda))

  def pi(variable: TD[E], typ: TD[E]): Option[TD[E]] =
    TD.optF(TD.mapOp(variable, typ)(l.pi))

  def appln(func: TD[E], arg: TD[E]): Option[TD[E]] =
    TD.optF(TD.mapOp(func, arg)(l.appln))

  def equality(lhs: TD[E], rhs: TD[E]): Option[TD[E]] =
    TD.optF(TD.mapOp(lhs, rhs)(l.equality))

  def sigma(variable: TD[E], typ: TD[E]): Option[TD[E]] =
    TD.optF(TD.mapOp(variable, typ)(l.sigma))

  def pair(x: TD[E], y: TD[E]): Option[TD[E]] =
    TD.optF(TD.mapOp(x, y)(l.pair))

  def proj1(xy: TD[E]): Option[TD[E]] =
    TD.optF(TD.map(xy)(l.proj1))

  def proj2(xy: TD[E]): Option[TD[E]] =
    TD.optF(TD.map(xy)(l.proj2))

  def or(first: TD[E], second: TD[E]): Option[TD[E]] =
    TD.optF(TD.mapOp(first, second)(l.or))

  def incl1(typ: TD[E]): Option[TD[E]] =
    TD.optF(TD.map(typ)(l.incl1))

  def incl2(typ: TD[E]): Option[TD[E]] =
    TD.optF(TD.map(typ)(l.incl2))

  /**
    * true type
    */
  def tt: Option[TD[E]] =
    l.tt map ((e: E) => TD.atom(e))

//    TD.optF(l.tt map ((e: E) => TD.Atom[E](e)))

  /**
    * element of true type
    */
  def qed: Option[TD[E]] =
    l.qed map ((e: E) => TD.atom(e))

  /**
    * false type
    */
  def ff: Option[TD[E]] =
    l.ff map ((e: E) => TD.atom(e))

  def orCases(first: TD[E], second: TD[E]): Option[TD[E]] =
    TD.optF(TD.mapOp(first, second)(l.orCases))

  def numeral(n: Int): Option[TD[E]] =
    l.numeral(n) map ((e: E) => TD.atom(e))
}

class TruncatedDistributionDomain[E: Domain]
    extends Domain[TruncatedDistribution[E]] {
  val l = implicitly[Domain[E]]

  def domain: TruncatedDistribution[E] => Option[TruncatedDistribution[E]] =
    (f: TruncatedDistribution[E]) => TD.optF(TD.map(f)(l.domain))
}

class TruncatedDistributionExprPatterns[E: ExprPatterns]
    extends ExprPatterns[TruncatedDistribution[E]] {
  val l = implicitly[ExprPatterns[E]]

  def isPair: TD[E] => Option[(TD[E], TD[E])] =
    (td: TD[E]) => {
      val tdPairOpt = TD.optF(td map (l.isPair))
      tdPairOpt map ((td) => (td.map(xy => (xy._1)), td map ((xy) => (xy._2))))
    }

  def isSigma: TD[E] => Option[(TD[E], TD[E])] =
    (td: TD[E]) => {
      val tdPairOpt = TD.optF(td map (l.isSigma))
      tdPairOpt map ((td) => (td.map(xy => (xy._1)), td map ((xy) => (xy._2))))
    }

  def isPi: TD[E] => Option[(TD[E], TD[E])] =
    (td: TD[E]) => {
      val tdPairOpt = TD.optF(td map (l.isPi))
      tdPairOpt map ((td) => (td.map(xy => (xy._1)), td map ((xy) => (xy._2))))
    }
}
