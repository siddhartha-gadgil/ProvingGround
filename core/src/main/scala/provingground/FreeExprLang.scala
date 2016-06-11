package provingground

import upickle.default._

sealed trait FreeExprLang {
  def as[E](implicit l: ExprLang[E]): Option[E]
}

case class Variable(name: String, typ: FreeExprLang) extends FreeExprLang {
  def as[E](implicit l: ExprLang[E]) =
    for (tp <- typ.as[E]; result <- l.variable(name, tp)) yield result
}

case class TypVariable(name: String, level: Int) extends FreeExprLang {
  def as[E](implicit l: ExprLang[E]) =
    l.typVariable(name, level)
}

case class AnonVar(typ: FreeExprLang) extends FreeExprLang {
  def as[E](implicit l: ExprLang[E]) =
    for (tp <- typ.as[E]; result <- l.anonVar(tp)) yield result
}

case class MetaVar(typ: FreeExprLang) extends FreeExprLang {
  def as[E](implicit l: ExprLang[E]) =
    for (tp <- typ.as[E]; result <- l.anonVar(tp)) yield result
}

case class FreeIncl1(typ: FreeExprLang) extends FreeExprLang {
  def as[E](implicit l: ExprLang[E]) =
    for (tp <- typ.as[E]; result <- l.incl1(tp)) yield result
}

case class FreeIncl2(typ: FreeExprLang) extends FreeExprLang {
  def as[E](implicit l: ExprLang[E]) =
    for (tp <- typ.as[E]; result <- l.incl2(tp)) yield result
}

case class FreeProj1(xy: FreeExprLang) extends FreeExprLang {
  def as[E](implicit l: ExprLang[E]) =
    for (tp <- xy.as[E]; result <- l.proj1(tp)) yield result
}

case class FreeProj2(xy: FreeExprLang) extends FreeExprLang {
  def as[E](implicit l: ExprLang[E]) =
    for (tp <- xy.as[E]; result <- l.proj2(tp)) yield result
}

case class FreeLambda(variable: FreeExprLang, value: FreeExprLang)
    extends FreeExprLang {
  def as[E](implicit l: ExprLang[E]) =
    for (x <- variable.as[E]; y <- value.as[E]; result <- l.lambda(x, y)) yield
      result
}

case class FreePi(variable: FreeExprLang, value: FreeExprLang)
    extends FreeExprLang {
  def as[E](implicit l: ExprLang[E]) =
    for (x <- variable.as[E]; y <- value.as[E]; result <- l.pi(x, y)) yield
      result
}

case class FreeAppln(func: FreeExprLang, arg: FreeExprLang)
    extends FreeExprLang {
  def as[E](implicit l: ExprLang[E]) =
    for (x <- func.as[E]; y <- arg.as[E]; result <- l.appln(x, y)) yield result
}

case class FreeEquality(lhs: FreeExprLang, rhs: FreeExprLang)
    extends FreeExprLang {
  def as[E](implicit l: ExprLang[E]) =
    for (x <- lhs.as[E]; y <- rhs.as[E]; result <- l.equality(x, y)) yield
      result
}

case class FreeSigma(variable: FreeExprLang, value: FreeExprLang)
    extends FreeExprLang {
  def as[E](implicit l: ExprLang[E]) =
    for (x <- variable.as[E]; y <- value.as[E]; result <- l.sigma(x, y)) yield
      result
}

case class FreePair(first: FreeExprLang, second: FreeExprLang)
    extends FreeExprLang {
  def as[E](implicit l: ExprLang[E]) =
    for (x <- first.as[E]; y <- second.as[E]; result <- l.pair(x, y)) yield
      result
}

case class OrCases(first: FreeExprLang, second: FreeExprLang)
    extends FreeExprLang {
  def as[E](implicit l: ExprLang[E]) =
    for (x <- first.as[E]; y <- second.as[E]; result <- l.orCases(x, y)) yield
      result
}

case class Or(first: FreeExprLang, second: FreeExprLang) extends FreeExprLang {
  def as[E](implicit l: ExprLang[E]) =
    for (x <- first.as[E]; y <- second.as[E]; result <- l.or(x, y)) yield
      result
}

case object TT extends FreeExprLang {
  def as[E](implicit l: ExprLang[E]) = l.tt
}

case object FF extends FreeExprLang {
  def as[E](implicit l: ExprLang[E]) = l.ff
}

case object QED extends FreeExprLang {
  def as[E](implicit l: ExprLang[E]) = l.qed
}

case class Numeral(n: Int) extends FreeExprLang {
  def as[E](implicit l: ExprLang[E]) = l.numeral(n)
}

object FreeExprLang {

  def Univ(n: Int) =
    TypVariable("Type", n) // FIXME : should better encode universes

  implicit object FreeLang
      extends ExprLang[FreeExprLang]
      with ExprPatterns[FreeExprLang] {
    def variable[S](name: S, typ: FreeExprLang): Option[FreeExprLang] =
      Some(Variable(name.toString(), typ))

    def typVariable[S](name: S, level: Int): Option[FreeExprLang] =
      Some(TypVariable(name.toString, level))

    /**
      * anonymous variable
      */
    def anonVar(typ: FreeExprLang): Option[FreeExprLang] = Some(AnonVar(typ))

    /**
      * meta-variable of a given type, i.e., whose value must be inferred
      * (elaborated in lean's terminology).
      */
    def metaVar(typ: FreeExprLang): Option[FreeExprLang] = Some(MetaVar(typ))

    def lambda(
        variable: FreeExprLang, value: FreeExprLang): Option[FreeExprLang] =
      Some(FreeLambda(variable, value))

    def pi(variable: FreeExprLang, typ: FreeExprLang): Option[FreeExprLang] =
      Some(FreePi(variable, typ))

    def appln(func: FreeExprLang, arg: FreeExprLang): Option[FreeExprLang] =
      Some(FreeAppln(func, arg))

    def equality(lhs: FreeExprLang, rhs: FreeExprLang): Option[FreeExprLang] =
      Some(FreeEquality(lhs, rhs))

    def sigma(variable: FreeExprLang,
              typFamily: FreeExprLang): Option[FreeExprLang] =
      Some(FreeSigma(variable, typFamily))

    def pair(x: FreeExprLang, y: FreeExprLang): Option[FreeExprLang] =
      Some(FreePair(x, y))

    def proj1(xy: FreeExprLang): Option[FreeExprLang] = Some(FreeProj1(xy))

    def proj2(xy: FreeExprLang): Option[FreeExprLang] = Some(FreeProj2(xy))

    def or(first: FreeExprLang, second: FreeExprLang): Option[FreeExprLang] =
      Some(Or(first, second))

    def incl1(typ: FreeExprLang): Option[FreeExprLang] = Some(FreeIncl1(typ))

    def incl2(typ: FreeExprLang): Option[FreeExprLang] = Some(FreeIncl2(typ))

    /**
      * true type
      */
    def tt: Option[FreeExprLang] = Some(TT)

    /**
      * element of true type
      */
    def qed: Option[FreeExprLang] = Some(QED)

    /**
      * false type
      */
    def ff: Option[FreeExprLang] = Some(FF)

    def orCases(
        first: FreeExprLang, second: FreeExprLang): Option[FreeExprLang] =
      Some(OrCases(first, second))

    def numeral(n: Int): Option[FreeExprLang] = Some(Numeral(n))

    def isPair: FreeExprLang => Option[(FreeExprLang, FreeExprLang)] = {
      case FreePair(first, second) => Some((first, second))
      case _ => None
    }

    def isSigma: FreeExprLang => Option[(FreeExprLang, FreeExprLang)] = {
      case FreeSigma(first, second) => Some((first, second))
      case _ => None
    }

    def isPi: FreeExprLang => Option[(FreeExprLang, FreeExprLang)] = {
      case FreePi(first, second) => Some((first, second))
      case _ => None
    }
  }

  object FromTerm
      extends TermToExpr[FreeExprLang](
          univ = (n) => Univ(n), predef = (t) => None)(FreeLang)

  def fromTerm(t: HoTT.Term) = FromTerm(t)

  def writeExpr(fr: FreeExprLang) = write(fr)

  def writeTerm(t: HoTT.Term): String = writeExpr(fromTerm(t).get)

  def readTerm(s: String): HoTT.Term =
    read[FreeExprLang](s).as[HoTT.Term](TermLang).get

  def writeDist(fd: FiniteDistribution[HoTT.Term]) =
    write(
        fd.pmf map { case Weighted(t, w) => PickledWeighted(writeTerm(t), w) })

  def readDist(s: String): FiniteDistribution[HoTT.Term] =
    FiniteDistribution(
        read[Vector[PickledWeighted]](s) map {
          case PickledWeighted(t, w) => Weighted(readTerm(t), w)
        }
    ).flatten

  def readTyp(s: String): HoTT.Typ[HoTT.Term] =
    read[FreeExprLang](s)
      .as[HoTT.Term](TermLang)
      .get
      .asInstanceOf[HoTT.Typ[HoTT.Term]]
}
