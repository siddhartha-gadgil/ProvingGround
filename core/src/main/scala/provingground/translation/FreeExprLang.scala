package provingground.translation

import provingground._

import functionfinder._

import upickle.default._

import cats.implicits._

import cats._

import cats.data.Tuple2K

import Functors._

import SubTypePattern.pattern

sealed trait FreeExpr {
  def as[E](implicit l: ExprLang[E]): Option[E]
}

object FreeExpr {

  case class Variable(name: String, typ: FreeExpr) extends FreeExpr {
    def as[E](implicit l: ExprLang[E]) =
      for (tp <- typ.as[E]; result <- l.variable(name, tp)) yield result
  }

  case class TypVariable(name: String, level: Int) extends FreeExpr {
    def as[E](implicit l: ExprLang[E]) =
      l.typVariable(name, level)
  }

  case class AnonVar(typ: FreeExpr) extends FreeExpr {
    def as[E](implicit l: ExprLang[E]) =
      for (tp <- typ.as[E]; result <- l.anonVar(tp)) yield result
  }

  case class MetaVar(typ: FreeExpr) extends FreeExpr {
    def as[E](implicit l: ExprLang[E]) =
      for (tp <- typ.as[E]; result <- l.anonVar(tp)) yield result
  }

  case class FreeIncl1(typ: FreeExpr) extends FreeExpr {
    def as[E](implicit l: ExprLang[E]) =
      for (tp <- typ.as[E]; result <- l.incl1(tp)) yield result
  }

  case class FreeIncl2(typ: FreeExpr) extends FreeExpr {
    def as[E](implicit l: ExprLang[E]) =
      for (tp <- typ.as[E]; result <- l.incl2(tp)) yield result
  }

  case class FreeProj1(xy: FreeExpr) extends FreeExpr {
    def as[E](implicit l: ExprLang[E]) =
      for (tp <- xy.as[E]; result <- l.proj1(tp)) yield result
  }

  case class FreeProj2(xy: FreeExpr) extends FreeExpr {
    def as[E](implicit l: ExprLang[E]) =
      for (tp <- xy.as[E]; result <- l.proj2(tp)) yield result
  }

  case class FreeLambda(variable: FreeExpr, value: FreeExpr) extends FreeExpr {
    def as[E](implicit l: ExprLang[E]) =
      for (x <- variable.as[E]; y <- value.as[E]; result <- l.lambda(x, y))
        yield result
  }

  case class FreePi(variable: FreeExpr, value: FreeExpr) extends FreeExpr {
    def as[E](implicit l: ExprLang[E]) =
      for (x <- variable.as[E]; y <- value.as[E]; result <- l.pi(x, y))
        yield result
  }

  case class FreeAppln(func: FreeExpr, arg: FreeExpr) extends FreeExpr {
    def as[E](implicit l: ExprLang[E]) =
      for (x <- func.as[E]; y <- arg.as[E]; result <- l.appln(x, y))
        yield result
  }

  case class FreeEquality(lhs: FreeExpr, rhs: FreeExpr) extends FreeExpr {
    def as[E](implicit l: ExprLang[E]) =
      for (x <- lhs.as[E]; y <- rhs.as[E]; result <- l.equality(x, y))
        yield result
  }

  case class FreeSigma(variable: FreeExpr, value: FreeExpr) extends FreeExpr {
    def as[E](implicit l: ExprLang[E]) =
      for (x <- variable.as[E]; y <- value.as[E]; result <- l.sigma(x, y))
        yield result
  }

  case class FreePair(first: FreeExpr, second: FreeExpr) extends FreeExpr {
    def as[E](implicit l: ExprLang[E]) =
      for (x <- first.as[E]; y <- second.as[E]; result <- l.pair(x, y))
        yield result
  }

  case class OrCases(first: FreeExpr, second: FreeExpr) extends FreeExpr {
    def as[E](implicit l: ExprLang[E]) =
      for (x <- first.as[E]; y <- second.as[E]; result <- l.orCases(x, y))
        yield result
  }

  case class Or(first: FreeExpr, second: FreeExpr) extends FreeExpr {
    def as[E](implicit l: ExprLang[E]) =
      for (x <- first.as[E]; y <- second.as[E]; result <- l.or(x, y))
        yield result
  }

  case object TT extends FreeExpr {
    def as[E](implicit l: ExprLang[E]) = l.tt
  }

  case object FF extends FreeExpr {
    def as[E](implicit l: ExprLang[E]) = l.ff
  }

  case object QED extends FreeExpr {
    def as[E](implicit l: ExprLang[E]) = l.qed
  }

  case class Numeral(n: Int) extends FreeExpr {
    def as[E](implicit l: ExprLang[E]) = l.numeral(n)
  }

  case class Special(name: String, typ: FreeExpr, args: List[FreeExpr])
      extends FreeExpr {
    def as[E](implicit l: ExprLang[E]) = None
  }

  object Special {
    import Functors._
    import Translator._

    val pattern = Pattern.partial[FreeExpr, Coded] {
      case Special(name, typ, args) => (name, (typ, args))
    }

    val build = (c: Coded[FreeExpr]) =>
      (Special(c._1, c._2._1, c._2._2): FreeExpr)
  }

  case object Univ {
    def apply(n: Int) =
      TypVariable("Type", n) // FIXME : should better encode universes

    def unapply(e: FreeExpr) = e match {
      case TypVariable("Type", n) => Some(n)
      case _                      => None
    }
  }

  implicit object FreeLang
      extends ExprLang[FreeExpr]
      with ExprPatterns[FreeExpr] {
    def variable[S](name: S, typ: FreeExpr): Option[FreeExpr] =
      Some(Variable(name.toString(), typ))

    def typVariable[S](name: S, level: Int): Option[FreeExpr] =
      Some(TypVariable(name.toString, level))

    /**
      * anonymous variable
      */
    def anonVar(typ: FreeExpr): Option[FreeExpr] = Some(AnonVar(typ))

    /**
      * meta-variable of a given type, i.e., whose value must be inferred
      * (elaborated in lean's terminology).
      */
    def metaVar(typ: FreeExpr): Option[FreeExpr] = Some(MetaVar(typ))

    def lambda(variable: FreeExpr, value: FreeExpr): Option[FreeExpr] =
      Some(FreeLambda(variable, value))

    def pi(variable: FreeExpr, typ: FreeExpr): Option[FreeExpr] =
      Some(FreePi(variable, typ))

    def appln(func: FreeExpr, arg: FreeExpr): Option[FreeExpr] =
      Some(FreeAppln(func, arg))

    def equality(lhs: FreeExpr, rhs: FreeExpr): Option[FreeExpr] =
      Some(FreeEquality(lhs, rhs))

    def sigma(variable: FreeExpr, typFamily: FreeExpr): Option[FreeExpr] =
      Some(FreeSigma(variable, typFamily))

    def pair(x: FreeExpr, y: FreeExpr): Option[FreeExpr] =
      Some(FreePair(x, y))

    def proj1(xy: FreeExpr): Option[FreeExpr] = Some(FreeProj1(xy))

    def proj2(xy: FreeExpr): Option[FreeExpr] = Some(FreeProj2(xy))

    def or(first: FreeExpr, second: FreeExpr): Option[FreeExpr] =
      Some(Or(first, second))

    def incl1(typ: FreeExpr): Option[FreeExpr] = Some(FreeIncl1(typ))

    def incl2(typ: FreeExpr): Option[FreeExpr] = Some(FreeIncl2(typ))

    /**
      * true type
      */
    def tt: Option[FreeExpr] = Some(TT)

    /**
      * element of true type
      */
    def qed: Option[FreeExpr] = Some(QED)

    /**
      * false type
      */
    def ff: Option[FreeExpr] = Some(FF)

    def orCases(first: FreeExpr, second: FreeExpr): Option[FreeExpr] =
      Some(OrCases(first, second))

    def numeral(n: Int): Option[FreeExpr] = Some(Numeral(n))

    def isPair: FreeExpr => Option[(FreeExpr, FreeExpr)] = {
      case FreePair(first, second) => Some((first, second))
      case _                       => None
    }

    def isSigma: FreeExpr => Option[(FreeExpr, FreeExpr)] = {
      case FreeSigma(first, second) => Some((first, second))
      case _                        => None
    }

    def isPi: FreeExpr => Option[(FreeExpr, FreeExpr)] = {
      case FreePi(first, second) => Some((first, second))
      case _                     => None
    }
  }

  object FromTerm
      extends TermToExpr[FreeExpr](univ = (n) => Univ(n),
                                   predef = (t) => None)(FreeLang)

  def fromTerm(t: HoTT.Term) = FromTerm(t)

  def writeExpr(fr: FreeExpr) = write(fr)

  def writeTerm(t: HoTT.Term): String = writeExpr(fromTerm(t).get)

  def readTerm(s: String): HoTT.Term =
    read[FreeExpr](s).as[HoTT.Term](TermLang).get

  import TermToExpr.{encode, decode}

  import HoTT.Term

  def writeDist(fd: FiniteDistribution[HoTT.Term],
                names: Vector[(Term, String)] = Vector()) =
    write(fd.pmf map {
      case Weighted(t, w) =>
        PickledWeighted(writeTerm(encode(names)(t)), w)
    })

  def readDist(s: String, names: Vector[(Term, String)] = Vector())
    : FiniteDistribution[HoTT.Term] =
    FiniteDistribution(read[Vector[PickledWeighted]](s) map {
      case PickledWeighted(t, w) => Weighted(decode(names)(readTerm(t)), w)
    }).flatten

  def readTyp(s: String): HoTT.Typ[HoTT.Term] =
    read[FreeExpr](s)
      .as[HoTT.Term](TermLang)
      .get
      .asInstanceOf[HoTT.Typ[HoTT.Term]]
}

import Translator._
import Functors._

import provingground._, HoTT._

object FreeExprPatterns {
  import FreeExpr._
  import ExprLang._
  def freeToExpr[E: ExprLang] =
    (Pattern.partial[FreeExpr, II] {
      case FreeAppln(a, b) => (a, b)
    } >> appln[E]) ||
      (Pattern.partial[FreeExpr, II] {
        case FreeLambda(a, b) => (a, b)
      } >> lambda[E]) ||
      (Pattern.partial[FreeExpr, II] {
        case FreePair(a, b) => (a, b)
      } >> pair[E]) ||
      (Pattern.partial[FreeExpr, II] {
        case FreePi(a, b) => (a, b)
      } >> pi[E]) ||
      (Pattern.partial[FreeExpr, II] {
        case FreeSigma(a, b) => (a, b)
      } >> sigma[E]) ||
      (Pattern.partial[FreeExpr, II] {
        case FreeEquality(a, b) => (a, b)
      } >> equality[E]) ||
      (Pattern.partial[FreeExpr, II] {
        case Or(a, b) => (a, b)
      } >> or[E]) ||
      (Pattern.partial[FreeExpr, II] {
        case OrCases(a, b) => (a, b)
      } >> orCases[E]) ||
      (Pattern.partial[FreeExpr, Named] {
        case Variable(name, typ) => (name, typ)
      }(namedTrav) >> variable[E]) ||
      (Pattern.partial[FreeExpr, Id] {
        case FreeIncl1(a) => a
      } >> incl1[E]) ||
      (Pattern.partial[FreeExpr, Id] {
        case FreeIncl2(a) => a
      } >> incl2[E]) ||
      (Pattern.partial[FreeExpr, Id] {
        case FreeProj1(a) => a
      } >> proj1[E]) ||
      (Pattern.partial[FreeExpr, Id] {
        case FreeProj2(a) => a
      } >> proj2[E]) || Translator.Simple((f: FreeExpr) => f.as[E])

  import FreeExpr._

  val univ = Pattern[FreeExpr, N](Univ.unapply)

  import TermLang._

  val freeToTerm =
    (univ >>> ((n: Int) => Universe(n): Term)) || freeToExpr[Term]

  val termToFree =
    TermPatterns.termToExpr((n: Int) => Some(Univ(n)): Option[FreeExpr])

  //  val prefix = "`"
  //
  //  def writePair(nexp: (String, List[FreeExpr])) = write(nexp)
  //
  //  def readPair(s: String) = read[(String, List[FreeExpr])](s)
  //
  //  def encode(exp : Coded[FreeExpr]) : FreeExpr =
  //    Variable(prefix + writePair((exp._1, exp._3)), exp._2)
  //
  //   val decode = Pattern.partial[FreeExpr, Coded]{
  //    case Variable(name, typ) if name.startsWith(prefix) =>
  //      val code = name.drop(prefix.length)
  //      val (p, expr) = readPair(code)
  //      (p, typ, expr)
  //  }
}

object SpecialTerms {
  object Names {
    val rfl = "reflexivity"

    val idRec = "idRecFn"

    val idInduc = "idInducFn"
  }

  object Decompose
      extends Pattern.Partial[Term, Coded]({
        case rfl @ Refl(dom: Typ[u], x: Term) =>
          (Names.rfl, (rfl.typ: Typ[Term], List(dom, x)))
        case rfn @ IdentityTyp.RecFn(domain: Typ[u],
                                     target: Typ[v],
                                     data: Func[x, y],
                                     a: Term,
                                     b: Term) =>
          (Names.idRec, (rfn.typ: Typ[Term], List(domain, target, data, a, b)))
        case incl1 @ IdentityTyp.InducFn(domain: Typ[u],
                                         target: Term,
                                         data: FuncLike[x, y],
                                         a: Term,
                                         b: Term) =>
          (Names.idInduc,
           (incl1.typ: Typ[Term], List(domain, target, data, a, b)))
      })

  import Names._

  val build: Coded[Term] => Term = {
    case (`rfl`, (_, List(dom: Typ[u], x: Term))) =>
      Refl(dom, x.asInstanceOf[u])
    case (`idInduc`,
          (_,
           List(domain: Typ[u],
                target: Term,
                data: FuncLike[x, v],
                a: Term,
                b: Term))) =>
      IdentityTyp.InducFn(
        domain,
        target
          .asInstanceOf[FuncLike[u, FuncLike[u, FuncLike[Term, Typ[v]]]]],
        data.asInstanceOf[FuncLike[u, v]],
        a.asInstanceOf[u],
        b.asInstanceOf[u])
  }

  import FreeExpr.Special

  val termToFree =
    (Decompose >>> (Special.build)) || (FreeExprPatterns.termToFree)

  val freeToTerm = (Special.pattern >>> build) || (FreeExprPatterns.freeToTerm)
}

object FreeExprHLPatterns {
  import FreeExpr._

  import shapeless._

  import HList._

  val variable = pattern[FreeExpr, Variable, StIdHN]

  implicitly[QuasiInclHList[FreeExpr, String :: HNil, StHN]]

  val anonVar = pattern[FreeExpr, AnonVar, IdHN]

  val typVariable = pattern[FreeExpr, TypVariable, StIntHN]

  val metaVar = pattern[FreeExpr, MetaVar, IdHN]

  val inL = pattern[FreeExpr, FreeIncl1, IdHN]

  val inR = pattern[FreeExpr, FreeIncl2, IdHN]

  val proj1 = pattern[FreeExpr, FreeProj1, IdHN]

  val proj2 = pattern[FreeExpr, FreeProj2, IdHN]

  val lambdaPat = pattern[FreeExpr, FreeLambda, IdIdHN]

  val piPat = pattern[FreeExpr, FreePi, IdIdHN]

  val sigmaPat = pattern[FreeExpr, FreeSigma, IdIdHN]

  val applnPat = pattern[FreeExpr, FreeAppln, IdIdHN]

  val pair = pattern[FreeExpr, FreePair, IdIdHN]

  val coprod = pattern[FreeExpr, Or, IdIdHN]

  val coprodElim = pattern[FreeExpr, OrCases, IdIdHN]

  val equality = pattern[FreeExpr, FreeEquality, IdIdHN]

  val tt = pattern[FreeExpr, TT.type, HN]

  val ff = pattern[FreeExpr, FF.type, HN]

  val qed = pattern[FreeExpr, QED.type, HN]
}
