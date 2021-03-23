package provingground.scalahott

import provingground._
import HoTT._

import spire.math._
import spire.implicits._

import scala.language.implicitConversions

object QField extends SymbolicField[Rational] {
  override def toString = "Q"

  val QTyp: QField.LocalTyp.type = LocalTyp

  sealed trait PosWit extends Term with Subs[PosWit] {
    val value: LocalTerm

    lazy val typ = Pos(value)

    def +(that: PosWit) = PosWitSum(this, that)
  }

  case class PosWitSum(a: PosWit, b: PosWit) extends PosWit {
    lazy val value: QField.LocalTerm = a.value + b.value

    def newobj: PosWit =
      throw new IllegalArgumentException(
        s"trying to use the constant $this as a variable (or a component of one)"
      )

    def subs(x: Term, y: Term) = PosWitSum(a.replace(x, y), b.replace(x, y))
  }

  case class PosWitProd(a: PosWit, b: PosWit) extends PosWit {
    lazy val value: QField.LocalTerm = a.value * b.value

    def newobj: PosWit =
      throw new IllegalArgumentException(
        s"trying to use the constant $this as a variable (or a component of one)"
      )

    def subs(x: Term, y: Term) = PosWitSum(a.replace(x, y), b.replace(x, y))
  }

  case class PosLiteral(a: Rational) extends PosWit {
    require(a >= 0, s"Rational number $a not positive")

    val value: QField.LocalTerm = Literal(a)

    def newobj: PosLiteral =
      throw new IllegalArgumentException(
        s"trying to use the constant $this as a variable (or a component of one)"
      )

    def subs(x: Term, y: Term): PosLiteral = this
  }

  case object PosZero extends PosWit {

    val value: QField.LocalTerm = Literal(0)

    def newobj: PosZero.type =
      throw new IllegalArgumentException(
        s"trying to use the constant $this as a variable (or a component of one)"
      )

    def subs(x: Term, y: Term): PosZero.type = this
  }

  case class SymbPosWit(name: AnySym, value: LocalTerm)
      extends PosWit
      with Symbolic {
    override def toString: String = name.toString + " : (" + typ.toString + ")"

    def newobj: SymbPosWit = SymbPosWit(InnerSym[Term](this), value)

    def subs(x: Term, y: Term): PosWit =
      if (x == this) y.asInstanceOf[PosWit]
      else {
        def symbobj(sym: AnySym) = (typ.replace(x, y): Pos).symbObj(sym)
        symSubs(symbobj)(x, y)(name)
      }
  }

  case class Pos(value: LocalTerm) extends Typ[PosWit] with Subs[Pos] {
    def subs(x: Term, y: Term) = Pos(value.replace(x, y))

    type Obj = PosWit

    val typ: Universe = Type

    def newobj: Pos = Pos(value.newobj)

    def variable(sym: AnySym) = SymbPosWit(sym, value)
  }

  val x: ScalaTerm[Rational] = "x" :: LocalTyp

  val y: ScalaTerm[Rational] = "y" :: LocalTyp

  lazy val leq
      : FuncLike[ScalaTerm[Rational], FuncLike[ScalaTerm[Rational], Pos]] = x :~> (y :~> Pos(
    y - x
  ))

  // val possum = x :~> (y :~> (Pos(x) ~>: (Pos(y) ~>: Pos(x + y))))
  //
  // val posprod = x :~> (y :~> (Pos(x) ~>: (Pos(y) ~>: Pos(x * y))))

  val dichotomy: FuncLike[ScalaTerm[Rational], Term] =
    "positivity-dichotomy" :: (x ~>: (Pos(x) || Pos(-x)))

  val posAndNegPos
      : FuncLike[ScalaTerm[Rational], FuncLike[Pos, FuncLike[Pos, Equality[
        ScalaTerm[Rational]
      ]]]] =
    "positive-and-negation-positive" :: (
      x ~>: (Pos(x) ~>: (Pos(-x) ~>: (x =:= Literal(0))))
    )

  val squarePositive: FuncLike[ScalaTerm[Rational], PosWit] =
    "square-positive" :: x ~>: Pos(x * x)

  val sumPositive: FuncLike[ScalaTerm[Rational], FuncLike[
    ScalaTerm[Rational],
    Func[PosWit, Func[PosWit, PosWit]]
  ]] =
    "sum-positive" :: x ~>: (y ~>: (Pos(x) ->: Pos(y) ->: Pos(x + y)))

  def showPositive(x: LocalTerm): Option[PosWit] = x match {
    case Literal(a) if a >= 0 => Some(PosLiteral(a))
    case PiTerm(multElems) =>
      if (multElems.exists(_._2 % 2 == 1)) None
      else {
        val sqrt = PiTerm(multElems.map { case (a, n) => (a, n / 2) })
        Some(squarePositive(sqrt))
      }
    case Comb(`sum`, a, b) =>
      for {
        pf1 <- showPositive(a)
        pf2 <- showPositive(b)
      } yield sumPositive(a)(b)(pf1)(pf2)
    case SigmaTerm(elems) =>
      elems.map(y => showPositive(y).map(y -> _)).reduce[Option[(LocalTerm, PosWit)]]{
        case (Some((a, pa)), Some((b, pb))) => Some((a + b) -> sumPositive(a)(b)(pa)(pb))
        case _ => None
      }.map(_._2)
    case _ => None
  }

  val z: ScalaTerm[Rational] = "z" :: LocalTyp

  val w: ScalaTerm[Rational] = "w" :: LocalTyp

  import IdentityTyp.transport

  val transpEqL
      : FuncLike[ScalaTerm[Rational], FuncLike[ScalaTerm[Rational], FuncLike[
        ScalaTerm[Rational],
        Func[Equality[ScalaTerm[Rational]], Func[PosWit, PosWit]]
      ]]] =
    x :~> (
      y :~> (z :~> (transport(w :-> (leq(w)(x)))(y)(z)))
    ) !: x ~>: (
      y ~>: (
        z ~>: (
          (y =:= z) ->: leq(y)(x) ->: leq(z)(x)
        )
      )
    )

  val transpEqR
      : FuncLike[ScalaTerm[Rational], FuncLike[ScalaTerm[Rational], FuncLike[
        ScalaTerm[Rational],
        Func[Equality[ScalaTerm[Rational]], Func[PosWit, PosWit]]
      ]]] =
    x :~> (
      y :~> (z :~> (transport(w :-> (leq(x)(w)))(y)(z)))
    ) !: x ~>: (
      y ~>: (
        z ~>: (
          (y =:= z) ->: leq(x)(y) ->: leq(x)(z)
        )
      )
    )

}
