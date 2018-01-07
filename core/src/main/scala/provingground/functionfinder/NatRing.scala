package provingground.functionfinder

import provingground._, HoTT._

import ScalaRep._

import spire.algebra._
import spire.math._
import spire.implicits._
import scala.util._
import scala.language.implicitConversions
import annotation.tailrec

object QField extends SymbolicField[Rational] {
  val QTyp = LocalTyp

  // val nat =
  //   (NatRing.LocalTyp.rep -->: LocalTyp.rep)((n: SafeLong) => Rational(n, 1))

  val Pos = "Positive" :: LocalTyp ->: Type

  val x = "x" :: LocalTyp

  val y = "y" :: LocalTyp

  val leq = x :~> (y :~> Pos(y - x))

  val possum = x :~> (y :~> (Pos(x) ~>: (Pos(y) ~>: Pos(x + y))))

  val posprod = x :~> (y :~> (Pos(x) ~>: (Pos(y) ~>: Pos(x * y))))

  val dichotomy = x ~>: (Pos(x) || Pos(-x))

  val posAndNegPos = x ~>: (Pos(x) ~>: (Pos(-x) ~>: (x =:= Literal(0))))

  def posLiteral(a: Rational) = {
    require(a >= 0, s"Rational number $a not positive")
    "verified" :: Pos(Literal(a))
  }

  val z = "z" :: LocalTyp

  val w = "w" :: LocalTyp

  import IdentityTyp.transport

  val transpEqL =
    x :~> (
      y :~> (z :~> (transport(w :-> (leq(w)(x)))(y)(z)))
    ) !: x ~>: (
      y ~>: (
        z ~>: (
          (y =:= z) ->: leq(y)(x) ->: leq(z)(x)
        )
      )
    )

  val transpEqR =
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

object NatRing extends SymbolicCRing[SafeLong] {
  override def toString = "Nat"
  val x                 = "x" :: LocalTyp
  val succ              = lmbda(x)(x + 1)

  val NatTyp = LocalTyp

  type Nat = LocalTerm

  implicit def intLiteral(n: Int): Nat = Literal(n)

  def recDefn[U <: Term with Subs[U]](n: SafeLong,
                                      formal: U,
                                      h: SafeLong => U => U): U =
    if (n == 0) formal else h(n - 1)(recDefn(n - 1, formal, h))

  case class Rec[U <: Term with Subs[U]](init: U, g: Func[Nat, Func[U, U]])
      extends Func[Nat, U] { self =>
    def h = (n: SafeLong) => g(Literal(n))

    val dom   = NatTyp
    val codom = init.typ.asInstanceOf[Typ[U]]

    val typ = dom ->: codom

    def subs(x: Term, y: Term) = Rec(init.replace(x, y), g.replace(x, y))
    def newobj                 = this

    def act(x: LocalTerm) = x match {
      case Literal(n) => recDefn(n, init, h)
      case LiteralSum(n, x) =>
        recDefn(n, Rec(init, g)(x), (k: SafeLong) => g(sum(Literal(k))(x)))
      case _ => FormalAppln[Nat, U](self, x)
    }
  }

  case class Induc[U <: Term with Subs[U]](typFamily: Func[Nat, Typ[U]],
                                           init: U,
                                           g: FuncLike[Nat, Func[U, U]])
      extends FuncLike[Nat, U] { self =>
    def h = (n: SafeLong) => g(Literal(n))

    val dom = NatTyp

    val typ = PiDefn(typFamily)

    val depcodom = typFamily

    def subs(x: Term, y: Term) = Induc(typFamily.replace(x, y), init.replace(x, y), g.replace(x, y))
    def newobj                 = this

    def act(x: LocalTerm) = x match {
      case Literal(n) => recDefn(n, init, h)
      case LiteralSum(n, x) =>
        recDefn(n,
                Induc(typFamily, init, g)(x),
                (k: SafeLong) => g(sum(Literal(k))(x)))
      case _ => FormalAppln[Nat, U](self, x)
    }
  }

  object Induc {
    def cast[U <: Term with Subs[U]](typFamily: Func[Nat, Typ[U]],
                                     init: U,
                                     g: FuncLike[Nat, FuncLike[U, U]]) =
      Induc(typFamily, init, g.asInstanceOf[FuncLike[Nat, Func[U, U]]])
  }

  def incl[A: CRing](r: SymbolicCRing[A]) = {
    val base = implicitly[Ring[A]]
    val n    = "n" :: LocalTyp
    val fn   = "f(n)" :: r.LocalTyp
    val step = n :-> (fn :-> r.sum(fn)(r.Literal(base.one)))
    Rec(r.Literal(base.zero), step)
  }
}
