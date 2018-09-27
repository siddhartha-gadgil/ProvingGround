package provingground.scalahott

import provingground._
import HoTT._
import ScalaRep._
import provingground.induction.{ExstInducDefn, ExstInducStruc}
import spire.algebra._
import spire.math._
import spire.implicits._

import scala.util._
import scala.language.implicitConversions
import annotation.tailrec

object QField extends SymbolicField[Rational] {
  override def toString = "Q"

  val QTyp = LocalTyp

  sealed trait PosWit extends Term with Subs[PosWit] {
    val value: LocalTerm

    lazy val typ = Pos(value)

    def +(that: PosWit) = PosWitSum(this, that)
  }

  case class PosWitSum(a: PosWit, b: PosWit) extends PosWit {
    lazy val value = a.value + b.value

    def newobj =
      throw new IllegalArgumentException(
        s"trying to use the constant $this as a variable (or a component of one)")

    def subs(x: Term, y: Term) = PosWitSum(a.replace(x, y), b.replace(x, y))
  }

  case class PosWitProd(a: PosWit, b: PosWit) extends PosWit {
    lazy val value = a.value * b.value

    def newobj =
      throw new IllegalArgumentException(
        s"trying to use the constant $this as a variable (or a component of one)")

    def subs(x: Term, y: Term) = PosWitSum(a.replace(x, y), b.replace(x, y))
  }

  case class PosLiteral(a: Rational) extends PosWit {
    require(a >= 0, s"Rational number $a not positive")

    val value = Literal(a)

    def newobj =
      throw new IllegalArgumentException(
        s"trying to use the constant $this as a variable (or a component of one)")

    def subs(x: Term, y: Term) = this
  }

  case object PosZero extends PosWit {

    val value = Literal(0)

    def newobj =
      throw new IllegalArgumentException(
        s"trying to use the constant $this as a variable (or a component of one)")

    def subs(x: Term, y: Term) = this
  }

  case class SymbPosWit(name: AnySym, value: LocalTerm)
      extends PosWit
      with Symbolic {
    override def toString = name.toString + " : (" + typ.toString + ")"

    def newobj = SymbPosWit(InnerSym[Term](this), value)

    def subs(x: Term, y: Term) =
      if (x == this) y.asInstanceOf[PosWit]
      else {
        def symbobj(sym: AnySym) = (typ.replace(x, y): Pos).symbObj(sym)
        symSubs(symbobj)(x, y)(name)
      }
  }

  case class Pos(value: LocalTerm) extends Typ[PosWit] with Subs[Pos] {
    def subs(x: Term, y: Term) = Pos(value.replace(x, y))

    type Obj = PosWit

    val typ = Type

    def newobj = Pos(value.newobj)

    def variable(sym: AnySym) = SymbPosWit(sym, value)
  }

  val x = "x" :: LocalTyp

  val y = "y" :: LocalTyp

  lazy val leq = x :~> (y :~> Pos(y - x))

  // val possum = x :~> (y :~> (Pos(x) ~>: (Pos(y) ~>: Pos(x + y))))
  //
  // val posprod = x :~> (y :~> (Pos(x) ~>: (Pos(y) ~>: Pos(x * y))))

  val dichotomy =
    "positivity-dichotomy" :: (x ~>: (Pos(x) || Pos(-x)))

  val posAndNegPos =
    "positive-and-negation-positive" :: (
      x ~>: (Pos(x) ~>: (Pos(-x) ~>: (x =:= Literal(0))))
    )

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

object NatRing extends SymbolicCRing[SafeLong] with ExstInducStruc {
  override def toString                = "Nat"
  val x: LocalTerm                     = "x" :: LocalTyp
  val succ: Func[LocalTerm, LocalTerm] = lmbda(x)(x + 1)

  val zero = Literal(0)

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

    val dom           = NatTyp
    val codom: Typ[U] = init.typ.asInstanceOf[Typ[U]]

    val typ: FuncTyp[LocalTerm, U] = dom ->: codom

    def subs(x: Term, y: Term) = Rec(init.replace(x, y), g.replace(x, y))
    def newobj: Rec[U]         = this

    def act(x: LocalTerm): U = x match {
      case Literal(n) => recDefn(n, init, h)
      case LiteralSum(n, x) =>
        recDefn(n, Rec(init, g)(x), (k: SafeLong) => g(sum(Literal(k))(x)))
      case _ => FormalAppln[Nat, U](self, x)
    }
  }

  case class Induc[U <: Term with Subs[U]](typFamily: Func[Nat, Typ[U]],
                                           init: U,
                                           g: FuncLike[Nat, Func[U, U]])
      extends FuncLike[Nat, U]
      with Subs[Induc[U]] { self =>
    def h: SafeLong => Func[U, U] = (n: SafeLong) => g(Literal(n))

    val dom = NatTyp

    val typ = PiDefn(typFamily)

    val depcodom: Func[Nat, Typ[U]] = typFamily

    def subs(x: Term, y: Term) =
      Induc(typFamily.replace(x, y), init.replace(x, y), g.replace(x, y))
    def newobj: Induc[U] = this

    def act(x: LocalTerm): U = x match {
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

  def rec[U <: Term with Subs[U]](codom: Typ[U])
    : Func[U, Func[Func[LocalTerm, Func[U, U]], Func[Nat, U]]] = {
    val init: U             = codom.Var
    val g                   = (NatTyp ->: (codom ->: codom)).Var
    val value: Func[Nat, U] = Rec(init, g)
    init :-> (g :-> value)
  }

  def induc[U <: Term with Subs[U]](typFamily: Func[Nat, Typ[U]])
    : Func[U, Func[FuncLike[LocalTerm, Func[U, U]], FuncLike[Nat, U]]] = {
    val init: U = typFamily(zero).Var
    val n       = NatTyp.Var
    val g       = (n ~>: (typFamily(n) ->: typFamily(succ(n)))).Var
    init :-> (g :-> Induc(typFamily, init, g))

  }

  def recOpt[C <: Term with Subs[C]](dom: Term, cod: Typ[C]): Option[Term] =
    if (dom == NatTyp) Some(rec(cod)) else None

  def inducOpt(dom: Term, cod: Term): Option[Term] =
    if (dom == NatTyp) cod match {
      case typFamily: Func[u, _] =>
        typFamily(zero.asInstanceOf[u]) match {
          case _ : Typ[w] => Some(induc(typFamily.asInstanceOf[Func[Nat, Typ[u]]]))
          case _ => None
        }

      case _ => None
    } else None

  def subs(x: Term, y: Term): ExstInducStruc = this

  override val constants: Vector[Term] = Vector(zero, succ)

  def incl[A: CRing](r: SymbolicCRing[A]): Func[LocalTerm, r.LocalTerm] = {
    val base = implicitly[Ring[A]]
    val n    = "n" :: LocalTyp
    val fn   = "f(n)" :: r.LocalTyp
    val step: Func[
      LocalTerm with Subs[LocalTerm],
      Func[RepTerm[A] with Subs[RepTerm[A]], r.LocalTerm]] = n :-> (fn :-> r
      .sum(fn)(r.Literal(base.one)))
    Rec(r.Literal(base.zero), step)
  }

  lazy val exstInducDefn = ExstInducDefn(Type, Vector(zero, succ), this, Vector())
}


