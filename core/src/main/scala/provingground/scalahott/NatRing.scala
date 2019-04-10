package provingground.scalahott

import provingground._
import HoTT._

import provingground.induction.{ExstInducDefn, ExstInducStrucs}
import spire.algebra._
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

  val x: RepTerm[Rational] = "x" :: LocalTyp

  val y: RepTerm[Rational] = "y" :: LocalTyp

  lazy val leq
      : FuncLike[RepTerm[Rational], FuncLike[RepTerm[Rational], Pos]] = x :~> (y :~> Pos(
    y - x
  ))

  // val possum = x :~> (y :~> (Pos(x) ~>: (Pos(y) ~>: Pos(x + y))))
  //
  // val posprod = x :~> (y :~> (Pos(x) ~>: (Pos(y) ~>: Pos(x * y))))

  val dichotomy: FuncLike[RepTerm[Rational], Term] =
    "positivity-dichotomy" :: (x ~>: (Pos(x) || Pos(-x)))

  val posAndNegPos
      : FuncLike[RepTerm[Rational], FuncLike[Pos, FuncLike[Pos, Equality[
        RepTerm[Rational]
      ]]]] =
    "positive-and-negation-positive" :: (
      x ~>: (Pos(x) ~>: (Pos(-x) ~>: (x =:= Literal(0))))
    )

  val z: RepTerm[Rational] = "z" :: LocalTyp

  val w: RepTerm[Rational] = "w" :: LocalTyp

  import IdentityTyp.transport

  val transpEqL
      : FuncLike[RepTerm[Rational], FuncLike[RepTerm[Rational], FuncLike[
        RepTerm[Rational],
        Func[Equality[RepTerm[Rational]], Func[PosWit, PosWit]]
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
      : FuncLike[RepTerm[Rational], FuncLike[RepTerm[Rational], FuncLike[
        RepTerm[Rational],
        Func[Equality[RepTerm[Rational]], Func[PosWit, PosWit]]
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

object NatRing extends SymbolicCRing[SafeLong] with ExstInducStrucs {
  type Nat = LocalTerm

  override def toString    = "Nat"
  val x: Nat               = "x" :: LocalTyp
  val succ: Func[Nat, Nat] = lmbda(x)(x + 1)

  val zero: Nat = Literal(0)

  val NatTyp: NatRing.LocalTyp.type = LocalTyp

  implicit def intLiteral(n: Int): Nat = Literal(n)

  val leq: FuncLike[Nat, FuncLike[Nat, SigmaTyp[Nat, Equality[Nat]]]] = {
    val x = LocalTyp.Var
    val y = LocalTyp.Var
    val z = LocalTyp.Var
    x :~> (y :~> (z &: (sum(z)(x) =:= y)))
  }

  object LEQ {
    def unapply(arg: Typ[Term]): Option[(Nat, Nat)] = arg match {
      case sg: SigmaTyp[u, v] if sg.fibers.dom == NatTyp =>
        val z = NatTyp.Var
        sg.fibers(z.asInstanceOf[u]) match {
          case IdentityTyp(NatTyp, lhs: Nat, y: Nat) =>
            findDifference(lhs, z).map { x =>
              x -> y
            }
          case _ => None
        }
      case _ => None
    }
  }

  object DIV {
    def unapply(arg: Typ[Term]): Option[(Nat, Nat)] = arg match {
      case sg: SigmaTyp[u, v] if sg.fibers.dom == NatTyp =>
        val z = NatTyp.Var
        sg.fibers(z.asInstanceOf[u]) match {
          case IdentityTyp(NatTyp, lhs: Nat, y: Nat) =>
            findFactor(z, lhs).map { x =>
              x -> y
            }
          case _ => None
        }
      case _ => None
    }
  }


  def recDefn[U <: Term with Subs[U]](
      n: SafeLong,
      formal: U,
      h: SafeLong => U => U
  ): U =
    if (n == 0) formal else h(n - 1)(recDefn(n - 1, formal, h))

  case class Rec[U <: Term with Subs[U]](init: U, g: Func[Nat, Func[U, U]])
      extends RecFunc[Nat, U] { self =>
    def h: SafeLong => Func[U, U] = (n: SafeLong) => g(Literal(n))

    val defnData: Vector[Term] = Vector(init, g)

    def fromData(data: Vector[Term]): RecFunc[Nat, U] =
      Rec(data(0).asInstanceOf[U], data(1).asInstanceOf[Func[Nat, Func[U, U]]])

    val dom: NatRing.LocalTyp.type = NatTyp
    val codom: Typ[U]              = init.typ.asInstanceOf[Typ[U]]

    val typ: FuncTyp[Nat, U] = dom ->: codom

    def subs(x: Term, y: Term): Rec[U] =
      Rec(init.replace(x, y), g.replace(x, y))
    def newobj: Rec[U] = this

    def act(x: Nat): U = x match {
      case Literal(n) => recDefn(n, init, h)
      case LiteralSum(n, a) =>
        recDefn(n, Rec(init, g)(a), (k: SafeLong) => g(sum(Literal(k))(a)))
      case _ => FormalAppln[Nat, U](self, x)
    }
  }

  case class Induc[U <: Term with Subs[U]](
      typFamily: Func[Nat, Typ[U]],
      init: U,
      g: FuncLike[Nat, Func[U, U]]
  ) extends InducFuncLike[Nat, U]
      with Subs[Induc[U]] { self =>
    def h: SafeLong => Func[U, U] = (n: SafeLong) => g(Literal(n))

    val dom: NatRing.LocalTyp.type = NatTyp

    val typ = PiDefn(typFamily)

    val defnData: Vector[Term] = Vector(init, g)

    def fromData(data: Vector[Term]): InducFuncLike[Nat, U] =
      Induc(
        typFamily,
        data(0).asInstanceOf[U],
        data(1).asInstanceOf[FuncLike[Nat, Func[U, U]]]
      )

    val depcodom: Func[Nat, Typ[U]] = typFamily

    def subs(x: Term, y: Term) =
      Induc(typFamily.replace(x, y), init.replace(x, y), g.replace(x, y))
    def newobj: Induc[U] = this

    def act(x: Nat): U = x match {
      case Literal(n) => recDefn(n, init, h)
      case LiteralSum(n, a) =>
        recDefn(
          n,
          Induc(typFamily, init, g)(a),
          (k: SafeLong) => g(sum(Literal(k))(a))
        )
      case _ => FormalAppln[Nat, U](self, x)
    }
  }

  object Induc {
    def cast[U <: Term with Subs[U]](
        typFamily: Func[Nat, Typ[U]],
        init: U,
        g: FuncLike[Nat, FuncLike[U, U]]
    ) =
      Induc(typFamily, init, g.asInstanceOf[FuncLike[Nat, Func[U, U]]])
  }

  def rec[U <: Term with Subs[U]](
      codom: Typ[U]
  ): Func[U, Func[Func[Nat, Func[U, U]], Func[Nat, U]]] = {
    val init: U             = codom.Var
    val g                   = (NatTyp ->: (codom ->: codom)).Var
    val value: Func[Nat, U] = Rec(init, g)
    init :-> (g :-> value)
  }

  def induc[U <: Term with Subs[U]](
      typFamily: Func[Nat, Typ[U]]
  ): Func[U, Func[FuncLike[Nat, Func[U, U]], FuncLike[Nat, U]]] = {
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
          case _: Typ[w] =>
            Some(induc(typFamily.asInstanceOf[Func[Nat, Typ[u]]]))
          case _ => None
        }

      case _ => None
    } else None

  def subs(x: Term, y: Term): ExstInducStrucs = this

  override val constants: Vector[Term] = Vector(zero, succ)

  def incl[A: CRing](r: SymbolicCRing[A]): Func[Nat, r.LocalTerm] = {
    val base = implicitly[Ring[A]]
    val n    = "n" :: LocalTyp
    val fn   = "f(n)" :: r.LocalTyp
    val step: Func[Nat with Subs[Nat], Func[
      RepTerm[A] with Subs[RepTerm[A]],
      r.LocalTerm
    ]] = n :-> (fn :-> r
      .sum(fn)(r.Literal(base.one)))
    Rec(r.Literal(base.zero), step)
  }

  lazy val exstInducDefn: ExstInducDefn =
    ExstInducDefn(Type, Vector(zero, succ), this)

  lazy val context: Context = Context.Empty
    .defineInduc(this)
    .defineSym(Name("zero"), Literal(0))
    .defineSym(Name("succ"), succ)
    .defineSym(Name("NatTyp"), NatTyp: Typ[Term])("sum" -> sum)("prod" -> prod)

  def findFactor(x: Nat, y: Nat): Option[Nat] =
    (x, y) match {
      case (a, b) if a == b => Some(Literal(1))
      case (Literal(a), Literal(b)) if b                  % a == 0 => Some(Literal(b / a))
      case (Literal(a), Comb(`prod`, Literal(b), c)) if b % a == 0 =>
        Some(prod(Literal(b / a))(c))
      case (Comb(`prod`, Literal(a), d), Comb(`prod`, Literal(b), e))
          if b % a == 0 =>
        findFactor(d, e).map { c =>
          (prod(Literal(b / a))(c))
        }
      case (PiTerm(m1), p2 @ PiTerm(_)) =>
        val m = m1.map { case (el, n) => el -> (p2.exponent(el) - n) }
        if (m.exists(_._2 < 0)) None else Some(PiTerm.reduce(m.toVector))
      case (el, p @ PiTerm(m)) =>
        if (p.exponent(el) > 0)
          Some(PiTerm.reduce((m + (el -> (p.exponent(el) - 1))).toVector))
        else None
      case _ => None
    }

  def findDivisibilty(x: Nat, y: Nat): Option[AbsPair[Nat, Equality[Nat]]] =
    findFactor(x, y).map { z: Nat =>
      DepPair(z, y.refl, divides(x)(y).fibers) !: divides(x)(y)
    }

  def findDifferenceFlip(x: Nat, y: Nat): Option[Nat] =
    (x, y) match {
      case (a, b) if a == b                   => Some(Literal(0))
      case (Literal(a), Literal(b)) if a <= b => Some(Literal(b - a))
      case (Literal(a), Comb(`sum`, Literal(b), c)) if a <= b =>
        Some(sum(Literal(b - a))(c))
      case (Comb(`sum`, Literal(a), d), Comb(`sum`, Literal(b), e))
          if a <= b  =>
        findDifferenceFlip(d, e).map { c =>
          (sum(Literal(b - a))(c))
        }
      case (d, Comb(`sum`, Literal(b), e))  =>
        findDifferenceFlip(d, e).map { c =>
          (sum(Literal(b))(c))
        }
      case (s: SigmaTerm, t) =>
        findDifferenceFlip(s.head, t).flatMap { d =>
          findDifferenceFlip(s.tail, d)
        }
      case (t, s: SigmaTerm) => // we know that `t` is not a sigma-term or a literal
        if (s.elems.contains(t)) Some(SigmaTerm.reduce(s.elems - t)) else None
      case _ => None
    }

  def findDifference(x: Nat, y: Nat): Option[Nat] = findDifferenceFlip(y, x)

  def findLEQ(x: Nat, y: Nat): Option[DepPair[Nat, Equality[Nat]]] =
    findDifferenceFlip(x, y).map { d =>
      DepPair(d, y.refl, leq(x)(y).fibers) !: leq(x)(y)
    }
}
