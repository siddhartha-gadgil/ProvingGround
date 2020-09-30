package provingground.scalahott

import provingground._, HoTT._
import ScalaRep._
// import translation._
//import scala.language.{existentials, implicitConversions}
import scala.util._
// import spire.algebra._
// import spire.implicits._
//import spire.syntax._
import cats.kernel._, cats.implicits._, cats.syntax._

class SymbolicGroup[A: Group] extends ScalaTyp[A] { self =>
  val group = implicitly[Group[A]]

  type LocalTerm = RepTerm[A]

  type Op = Func[LocalTerm, Func[LocalTerm, LocalTerm]]

  val e = group.empty.term

  object Comb {
    def unapply(term: Term): Option[(LocalTerm, LocalTerm)] = term match {
      case MiscAppln(MiscAppln(`mul`, x), y) =>
        Try((x.asInstanceOf[LocalTerm], y.asInstanceOf[LocalTerm])).toOption
      case _ => None
    }

    def apply(x: LocalTerm, y: LocalTerm) = FormalAppln(FormalAppln(mul, x), y)
  }

  object Literal extends ScalaSym[LocalTerm, A](self)

  case object inv extends Func[LocalTerm, LocalTerm] {
    override val toString = "inv"

    val dom = self

    val codom = self

    val typ = self ->: self

    def subs(x: Term, y: Term) = this

    def newobj =
      throw new IllegalArgumentException(
        s"trying to use the constant $this as a variable (or a component of one)")

    def act(y: LocalTerm) = y match {
      case Literal(b)                     => Literal(group.inverse(b))
      case MiscAppln(`inv`, p: LocalTerm) => p
      case Comb(x, y)                     => mul(inv(y))(inv(x))
      case p                              => FormalAppln(inv, p)
    }
  }

  case class MultLiteral(a: A)
      extends Func[LocalTerm, LocalTerm]
      with MiscAppln {
    val func = mul

    val arg = Literal(a)

    val dom = self

    val codom = self

    val typ = self ->: self

    def subs(x: Term, y: Term) = this

    def newobj =
      throw new IllegalArgumentException(
        s"trying to use the constant $this as a variable (or a component of one)")

    def act(y: LocalTerm) = y match {
      case Literal(b)          => Literal(group.combine(a, b))
      case Comb(Literal(b), v) => MultLiteral(group.combine(a, b))(v)
      case p =>
        FormalAppln(this, p)
//        Comb(Literal(a), p)
    }
  }

  case class MultTerm(a: LocalTerm)
      extends Func[LocalTerm, LocalTerm]
      with MiscAppln {
    val func = mul

    val arg = a

    val dom = self

    lazy val ia = inv(a)

    val codom = self

    val typ = self ->: self

    def subs(x: Term, y: Term) = mul(a.replace(x, y))

    def newobj =
      throw new IllegalArgumentException(
        s"trying to use the constant $this as a variable (or a component of one)")

    def act(y: LocalTerm) = y match {
      case uv @ Comb(u, v) =>
        if (u == ia) v
        else
          FormalAppln(this, uv)
//          Comb(a, Comb(u, v))
      case `e` => a
      case p =>
        if (p == ia) e
        else
          FormalAppln(this, p)
//          Comb(a, p)
    }
  }

  case object mul extends Func[LocalTerm, Func[LocalTerm, LocalTerm]] { w =>
    val dom: SymbolicGroup[A] = self

    val codom: FuncTyp[LocalTerm, RepTerm[A]] = self ->: self

    val typ: FuncTyp[LocalTerm, Func[LocalTerm, RepTerm[A]]] = dom ->: codom

    def subs(x: Term, y: Term): mul.type = this

    def newobj =
      throw new IllegalArgumentException(
        s"trying to use the constant $this as a variable (or a component of one)")

    def act(y: LocalTerm): Func[LocalTerm, LocalTerm] = y match {
      case `e`        => HoTT.id(self)
      case Literal(a) => MultLiteral(a)
      case Comb(u, v) =>
        val x = self.Var
        x :-> mul(u)(mul(v)(x))
      case p => MultTerm(p)
    }

    override val toString = "mul"
  }

  implicit val groupStructure: Group[LocalTerm] = new Group[LocalTerm] {
    val empty: RepTerm[A] = e

    def combine(x: LocalTerm, y: LocalTerm): LocalTerm = mul(x)(y)

    def inverse(x: LocalTerm): LocalTerm = inv(x)

  }

  val g = "g" :: self

  val h = "h" :: self

  val lm = g :-> (h :-> (g |+| h))

  val rm = g :-> (h :-> (h |+| g))

  val power = {
    import NatRing._
    val g                                          = "g" :: self
    val n                                          = "n" :: NatTyp
    val fng                                        = "f(n)(g)" :: self
    val step                                       = n :-> (fng :-> mul(fng)(g))
    val recFn: Func[NatRing.LocalTerm, RepTerm[A]] = Rec(e, step)
    g :-> recFn
  }
  object Theorems {
    import NatRing.{Literal => nat, power => _, _}

    lazy val n = "n" :: NatTyp
    lazy val m = "m" :: NatTyp

    lazy val g = "g" :: self
    lazy val h = "h" :: self

    object ConjPower {
      lazy val ghthm = n :-> ((power(g |+| h |+| g.inverse())(n)) =:= (g |+| power(
        h)(n) |+| g.inverse()))

      lazy val hyp  = "hyp" :: ghthm(n)
      lazy val c    = g |+| h |+| g.inverse()
      lazy val base = e.refl
      lazy val step = n :~> (hyp :-> (self.rm(c) *: hyp))
      lazy val ghpf = Induc(ghthm, base, step)
      lazy val pf   = g :~> (h :~> ghpf)
      lazy val thm  = g ~>: (h ~>: (n ~>: (ghthm(n))))
    }

    object PowerDistributive {
      lazy val gthm = n :-> ((self.power(g)(m) |+| self.power(g)(n)) =:= self
        .power(g)(NatRing.sum(m)(n)))

      lazy val hyp = "hyp" :: gthm(n)

      lazy val base = self.power(g)(m).refl
      lazy val step = n :~> (hyp :-> (self.rm(g) *: hyp))
      lazy val gpf  = Induc(gthm, base, step)

      lazy val pf  = g :~> (m :~> gpf)
      lazy val thm = g ~>: (m ~>: (n ~>: gthm(n)))
    }
  }

}

import andrewscurtis.FreeGroups._

object FreeGroup extends SymbolicGroup[Word] {
  def word(s: String) = Literal(Word.fromString(s))

  override def toString = "FreeGroup"
}
