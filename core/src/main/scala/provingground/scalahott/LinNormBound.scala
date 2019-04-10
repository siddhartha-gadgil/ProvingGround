package provingground.scalahott

import provingground._
import andrewscurtis.FreeGroups._
import HoTT._
import LinNormBound._
import spire.implicits._
import spire.math._
import NatRing.{Literal => nat, leq => leqNat, _}
import QField.{Literal => rat, _}
import FreeGroup.{Literal => elem, _}

sealed abstract class LinNormBound(val word: Word, val bound: Rational) {
  lazy val theorem: Pos = upbound(elem(word))(rat(bound))

  val bnd: QField.LocalTerm with Subs[QField.LocalTerm] = rat(bound)

  val el: FreeGroup.LocalTerm = elem(word)

  val wit: Term

  lazy val proof: PosWit = wit !: theorem

  lazy val fullProof: Term = lambdaClosure(expvars)(proof)

  lazy val fullTheorem : Typ[Term] = fullProof.typ

  def ++(that: LinNormBound): LinNormBound = Triang(this, that)

  def *:(n: Int): LinNormBound = ConjGen(n, this)

  def +:(n: Int): LinNormBound = Gen(n) ++ this
}

object LinNormBound {
  // Some variables
  val g: RepTerm[Word] = "g" :: FreeGroup
  val h: RepTerm[Word] = "h" :: FreeGroup
  val n: RepTerm[SafeLong] = "n" :: NatTyp
  val x: RepTerm[Rational] = "x" :: QTyp
  val y: RepTerm[Rational] = "y" :: QTyp

  // The length function
  val l: Func[RepTerm[Word], RepTerm[Rational]] = "l" :: FreeGroup ->: QTyp

  // Notation
  val upbound: FuncLike[RepTerm[Word], FuncLike[RepTerm[Rational], Pos]] = g :~> (x :~> leq(l(g))(x))
  val pos: Pos = Pos(l(FreeGroup.e))

  // The axioms
  lazy val triang: FuncLike[RepTerm[Word], FuncLike[RepTerm[Word], PosWit]] =
    "triangle-inequality" :: (
      g ~>: (h ~>: (
        (leq(l(g |+| h))(l(g) + l(h)))
      ))
    )

  lazy val conjInv: FuncLike[RepTerm[Word], FuncLike[RepTerm[Word], Equality[RepTerm[Rational]]]] =
    "conjugacy-invariance" :: (
      g ~>: (
        h ~>: (
          (l(h) =:= (l(g |+| h |+| g.inverse)))
        )
      )
    )

  lazy val symmetry: FuncLike[RepTerm[Word], Equality[RepTerm[Rational]]] =
    "symmetry" :: g ~>: {
      l(g) =:= l(g.inverse)
    }

  lazy val powerBound: FuncLike[RepTerm[Word], FuncLike[RepTerm[Rational], FuncLike[RepTerm[SafeLong], FuncLike[PosWit, PosWit]]]] =
    "homogeneity" :: (
      g ~>: (
        x ~>: (
          n ~>: (
            ("hyp" :: (leq(l(FreeGroup.power(g)(n)))(x))) ~>: (
              leq(l(g))(x / nr)
            )
          )
        )
      )
    )

  lazy val gen: Func[RepTerm[SafeLong], RepTerm[Word]] = (NatRing.LocalTyp.rep -->: FreeGroup.rep)((n: SafeLong) =>
    Word(Vector(n.toInt)))

  val NtoQ: Func[NatRing.LocalTerm, QField.LocalTerm] = NatRing.incl(QField)
  val nr: QField.LocalTerm = NtoQ(n)

  lazy val genBound: FuncLike[RepTerm[SafeLong], PosWit] =
    "generator-bound" :: (
      n ~>: (
        leq(l(gen(n)))(rat(1))
      )
    )

  lazy val expvars : Vector[Term] = Vector(l, triang, conjInv, symmetry, powerBound, genBound)

  // Derived lemmas in HoTT
  lazy val triangBound: FuncLike[RepTerm[Word], FuncLike[RepTerm[Word], FuncLike[RepTerm[Rational], FuncLike[RepTerm[Rational], FuncLike[PosWit, FuncLike[PosWit, PosWit]]]]]] = {
    val b1  = "b1" :: leq(l(g))(x)
    val b2  = "b2" :: leq(l(h))(y)
    val res = triang(g)(h) + b1 + b2: PosWit
    g :~> (h :~> (x :~> (y :~> (b1 :~> (b2 :~>
      ((triang(g)(h) + b1 + b2: PosWit) !: leq(l(g |+| h))(x + y))
    // ("triangle-inequality" :: leq(l(g |+| h))(x + y))
    )))))
  }

  lazy val invBound: FuncLike[RepTerm[Word], FuncLike[RepTerm[Rational], FuncLike[PosWit, PosWit]]] = {
    val hyp = "hyp" :: (leq(l(g))(x))
    val bx  = y :-> leq(y)(x)
    g :~> (
      x :~> (
        hyp :~> (
          symmetry(g).lift(bx)(hyp)
          // "inverse-invariance" :: leq(l(g.inverse))(x)
        )
      )
    )
  }

  case class Gen(n: Int) extends LinNormBound(Word(Vector(n)), 1) {
    require(n != 0, "No generator with index 0")

    lazy val wit: PosWit =
      if (n > 0) genBound(nat(n))
      else invBound(gen(nat(-n)))(rat(1))(genBound(nat(-n)))

    override val toString: String = Word(Vector(n)).toString
  }

  case class ConjGen(n: Int, pf: LinNormBound)
      extends LinNormBound(pf.word.conjGen(-n), pf.bound) {
    require(n != 0, "No generator with index 0")

    lazy val wit: PosWit = {
      val g      = if (n > 0) gen(nat(n)) else gen(nat(-n)).inverse
      val conjEq = conjInv(g)(pf.el)
      transpEqL(bnd)(l(pf.el))(l(el))(conjEq)(pf.proof)
    }
  }

  case class Triang(pf1: LinNormBound, pf2: LinNormBound)
      extends LinNormBound(pf1.word ++ pf2.word, pf1.bound + pf2.bound) {
    lazy val wit: PosWit =
      triangBound(pf1.el)(pf2.el)(pf1.bnd)(pf2.bnd)(pf1.proof)(pf2.proof)
  }

  case class PowerBound(baseword: Word, n: Int, pf: LinNormBound)
      extends LinNormBound(baseword, pf.bound / n) {
    require(pf.word == baseword.pow(n),
            s"The element ${pf.word} is not the ${n}th power of $baseword")

    lazy val wit: PosWit =
      powerBound(el)(pf.bnd)(nat(n))(pf.proof)
  }

  // case object Empty extends LinNormBound(Word(Vector()), 0)
}
