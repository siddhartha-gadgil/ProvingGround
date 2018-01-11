package provingground.functionfinder

import provingground._, andrewscurtis.FreeGroups._

import HoTT._

import LinNormBound._

import spire.implicits._

import spire.math._

import NatRing.{Literal => nat, _}, QField.{Literal => rat, _},
FreeGroup.{Literal => elem, _}

sealed abstract class LinNormBound(val word: Word, val bound: Rational) {
  lazy val theorem = upbound(elem(word))(rat(bound))

  val bnd = rat(bound)

  val el = elem(word)

  val wit: Term

  lazy val proof = wit !: theorem

  lazy val fullProof = lambdaClosure(expvars)(proof)

  lazy val fullTheorem = fullProof.typ

  def ++(that: LinNormBound) = Triang(this, that)

  def *:(n: Int) = ConjGen(n, this)

  def +:(n: Int) = Gen(n) ++ this
}

object LinNormBound {
  // Some variables
  val g = "g" :: FreeGroup
  val h = "h" :: FreeGroup
  val n = "n" :: NatTyp
  val x = "x" :: QTyp
  val y = "y" :: QTyp

  // The length function
  val l = "l" :: FreeGroup ->: QTyp

  // Notation
  val upbound = g :~> (x :~> leq(l(g))(x))
  val pos     = Pos(l(FreeGroup.e))

  // The axioms
  lazy val triang =
    "triangle-inequality" :: (
      g ~>: (h ~>: (
        (leq(l(g |+| h))(l(g) + l(h)))
      ))
    )

  lazy val conjInv =
    "conjugacy-invariance" :: (
      g ~>: (
        h ~>: (
          (l(h) =:= (l(g |+| h |+| g.inverse)))
        )
      )
    )

  lazy val symmetry =
    "symmetry" :: g ~>: {
      l(g) =:= l(g.inverse)
    }

  lazy val powerBound =
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

  lazy val gen = (NatRing.LocalTyp.rep -->: FreeGroup.rep)((n: SafeLong) =>
    Word(Vector(n.toInt)))

  val NtoQ = NatRing.incl(QField)
  val nr   = NtoQ(n)

  lazy val genBound =
    "generator-bound" :: (
      n ~>: (
        leq(l(gen(n)))(rat(1))
      )
    )

  lazy val expvars = Vector(l, triang, conjInv, symmetry, powerBound, genBound)

  // Derived lemmas in HoTT
  lazy val triangBound = {
    val b1  = "b1" :: leq(l(g))(x)
    val b2  = "b2" :: leq(l(h))(y)
    val res = triang(g)(h) + b1 + b2: PosWit
    g :~> (h :~> (x :~> (y :~> (b1 :~> (b2 :~>
      ((triang(g)(h) + b1 + b2: PosWit) !: leq(l(g |+| h))(x + y))
    // ("triangle-inequality" :: leq(l(g |+| h))(x + y))
    )))))
  }

  lazy val invBound = {
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

    lazy val wit =
      if (n > 0) genBound(nat(n))
      else invBound(gen(nat(-n)))(rat(1))(genBound(nat(-n)))

    override val toString = Word(Vector(n)).toString
  }

  case class ConjGen(n: Int, pf: LinNormBound)
      extends LinNormBound(pf.word.conjGen(-n), pf.bound) {
    require(n != 0, "No generator with index 0")

    lazy val wit = {
      val g      = if (n > 0) gen(nat(n)) else gen(nat(-n)).inverse
      val conjEq = conjInv(g)(pf.el)
      transpEqL(bnd)(l(pf.el))(l(el))(conjEq)(pf.proof)
    }
  }

  case class Triang(pf1: LinNormBound, pf2: LinNormBound)
      extends LinNormBound(pf1.word ++ pf2.word, pf1.bound + pf2.bound) {
    lazy val wit =
      triangBound(pf1.el)(pf2.el)(pf1.bnd)(pf2.bnd)(pf1.proof)(pf2.proof)
  }

  case class PowerBound(baseword: Word, n: Int, pf: LinNormBound)
      extends LinNormBound(baseword, pf.bound / n) {
    require(pf.word == baseword.pow(n),
            s"The element ${pf.word} is not the ${n}th power of $baseword")

    lazy val wit =
      powerBound(el)(pf.bnd)(nat(n))(pf.proof)
  }

  // case object Empty extends LinNormBound(Word(Vector()), 0)
}
