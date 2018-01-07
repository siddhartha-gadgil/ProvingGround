package provingground.functionfinder

import provingground._, andrewscurtis.FreeGroups._

import LinNormBound._

import spire.implicits._

import spire.math._

import NatRing.{Literal => nat, _}, QField.{Literal => rat, _},
FreeGroup.{Literal => elem, _}

sealed abstract class LinNormBound(val word: Word, val bound: Rational) {
  val theorem = upbound(elem(word))(rat(bound))

  def ++(that: LinNormBound) = Triang(this, that)

  def *:(n: Int) = ConjGen(n, this)

  def +:(n: Int) = Gen(n) ++ this
}

object LinNormBound {
  val g = "g" :: FreeGroup
  val h = "h" :: FreeGroup
  val n = "n" :: NatTyp
  val x = "x" :: QTyp
  val y = "y" :: QTyp

  val l       = "l" :: FreeGroup ->: QTyp
  val upbound = g :-> (x :-> leq(l(g))(x))

  val pos = Pos(l(FreeGroup.e))

  val triang =
    g :-> (h :-> (x :-> (y :-> (("b1" :: leq(l(g))(x)) :-> (("b2" :: leq(l(h))(
      y)) :-> ("axiom" :: leq(l(g |+| h))(x + y)))))))

  val conjBound =
    g :-> (
      h :-> (
        x :-> (
          ("hyp" :: (leq(l(h))(x))) :-> (
            "axiom" :: (leq(l(g |+| h |+| g.inverse))(x))
          )
        )
      )
    )

  val invBound =
    g :-> (
      x :-> (
        ("hyp" :: leq(l(g))(x)) :-> (
          "axiom" :: leq(l(g.inverse))(x)
        )
      )
    )

  val gen = (NatRing.LocalTyp.rep -->: FreeGroup.rep)((n: SafeLong) =>
    Word(Vector(n.toInt)))

  val genBound =
    n :-> (
      leq(l(gen(n)))(rat(1))
    )

  val NtoQ = NatRing.incl(QField)
  val nr   = NtoQ(n)
  val gn   = FreeGroup.power(g)(n)

  val powerBound =
    g :-> (
      x :-> (
        n :-> (
          ("hyp" :: (leq(l(g))(x))) :-> (
            "axiom" :: leq(l(gn))(x / nr)
          )
        )
      )
    )

  case class Gen(n: Int) extends LinNormBound(Word(Vector(n)), 1) {
    require(n != 0, "No generator with index 0")

    override val toString = Word(Vector(n)).toString
  }

  case class ConjGen(n: Int, pf: LinNormBound)
      extends LinNormBound(n +: pf.word :+ (-n), pf.bound) {
    require(n != 0, "No generator with index 0")
  }

  case class Triang(pf1: LinNormBound, pf2: LinNormBound)
      extends LinNormBound(pf1.word ++ pf2.word, pf1.bound + pf2.bound)

  case class PowerBound(baseword: Word, n: Int, pf: LinNormBound)
      extends LinNormBound(baseword, pf.bound / n) {
    require(pf.word == baseword.pow(n),
            s"The element ${pf.word} is not the ${n}th power of $baseword")
  }

  // case object Empty extends LinNormBound(Word(Vector()), 0)
}