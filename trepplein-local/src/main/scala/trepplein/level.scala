package trepplein

import trepplein.Level.{IMax, Max, _}

import scala.annotation.tailrec
import scala.language.implicitConversions

sealed abstract class Level {
  def dump: String =
    this match {
      case Zero         => "Level.Zero"
      case Succ(level)  => s"Level.Succ(${level.dump})"
      case Max(a, b)    => s"Level.Max(${a.dump}, ${b.dump})"
      case IMax(a, b)   => s"Level.IMax(${a.dump}, ${b.dump})"
      case Param(param) => s"Level.Param(${param.dump})"
    }

  def instantiate(subst: Map[Param, Level]): Level =
    this match {
      case Zero        => Zero
      case Succ(level) => Succ(level.instantiate(subst))
      case Max(a, b)   => Max(a.instantiate(subst), b.instantiate(subst))
      case IMax(a, b)  => IMax(a.instantiate(subst), b.instantiate(subst))
      case p: Param    => subst.getOrElse(p, p)
    }

  def univParams: Set[Param] =
    this match {
      case Zero         => Set()
      case Succ(level)  => level.univParams
      case Max(a, b)    => a.univParams union b.univParams
      case IMax(a, b)   => a.univParams union b.univParams
      case param: Param => Set(param)
    }

  def simplify: Level =
    this match {
      case Zero | Param(_) => this
      case Succ(level)     => Succ(level.simplify)
      case Max(l1, l2)     => Max.combining(l1.simplify, l2.simplify)
      case IMax(a, b) =>
        b.simplify match {
          case b_ @ Succ(_) => Max.combining(a.simplify, b_)
          case Zero         => Zero
          case b_           => IMax(a.simplify, b_)
        }
    }

  def <==(that: Level): Boolean = isLeqCore(this.simplify, that.simplify, 0)

  def ===(that: Level): Boolean = {
    val a = this.simplify
    val b = that.simplify
    isLeqCore(a, b, 0) && isLeqCore(b, a, 0)
  }

  def isZero: Boolean    = this <== Zero
  def isNonZero: Boolean = Succ(Zero) <== this

  def maybeZero: Boolean    = !isNonZero
  def maybeNonZero: Boolean = !isZero
}

object Level {
  case object Zero                    extends Level
  case class Succ(level: Level)       extends Level
  case class Max(a: Level, b: Level)  extends Level
  case class IMax(a: Level, b: Level) extends Level
  case class Param(param: Name)       extends Level

  object Max {
    def combining(l1: Level, l2: Level): Level =
      (l1, l2) match {
        case (Succ(l1_), Succ(l2_)) => Succ(combining(l1_, l2_))
        case (Zero, _)              => l2
        case (_, Zero)              => l1
        case (_, _)                 => Max(l1, l2)
      }
  }

  implicit def ofNat(n: Int): Level = Offset(n, Zero)

  object Offset {
    def unapply(level: Level): Some[(Int, Level)] = {
      @tailrec
      def decompose(i: Int, level: Level): (Int, Level) =
        level match {
          case Succ(l) => decompose(i + 1, l)
          case _       => (i, level)
        }
      Some(decompose(0, level))
    }

    @tailrec
    def apply(i: Int, level: Level): Level =
      if (i == 0) level else apply(i - 1, Succ(level))
  }

  /** l1 <= l2 + diff */
  private def isLeqCore(l1: Level, l2: Level, diff: Int): Boolean = {
    def split(p: Param): Boolean =
      Seq(Map(p -> Succ(p)), Map(p -> Zero)).forall(
        subst =>
          isLeqCore(l1.instantiate(subst).simplify,
                    l2.instantiate(subst).simplify,
                    diff))

    (l1, l2) match {
      // simplification
      case (Zero, _) if diff >= 0 => true
      case (_, Zero) if diff < 0  => false
      case (Param(i), Param(j))   => i == j && diff >= 0
      case (Param(_), Zero)       => false
      case (Zero, Param(_))       => diff >= 0
      case (Succ(l1_), _)         => isLeqCore(l1_, l2, diff - 1)
      case (_, Succ(l2_))         => isLeqCore(l1, l2_, diff + 1)

      // descend left
      case (Max(a, b), _) => isLeqCore(a, l2, diff) && isLeqCore(b, l2, diff)

      // descend right
      case (Param(_) | Zero, Max(a, b)) =>
        isLeqCore(l1, a, diff) || isLeqCore(l1, b, diff)

      // imax
      case (IMax(a1, b1), IMax(a2, b2)) if a1 == a2 && b1 == b2 => true
      case (IMax(_, p @ Param(_)), _)                           => split(p)
      case (_, IMax(_, p @ Param(_)))                           => split(p)
      case (IMax(a, IMax(b, c)), _) =>
        isLeqCore(Max(IMax(a, c), IMax(b, c)), l2, diff)
      case (IMax(a, Max(b, c)), _) =>
        isLeqCore(Max(IMax(a, b), IMax(a, c)).simplify, l2, diff)
      case (_, IMax(a, IMax(b, c))) =>
        isLeqCore(l1, Max(IMax(a, c), IMax(b, c)), diff)
      case (_, IMax(a, Max(b, c))) =>
        isLeqCore(l1, Max(IMax(a, b), IMax(a, c)).simplify, diff)
    }
  }
}
