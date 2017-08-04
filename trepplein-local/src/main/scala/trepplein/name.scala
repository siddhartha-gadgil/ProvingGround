package trepplein

import Name._

import scala.annotation.tailrec
import scala.language.implicitConversions
import scala.runtime.ScalaRunTime

sealed abstract class Name extends Product {
  override val hashCode: Int = ScalaRunTime._hashCode(this)

  @tailrec
  private def equals(n1: Name, n2: Name): Boolean =
    (n1 eq n2) || ((n1, n2) match {
      case (Name.Str(p1, l1), Name.Str(p2, l2)) =>
        if (l1 != l2) return false
        equals(p1, p2)
      case (Name.Num(p1, l1), Name.Num(p2, l2)) =>
        if (l1 != l2) return false
        equals(p1, p2)
      case _ => false
    })
  override def equals(that: Any): Boolean =
    that match {
      case that: Name => equals(this, that)
      case _          => false
    }

  override def toString: String = {
    val buf = new StringBuilder
    def write(n: Name): Boolean =
      n match {
        case Anon => false
        case Str(prefix, limb) =>
          if (write(prefix)) buf += '.'
          buf ++= limb
          true
        case Num(prefix, limb) =>
          if (write(prefix)) buf += '.'
          buf.append(limb)
          true
      }
    write(this)
    buf.result()
  }

  def isAnon: Boolean = this == Anon

  def dump: String =
    this match {
      case Anon              => "Name.Anon"
      case Str(prefix, limb) => s"""Name.Str(${prefix.dump}, "$limb")"""
      case Num(prefix, limb) => s"Name.Num(${prefix.dump}, $limb)"
    }
}
object Name {
  def apply(limbs: String*): Name =
    limbs.foldLeft[Name](Anon)(Str)

  def fresh(suggestion: Name, blacklist: Set[Name]): Name =
    (suggestion #:: Stream.from(0).map(i => Name.Num(suggestion, i): Name))
      .filterNot(blacklist)
      .head

  case object Anon                                 extends Name
  final case class Str(prefix: Name, limb: String) extends Name
  final case class Num(prefix: Name, limb: Long)   extends Name

  implicit def ofString(s: String): Name = Name(s.split("\\."): _*)
}
