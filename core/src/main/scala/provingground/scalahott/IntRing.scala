package provingground.scalahott

import provingground._
import HoTT._
import spire.math._
import spire.implicits._

import scala.language.implicitConversions

object IntRing extends SymbolicCRing[SafeLong]{
  type Ints = LocalTerm

  val IntTyp: IntRing.LocalTyp.type = LocalTyp

  implicit def intLiteral(n: Int): Ints = Literal(n)
}