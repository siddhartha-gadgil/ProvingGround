package provingground.scalahott

import provingground._
import HoTT._
import spire.math._
import spire.implicits._

import scala.language.implicitConversions

object IntRing extends SymbolicCRing[SafeLong] {
  type Ints = LocalTerm

  override def toString: String = "Ints"

  val IntTyp: IntRing.LocalTyp.type = LocalTyp

  implicit def intLiteral(n: Int): Ints = Literal(n)

  val natIncl: SymbolicCRing.Homomorphism[SafeLong, SafeLong] =
    SymbolicCRing.Homomorphism(NatRing, this, identity[SafeLong])

  val ratIncl: SymbolicCRing.Homomorphism[SafeLong, Rational] =
    SymbolicCRing.Homomorphism(this, QField, (x: SafeLong) => x: Rational)

  lazy val Positive: Func[ScalaTerm[SafeLong], Typ[Term]] = {
    val x = IntTyp.Var
    x :-> QField.Pos(ratIncl(x))
  }

  lazy val LEQ: Func[Func[ScalaTerm[SafeLong], ScalaTerm[SafeLong]], Typ[Term]] = {
    val x = IntTyp.Var
    val y = IntTyp.Var
    x :-> y :-> Positive(y - x)
  }
}
