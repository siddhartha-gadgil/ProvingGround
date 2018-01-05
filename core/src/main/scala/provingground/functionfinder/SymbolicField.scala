package provingground.functionfinder

import provingground._, HoTT._

import ScalaRep._

import spire.algebra._
import spire.math._
import spire.implicits._
import scala.util._
import scala.language.implicitConversions

/**
  * @author gadgil
  */
class SymbolicField[A: Field] extends SymbolicCRing[A] { self =>
  val field = implicitly[Field[A]]

  import field._

  override lazy val reciprocal: Func[LocalTerm, LocalTerm] =
    new Func[LocalTerm, LocalTerm] {
      val dom = LocalTyp

      val codom = LocalTyp

      val typ = dom ->: dom

      def act(x: LocalTerm) = x match {
        case Literal(a) => Literal(1 / a)
        case Comb(op, u, v) if op == prod =>
          prod(reciprocal(u))(reciprocal(v))
        case Reciprocal(a) => a
        case PiTerm(elems) => {
          val flipElems = for ((a, p) <- elems) yield (a, -p)
          PiTerm(flipElems)
        }
        case a => Reciprocal(a)
      }

      def subs(x: Term, y: Term) = this

      def newobj =
        throw new IllegalArgumentException(
          s"trying to use the constant $this as a variable (or a component of one)")

      override def toString = "reciprocal"
    }

  override def power(x: LocalTerm, n: Int) =
    if (n >= 0) posPower(x, n) else posPower(reciprocal(x), -n)

  implicit val fieldStructure: Field[LocalTerm] = new Field[LocalTerm] {
    val zero = Literal(ring.zero)

    val one = Literal(ring.one)

    def plus(x: LocalTerm, y: LocalTerm) = self.sum(x)(y)

    def times(x: LocalTerm, y: LocalTerm) = self.prod(x)(y)

    def negate(x: LocalTerm) = self.negate(x)

    // Members declared in spire.algebra.EuclideanRing
    def gcd(a: SymbolicField.this.LocalTerm, b: SymbolicField.this.LocalTerm)(
        implicit ev: spire.algebra.Eq[SymbolicField.this.LocalTerm])
      : SymbolicField.this.LocalTerm =
      Literal(field.one)

    def lcm(a: SymbolicField.this.LocalTerm, b: SymbolicField.this.LocalTerm)(
        implicit ev: spire.algebra.Eq[SymbolicField.this.LocalTerm])
      : SymbolicField.this.LocalTerm = ???

    // def mod(a: SymbolicField.this.LocalTerm,
    //         b: SymbolicField.this.LocalTerm): SymbolicField.this.LocalTerm =
    //   Literal(field.zero)
    //
    // def quot(a: SymbolicField.this.LocalTerm,
    //          b: SymbolicField.this.LocalTerm): SymbolicField.this.LocalTerm =
    //   div(a, b)

    // Members declared in spire.algebra.MultiplicativeGroup
    def div(x: LocalTerm, y: LocalTerm): LocalTerm =
      self.prod(x)(self.reciprocal(y))
  }
}
