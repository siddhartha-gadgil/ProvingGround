package provingground

import HoTT._

import ScalaRep._

import spire.algebra._
import spire.math._
import spire.implicits._
import scala.util._
import scala.language.implicitConversions
import annotation.tailrec

/**
 * @author gadgil
 */
class SymbolicCRing[A: CRing] extends SymbolicCRig[A] {self =>
  val ring = implicitly[Ring[A]]

  import ring._

  val minusone = Literal(ring.negate(one))

  val negate = prod(minusone)

  implicit val cringStructure : CRing[LocalTerm] = new CRing[LocalTerm]{
    val zero = Literal(ring.zero)

    val one = Literal(ring.one)

    def plus(x: LocalTerm, y: LocalTerm) = self.sum(x)(y)

    def times(x: LocalTerm, y: LocalTerm) = self.prod(x)(y)

    def negate(x: LocalTerm) = self.negate(x)
  }
}

object SymbolicCRing{
  case object NatRing extends SymbolicCRing[SafeLong]{
    val x = "x" :: LocalTyp
    val succ = lmbda(x)(x + 1)

    val X = "X" :: __
    val init = X.Var
    val g = (NatTyp ->: X ->: X).Var

    val n = "n" :: NatTyp

    @tailrec def recDefn[U <: Term with Subs[U]](n: SafeLong, formal: U, h: SafeLong => U => U) : U =
      if (n == 0) formal else recDefn(n -1 , h(n)(formal), h)

    object recValue extends Func[LocalTerm, Term]{
      val h = (n: SafeLong) => g(Literal(n))

      val dom = NatTyp
      val codom = X

      val typ = dom ->: codom

      def subs(x: Term, y: Term) = this
      def newobj = this


      def act(x: LocalTerm) = x match {
        case Literal(n) => recDefn(n, init, h)
        case LiteralSum(n, x) => recDefn(n, recValue(x), h)
        case _ => FormalAppln(recValue, x)
      }



    val recLambda = lmbda(n)(recValue(n))

    val rec =
      lambda(X)(
        lmbda(init)(
          lmbda(g)(
            recValue : Func[LocalTerm, Term]
          )
        )
      )
    }

    val Xs = "X()" :: NatTyp ->: __

    val gs = (NatTyp ~>: (Xs(n) ~>: Xs(succ(n)))).Var

    object inducValue extends FuncLike[LocalTerm, Term]{
      val h = (n: SafeLong) => gs(Literal(n))

      val dom = NatTyp
      val depcodom = Xs

      val typ = dom ~>: (Xs(n))

      def subs(x: Term, y: Term) = this
      def newobj = this


      def act(x: LocalTerm) = x match {
        case Literal(n) => recDefn(n, init, h)
        case LiteralSum(n, x) => recDefn(n, recValue(x), h)
        case _ => FormalAppln(recValue, x)
      }

  }

  val induc =
    lambda(Xs)(
      lambda(init)(
        lambda(gs)(
          inducValue : FuncLike[LocalTerm, Term]
        )
      )
    )
}

  val NatTyp = NatRing.LocalTyp

  type Nat = NatRing.LocalTerm


}
