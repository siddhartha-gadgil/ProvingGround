package provingground.functionfinder

import provingground._, HoTT._
import ScalaRep._
// import translation._
//import scala.language.{existentials, implicitConversions}
import scala.util._
// import spire.algebra._
// import spire.implicits._
//import spire.syntax._
import cats.kernel._, cats.implicits._, cats.syntax._

class SymbolicGroup[A : Group] extends ScalaTyp[A]{self =>
  val group = implicitly[Group[A]]

  type LocalTerm = RepTerm[A]

  type Op = Func[LocalTerm, Func[LocalTerm, LocalTerm]]

  val inv = ((x: A) => group.inverse(x)).term

  val mulFn : A => A => A = ((x: A) => ((y: A) =>  group.combine(x, y)))

  val opRep = rep -->: rep -->: rep

  val mulBase : Func[LocalTerm, Func[LocalTerm, LocalTerm]] = opRep(mulFn)

  val id = group.empty.term

  object Comb {
  def unapply(term: Term): Option[(LocalTerm, LocalTerm)] = term match {
    case FormalAppln(FormalAppln(`mul`, x), y) =>
      Try(
        (
         x.asInstanceOf[LocalTerm],
         y.asInstanceOf[LocalTerm])).toOption
    case _ => None
    }

  def apply(x: LocalTerm, y: LocalTerm) = FormalAppln(FormalAppln(mul, x), y)
  }

  object Literal extends ScalaSym[LocalTerm, A](self)


  case class MultLiteral(a: A) extends Func[LocalTerm, LocalTerm] {
    val dom = self

    val codom = self

    val typ = self ->: self

    def subs(x: Term, y: Term) = this

    def newobj =
      throw new IllegalArgumentException(
        s"trying to use the constant $this as a variable (or a component of one)")

    def act(y: LocalTerm) = y match {
      case Literal(b)                         => Literal(group.combine(a, b))
      case Comb(Literal(b), v) =>             MultLiteral(group.combine(a, b))(v)
      case p                                  => mul(Literal(a))(p)
    }
  }


  case class MultTerm(a: LocalTerm) extends Func[LocalTerm, LocalTerm] {
    val dom = self

    val codom = self

    val typ = self ->: self

    def subs(x: Term, y: Term) = MultTerm(a.replace(x, y))

    def newobj =
      throw new IllegalArgumentException(
        s"trying to use the constant $this as a variable (or a component of one)")

    def act(y: LocalTerm) = y match {
      case Comb(u, v) =>             Comb(Comb(a, u),v)
      case p                                  => mulBase(a)(p)
    }
  }

  case object mul extends Func[LocalTerm, Func[LocalTerm, LocalTerm]] {w =>
    val dom = self

    val codom = self ->: self

    val typ = dom ->: codom

    def subs(x: Term, y: Term) = this

    def newobj =
      throw new IllegalArgumentException(
        s"trying to use the constant $this as a variable (or a component of one)")

    def act(y: LocalTerm) = y match {
      case Literal(a)                         => MultLiteral(a)
      case Comb(x, y) =>              ???
      case p                                  => mulBase(p)
  }
  }




  implicit val groupStructure: Group[LocalTerm] = new Group[LocalTerm]{
    val empty = id

    def combine(x: LocalTerm, y: LocalTerm) = mul(x)(y)

    def inverse(x: LocalTerm) = inv(x)

  }
}
