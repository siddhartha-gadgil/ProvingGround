package provingground.functionfinder

import provingground._, HoTT._

import ScalaPolyRep._

import ScalaRep._

//import spire.algebra._
import spire.math.{poly => mathpoly, _}
import spire.implicits._
//import scala.util._
import scala.language.implicitConversions

import NatRing._

import NatTyp._

/**
  * @author gadgil
  */
case class IndexedVecTyp[X, +U <: RepTerm[X] with Subs[U]](
    basetyp: Typ[U],
    dim: SafeLong)(implicit baserep: ScalaRep[U, X])
    extends Typ[RepTerm[Vector[X]]] {
  type Obj = RepTerm[Vector[X]]

  val typ = Universe(0)

  def variable(name: AnySym): RepTerm[Vector[X]] =
    RepSymbObj[Vector[X], RepTerm[Vector[X]]](name, this)

  def newobj =
    throw new IllegalArgumentException(
      s"trying to use the constant $this as a variable (or a component of one)")

  def subs(x: Term, y: Term) = (x, y) match {
    case (xt: Typ[_], yt: Typ[_]) if (xt == this) =>
      yt.asInstanceOf[Typ[RepTerm[Vector[X]]]]
    case _ => this
  }
}

class VecTyps[X, U <: RepTerm[X] with Subs[U]](basetyp: Typ[U])(
    implicit baserep: ScalaRep[U, X]) {
  implicit val vrep = IndexedVecTyp.vecRep[U, X]

  assert(
    basetyp == baserep.typ,
    s"specified type $basetyp does not match type of ${baserep.typ} of scalarep")

  val n = "n" :: NatTyp

  val Vec = ((n: SafeLong) =>
    IndexedVecTyp[X, U](basetyp, n): Typ[RepTerm[Vector[X]]]).term

  val NilVec = (Vector(): Vector[X]).getTerm(Vec(Literal(0)))

  val consTyp = n ~>: (basetyp ->: Vec(n) ->: Vec(succ(n)))

  val consFn = (n: SafeLong) => (x: X) => (v: Vector[X]) => (x +: v)

  val consRep = depFuncPolyRep(
    poly(NatTyp.rep),
    depFuncPolyRep(poly(baserep), depFuncPolyRep(vrep, vrep)))

  val consLike = ScalaPolyTerm(consFn)(consRep).getTerm(consTyp)

  val cons = consLike.asInstanceOf[
    FuncLike[Nat, Func[U, Func[RepTerm[Vector[X]], RepTerm[Vector[X]]]]]]
}

object NatVecTyps extends VecTyps[SafeLong, Nat](NatTyp)

object IndexedVecTyp {
  case class VecPolyRep[U <: Term with Subs[U], X]()
      extends ScalaPolyRep[RepTerm[Vector[X]], Vector[X]] {

    def apply(typ: Typ[Term])(elem: Vector[X]) = typ match {
      case tp @ IndexedVecTyp(basetyp, dim) if dim == elem.size => {
        val pattern = new ScalaSym[RepTerm[Vector[X]], Vector[X]](
          tp.asInstanceOf[Typ[RepTerm[Vector[X]]]])
        Some(pattern(elem))
      }
      case _ => None
    }

    def unapply(term: RepTerm[Vector[X]]) = term.typ match {
      case tp: IndexedVecTyp[_, X] => {
        val pattern = new ScalaSym[Term, Vector[X]](tp)
        pattern.unapply(term)
      }
      case _ => None
    }

    def subs(x: Term, y: Term) = this
  }

  implicit def vecRep[U <: Term with Subs[U], X](
      implicit baserep: ScalaPolyRep[U, X])
    : ScalaPolyRep[RepTerm[Vector[X]], Vector[X]] = VecPolyRep[U, X]

  val n = "n" :: NatTyp
}
