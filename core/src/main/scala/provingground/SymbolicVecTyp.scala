package provingground

import HoTT._

import ScalaPolyRep._

import ScalaRep._


import spire.algebra._
import spire.math.{poly => mathpoly, _}
import spire.implicits._
import scala.util._
import scala.language.implicitConversions

import NatRing._

import NatTyp._

/**
 * @author gadgil
 */
case class SymbolicVecTyp[X, +U<: RepTerm[X] with Subs[U]](basetyp: Typ[U], dim: SafeLong)(implicit baserep: ScalaRep[U, X]) extends Typ[RepTerm[Vector[X]]]{

    val typ = Universe(0)

    def symbObj(name: AnySym): RepTerm[Vector[X]] = RepSymbObj[Vector[X], RepTerm[Vector[X]]](name, this)

    def newobj = this

    def subs(x: Term, y: Term) = (x, y) match {
      case (xt: Typ[_], yt: Typ[_]) if (xt == this) => yt.asInstanceOf[Typ[RepTerm[Vector[X]]]]
      case _ => this
    }
}

object SymbolicVecTyp{
  case class VecPolyRep[U <: Term with Subs[U], X]() extends ScalaPolyRep[RepTerm[Vector[X]], Vector[X]]{

    def apply(typ: Typ[Term])(elem: Vector[X]) = typ match{
      case tp @ SymbolicVecTyp(basetyp, dim) if dim == elem.size => {
        val pattern = new ScalaSym[RepTerm[Vector[X]], Vector[X]](tp.asInstanceOf[Typ[RepTerm[Vector[X]]]])
        Some(pattern(elem))
      }
      case _ => None
    }

    def unapply(term: RepTerm[Vector[X]]) = term.typ match {
      case tp : SymbolicVecTyp[_, X] => {
        val pattern = new ScalaSym[Term, Vector[X]](tp)
        pattern.unapply(term)
      }
      case _ => None
    }

    def subs(x: Term, y: Term) = this
  }

  implicit def vecRep[U <: Term with Subs[U], X](implicit baserep: ScalaPolyRep[U, X]) : ScalaPolyRep[RepTerm[Vector[X]], Vector[X]] = VecPolyRep[U, X]


  val n = "n" :: NatTyp



//  implicit val NatVecRep = vecRep[RepTerm[SafeLong], SafeLong](poly(NatTyp.rep))

  
  val Vec = (((n: SafeLong) => (SymbolicVecTyp[SafeLong, RepTerm[SafeLong]](NatTyp, n) : Typ[Term])).hott(NatTyp ->: __)).get

  private val ltyp = n ~>: (Vec(n) ->: NatTyp)

  private val vsize = (n : SafeLong) =>
    (v : Vector[SafeLong]) => {
      assert(v.size ==n ,"domain mismatch in Pi-type")
      n
  }

  val nsize = vsize.hott(ltyp)

  private val nvsucc =
    (n : SafeLong) =>
      (a : SafeLong) =>
        (v : Vector[SafeLong]) => {
          assert(v.size ==n ,"domain mismatch in Pi-type")
          a +: v
          }

  private val nsucctyp  = n  ~>: NatTyp ->: (Vec(n) ->: Vec(NatRing.succ(n)))

  val nsucc = nvsucc.hott(nsucctyp)
}
