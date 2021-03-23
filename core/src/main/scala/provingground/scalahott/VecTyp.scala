package provingground.scalahott

import provingground._, HoTT._

import ScalaPolyRep._

import ScalaRep._

/**
  * @author gadgil
  */
case class VecTyp[X, +U <: ScalaTerm[X] with Subs[U]](basetyp: Typ[U], dim: Long)(
    implicit _baserep: ScalaRep[U, X])
    extends Typ[ScalaTerm[Vector[X]]] {
  val baserep = _baserep

  type Obj = ScalaTerm[Vector[X]]

  val typ = Universe(0)

  def variable(name: AnySym): ScalaTerm[Vector[X]] =
    RepSymbObj[Vector[X], ScalaTerm[Vector[X]]](name, this)

  def newobj =
    throw new IllegalArgumentException(
      s"trying to use the constant $this as a variable (or a component of one)")

  def subs(x: Term, y: Term) = (x, y) match {
    case (xt: Typ[_], yt: Typ[_]) if (xt == this) =>
      yt.asInstanceOf[Typ[ScalaTerm[Vector[X]]]]
    case _ => this
  }
}

object VecTyp {
  case class VecPolyRep[U <: Term with Subs[U], X]()
      extends ScalaPolyRep[ScalaTerm[Vector[X]], Vector[X]] {

    def apply(typ: Typ[Term])(elem: Vector[X]) = typ match {
      case tp @ VecTyp(basetyp, dim) if dim == elem.size => {
        val pattern = new ScalaSym[ScalaTerm[Vector[X]], Vector[X]](
          tp.asInstanceOf[Typ[ScalaTerm[Vector[X]]]])
        Some(pattern(elem))
      }
      case _ => None
    }

    def unapply(term: ScalaTerm[Vector[X]]) = term.typ match {
      case tp: VecTyp[_, X] => {
        val pattern = new ScalaSym[Term, Vector[X]](tp)
        pattern.unapply(term)
      }
      case _ => None
    }

    def subs(x: Term, y: Term) = this
  }

  implicit def vecRep[U <: Term with Subs[U], X](
      implicit baserep: ScalaPolyRep[U, X])
    : ScalaPolyRep[ScalaTerm[Vector[X]], Vector[X]] = VecPolyRep[U, X]()

  val n = "n" :: NatTypLong

  import NatTypLong._

  implicit val NatVecRep
    : ScalaPolyRep[ScalaTerm[_root_.scala.`package`.Vector[_root_.scala.Long]],
                   _root_.scala.`package`.Vector[_root_.scala.Long]] =
    vecRep[ScalaTerm[Long], Long](poly(NatTypLong.rep))

  val Vec =
    (((n: Long) => (VecTyp[Long, ScalaTerm[Long]](NatTypLong, n): Typ[Term]))
      .hott(NatTypLong ->: Type))
      .get

  private val ltyp = n ~>: (Vec(n) ->: NatTypLong)

  private val vsize = (n: Long) =>
    (v: Vector[Long]) => {
      assert(v.size == n, "domain mismatch in Pi-type")
      n
  }

  val nsize = vsize.hott(ltyp)

  private val nvsucc = (n: Long) =>
    (a: Long) =>
      (v: Vector[Long]) => {
        assert(v.size == n, "domain mismatch in Pi-type")
        a +: v
  }

  private val nsucctyp =
    n ~>: NatTypLong ->: (Vec(n) ->: Vec(NatTypLong.succ(n)))

  val nsucc = nvsucc.hott(nsucctyp)
}
