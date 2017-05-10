package provingground

import HoTT._

import ScalaPolyRep._

import ScalaRep._

/**
  * @author gadgil
  */
case class VecTyp[X, +U <: RepTerm[X] with Subs[U]](
    basetyp: Typ[U],
    dim: Long)(implicit _baserep: ScalaRep[U, X])
    extends Typ[RepTerm[Vector[X]]] {
  val baserep = _baserep

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

object VecTyp {
  case class VecPolyRep[U <: Term with Subs[U], X]()
      extends ScalaPolyRep[RepTerm[Vector[X]], Vector[X]] {

    def apply(typ: Typ[Term])(elem: Vector[X]) = typ match {
      case tp @ VecTyp(basetyp, dim) if dim == elem.size => {
        val pattern = new ScalaSym[RepTerm[Vector[X]], Vector[X]](
          tp.asInstanceOf[Typ[RepTerm[Vector[X]]]])
        Some(pattern(elem))
      }
      case _ => None
    }

    def unapply(term: RepTerm[Vector[X]]) = term.typ match {
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
    : ScalaPolyRep[RepTerm[Vector[X]], Vector[X]] = VecPolyRep[U, X]

  val n = "n" :: NatTypLong

  import NatTypLong._

  implicit val NatVecRep = vecRep[RepTerm[Long], Long](poly(NatTypLong.rep))

  val Vec =
    (((n: Long) => (VecTyp[Long, RepTerm[Long]](NatTypLong, n): Typ[Term]))
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
