package provingground.scalahott

import provingground._, HoTT._

import ScalaRep.{ScalaSym}

//import ScalaRep._

import ScalaPolyRep._

/**
  * @author gadgil
  */
class ScalaVec[X](val basetyp: Typ[Term])(
    implicit baserep: ScalaPolyRep[ScalaTerm[X], X]) {
  case class VecTyp(dim: Int) extends ScalaTyp[Vector[X]]

  implicit object Rep extends ScalaPolyRep[ScalaTerm[Vector[X]], Vector[X]] {
    def apply(typ: Typ[Term])(elem: Vector[X]) = typ match {
      case tp @ VecTyp(dim) if dim == elem.size => {
        val pattern = new ScalaSym[ScalaTerm[Vector[X]], Vector[X]](tp)
        Some(pattern(elem))
      }
      case _ => None
    }

    def unapply(term: ScalaTerm[Vector[X]]) = term.typ match {
      case tp: VecTyp => {
        val pattern = new ScalaSym[Term, Vector[X]](tp)
        pattern.unapply(term)
      }
      case _ => None
    }

    def subs(x: Term, y: Term) = this
  }

  implicit val nrep
    : ScalaPolyRep[ScalaTerm[_root_.scala.Long], _root_.scala.Long] = poly(
    NatTypLong.rep)

  implicit val urep: ScalaPolyRep[Typ[Term], Typ[Term]] = poly(ScalaRep.UnivRep)

  private val a = basetyp.Var

  private val n = "n" :: NatTypLong

  val Vec =
    ((n: Long) => (VecTyp(n.toInt): Typ[Term])).hott(NatTypLong ->: Type).get

  private val ltyp = n ~>: (Vec(n) ->: NatTypLong)

  private val vsize = (n: Long) =>
    (v: Vector[X]) => {
      assert(v.size == n, "domain mismatch in Pi-type")
      n
  }

  val size = vsize.hott(ltyp)

  private val vcons = (n: Long) =>
    (a: X) =>
      (v: Vector[X]) => {
        assert(v.size == n, "domain mismatch in Pi-type")
        a +: v
  }

  val m: ScalaTerm[Long] = NatTypLong.succ(n)

  private val constyp = n ~>: NatTypLong ->: (Vec(n) ->: Vec(m))

  import NatTypLong.rep

  implicitly[ScalaPolyRep[ScalaTerm[Long], Long]]

  implicitly[ScalaPolyRep[ScalaTerm[Vector[X]], Vector[X]]]

  implicit val r: ScalaPolyRep[
    FuncLike[ScalaTerm[_root_.scala.Long],
             FuncLike[ScalaTerm[X],
                      FuncLike[ScalaTerm[_root_.scala.`package`.Vector[X]],
                               ScalaTerm[_root_.scala.`package`.Vector[X]]]]],
    _root_.scala.Long => X => _root_.scala.`package`.Vector[X] => _root_.scala.`package`.Vector[
      X]] = depFuncPolyRep(
    implicitly[ScalaPolyRep[ScalaTerm[Long], Long]],
    implicitly[ScalaPolyRep[
      FuncLike[ScalaTerm[X], FuncLike[ScalaTerm[Vector[X]], ScalaTerm[Vector[X]]]],
      X => Vector[X] => Vector[X]]]
  )

  val succ = ScalaPolyTerm(vcons)(r).hott(constyp).get

  val empty = Vector.empty[X].hott(VecTyp(0)).get
}
