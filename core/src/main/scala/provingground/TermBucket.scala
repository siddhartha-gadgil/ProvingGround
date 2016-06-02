package provingground
import HoTT._

import scala.collection.mutable.{Map => mMap}

class TermBucket {
  var tot: Long = 0

  import TermBucket.{fd, fdMap}

  val terms: mMap[Typ[Term], List[Term]] = mMap()

  val termTypes: mMap[Typ[Term], Long] = mMap()

  val types: mMap[Typ[Term], Long] = mMap()

  def append(t: Term) = {
    tot += 1

    val typ = t.typ

    terms(typ) = t :: (terms.getOrElse(typ, List()))

    termTypes(typ) = termTypes.getOrElse(typ, 0: Long) + 1

    t match {
      case tp: Typ[u] =>
        types(tp) = types.getOrElse(tp, 0: Long) + 1
      case _ => ()
    }
  }

  def termDistMap = fdMap(terms, tot) mapValues (_.flatten)

  def termTypDist = fd(termTypes, tot).flatten

  def typDist = fd(types, tot).flatten
}

object TermBucket {
  def fdMap[A](m: mMap[A, List[Term]], tot: Long) = {
//    val tot = m.values.flatten.size
    (m mapValues (
            (l) =>
              FiniteDistribution((l map (Weighted(_, 1.0 / tot))).toVector)
        )).toMap
  }

  def fd(m: mMap[Typ[Term], Long], tot: Long) = {
//    val tot = m.values.sum
    val pmf = for ((x, l) <- m) yield Weighted(x, l * 1.0 / tot)
    FiniteDistribution(pmf.toVector)
  }

  def toLambda(x: Term, scale: Double): Weighted[Term] => Weighted[Term] = {
    case Weighted(y, p) =>
      if (y dependsOn (x)) Weighted(lambda(x)(y), p * scale)
      else Weighted(y, p)
  }

  def mkLambda(vars: List[Weighted[Term]], scale: Double)(
      yp: Weighted[Term]): Weighted[Term] = vars match {
    case List() => yp
    case head :: tail =>
      toLambda(head.elem, scale * head.weight)(mkLambda(tail, scale)(yp))
  }

  def toPi(
      x: Term, scale: Double): Weighted[Typ[Term]] => Weighted[Typ[Term]] = {
    case Weighted(y, p) =>
      if (y dependsOn (x)) Weighted(pi(x)(y), p * scale) else Weighted(y, p)
  }

  def mkPi(vars: List[Weighted[Term]], scale: Double)(
      yp: Weighted[Typ[Term]]): Weighted[Typ[Term]] = vars match {
    case List() => yp
    case head :: tail =>
      toPi(head.elem, scale * head.weight)(mkPi(tail, scale)(yp))
  }
}

class WeightedTermBucket {
  var tot: Double = 0

  import WeightedTermBucket.{fd, fdMap}

  val terms: mMap[Typ[Term], List[Weighted[Term]]] = mMap()

  val termTypes: mMap[Typ[Term], Double] = mMap()

  val types: mMap[Typ[Term], Double] = mMap()

  def append(t: Weighted[Term]) = {
    tot += t.weight

    val typ = t.elem.typ

    terms(typ) = t :: (terms.getOrElse(typ, List()))

    termTypes(typ) = termTypes.getOrElse(typ, 0.0) + t.weight

    t.elem match {
      case tp: Typ[u] =>
        types(tp) = types.getOrElse(tp, 0.0) + t.weight
      case _ => ()
    }
  }

  def termDistMap = fdMap(terms, tot)

  def termTypDist = fd(termTypes, tot)

  def typDist = fd(types, tot)
}

object WeightedTermBucket {
  def fd(m: mMap[Typ[Term], Double], tot: Double) = {
    //  val tot = m.values.sum
    val pmf = for ((x, l) <- m) yield Weighted(x, l / tot)
    FiniteDistribution(pmf.toVector)
  }

  def fdMap[A](m: mMap[A, List[Weighted[Term]]], tot: Double) = {
    //  val tot = (m.values.flatten map (_.weight)).sum
    (m mapValues (
            (l) =>
              FiniteDistribution((l map ((wt) =>
                            Weighted(wt.elem, wt.weight / tot))).toVector)
        )).toMap
  }
}
