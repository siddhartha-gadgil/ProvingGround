package provingground.learning
import provingground._
import HoTT._

import scala.collection.mutable

class TermBucket {
  var tot: Long = 0

  var loops: Int = 0

  var startTime: Long = System.currentTimeMillis()

  def elapsedTime = System.currentTimeMillis() - startTime

  import TermBucket.{fd, fdMap}

  /**
    * terms counted, sorted by types
    */
  val terms: mutable.Map[Typ[Term], Vector[Term]] = mutable.Map()

  /**
    * number of  terms of a given type
    */
  val termTypes: mutable.Map[Typ[Term], Long] = mutable.Map()

  /**
    * count of generation of a type (as a term)
    */
  val types: mutable.Map[Typ[Term], Long] = mutable.Map()

  def clear() = {
    tot = 0
    terms.clear()
    termTypes.clear()
    types.clear()
  }

  def clearAll() = {
    clear()
    loops = 0
    startTime = System.currentTimeMillis()
  }

  def append(t: Term) = {
    tot += 1

    val typ: Typ[Term] = t.typ

    terms(typ) = t +: (terms.getOrElse(typ, Vector()))

    termTypes(typ) = termTypes.getOrElse(typ, 0: Long) + 1

    t match {
      case tp: Typ[u] =>
        types(tp) = types.getOrElse(tp, 0: Long) + 1
      case _ => ()
    }
  }

  /**
    * finite distribution of terms with a given type (total not 1, but weight of type)
    */
  def getTermDistMap = fdMap(terms, tot).view.mapValues (_.flatten).toMap

  /**
    * theorems weighted by the total weight of their prooofs
    */
  def getThmsByProofs = fd(termTypes, tot).flatten

  /**
    * types weighted by their frequency of generation as terms
    */
  def getTypDist = fd(types, tot).flatten.normalized()

  /**
    * inhabited types (i.e. theorems) weighted by their frequency of generation as terms,
    * normalized to account for most types not being theorems
    */
  def getTheorems = {
    val typDist = getTypDist
    val pmf     = getThmsByProofs.supp map ((t) => Weighted(t, typDist(t)))
    FiniteDistribution(pmf).normalized()
  }
}

object TermBucket {
  def fdMap[A](m: mutable.Map[A, Vector[Term]], tot: Long) = {
    //    val tot = m.values.flatten.size
    (m.view.mapValues
      ((l) => FiniteDistribution((l map (Weighted(_, 1.0 / tot))).toVector))).toMap
  }

  def fd(m: mutable.Map[Typ[Term], Long], tot: Long) = {
    //    val tot = m.values.sum
    val pmf = for ((x, l) <- m) yield Weighted(x, l * 1.0 / tot)
    FiniteDistribution(pmf.toVector)
  }

  def toLambda(x: Term, scale: Double): Weighted[Term] => Weighted[Term] = {
    case Weighted(y, p) =>
      if (y dependsOn (x)) Weighted(lambda(x)(y), p * scale)
      else Weighted(y, p)
  }

  def mkLambda(vars: Vector[Weighted[Term]], scale: Double)(
      yp: Weighted[Term]): Weighted[Term] = vars match {
    case Vector() => yp
    case head +: tail =>
      toLambda(head.elem, scale * head.weight)(mkLambda(tail, scale)(yp))
  }

  def lambdaDist(vars: Vector[Weighted[Term]], scale: Double)(
      fd: FiniteDistribution[Term]) = {
    FiniteDistribution(fd.pmf map (mkLambda(vars, scale)(_)))
  }

  def toPi(x: Term, scale: Double): Weighted[Typ[Term]] => Weighted[Typ[Term]] = {
    case Weighted(y, p) =>
      if (y dependsOn (x)) Weighted(pi(x)(y), p * scale) else Weighted(y, p)
  }

  def mkPi(vars: Vector[Weighted[Term]], scale: Double)(
      yp: Weighted[Typ[Term]]): Weighted[Typ[Term]] = vars match {
    case Vector() => yp
    case head +: tail =>
      toPi(head.elem, scale * head.weight)(mkPi(tail, scale)(yp))
  }

  def piDist(vars: Vector[Weighted[Term]], scale: Double)(
      fd: FiniteDistribution[Typ[Term]]) = {
    FiniteDistribution(fd.pmf map (mkPi(vars, scale)(_)))
  }
}

class WeightedTermBucket {
  var tot: Double = 0

  import WeightedTermBucket.{fd, fdMap}

  val terms: mutable.Map[Typ[Term], Vector[Weighted[Term]]] = mutable.Map()

  val termTypes: mutable.Map[Typ[Term], Double] = mutable.Map()

  val types: mutable.Map[Typ[Term], Double] = mutable.Map()

  def append(t: Weighted[Term]) = {
    tot += t.weight

    val typ = t.elem.typ

    terms(typ) = t +: (terms.getOrElse(typ, Vector()))

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
  def fd(m: mutable.Map[Typ[Term], Double], tot: Double) = {
    //  val tot = m.values.sum
    val pmf = for ((x, l) <- m) yield Weighted(x, l / tot)
    FiniteDistribution(pmf.toVector)
  }

  def fdMap[A](m: mutable.Map[A, Vector[Weighted[Term]]], tot: Double) = {
    //  val tot = (m.values.flatten map (_.weight)).sum
    (m.view.mapValues
      ((l) =>
        FiniteDistribution((l map
          ((wt) => Weighted(wt.elem, wt.weight / tot))).toVector))).toMap
  }
}
