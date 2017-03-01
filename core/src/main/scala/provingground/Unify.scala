package provingground

import provingground.HoTT._
//import provingground.Contexts._
import scala.util.Try
//import scala.reflect.runtime.universe.{Try => UnivTry, Function => FunctionUniv, _}

object Unify {
  def multisub[U <: Term with Subs[U]](x: U, m: Map[Term, Term]): U =
    m.toVector match {
      case Vector() => x
      case (a, b) +: tail => multisub(x.replace(a, b), tail.toMap)
    }

  def dependsOn(term: Term): Vector[Term] => Boolean = {
    case Vector() => false
    case x +: ys => term.dependsOn(x) || dependsOn(term)(ys)
  }

  def mergeMaps[U, V](x: Map[U, V], y: Map[U, V]): Option[Map[U, V]] =
    x.keys.toVector match {
      case Vector() => Some(y)
      case a +: bs =>
        if (y.getOrElse(a, x(a)) == x(a)) Some(y + ((a, x(a)))) else None
    }

  def mergeOptMaps[U, V](
      x: Option[Map[U, V]], y: Option[Map[U, V]]): Option[Map[U, V]] =
    x flatMap ((a) => y flatMap ((b) => mergeMaps(a, b)))

  def mergeAll[U, V](xs: Option[Map[U, V]]*): Option[Map[U, V]] =
    xs.toVector match {
      case Vector() => Some(Map())
      case x +: ys =>
        mergeOptMaps(x, mergeAll(ys: _*))
    }

  def unifyVector(xys: Vector[(Term, Term)],
                  freeVars: Term => Boolean): Option[Map[Term, Term]] =
    xys match {
      case Vector() => None
      case Vector((x, y)) => unify(x, y, freeVars)
      case head +: tail =>
        unify(head._1, head._2, freeVars) flatMap
        ((subMap) =>
              {
                val newVars =
                  (x: Term) => freeVars(x) && !(subMap.keySet contains x)
                val newTail =
                  tail map {
                    case (a, b) =>
                      (multisub(a, subMap), multisub(b, subMap))
                  }
                val tailMapOpt = unifyVector(newTail, newVars)
                val mapOpt = tailMapOpt map ((tm) => subMap ++ tm)
                mapOpt
            })
    }

  def unifyAll(freeVars: Term => Boolean)(xys: (Term, Term)*) =
    unifyVector(xys.toVector, freeVars)

  def unify(lhs: Term,
            rhs: Term,
            freevars: Term => Boolean): Option[Map[Term, Term]] = {
    if (lhs == rhs) Some(Map())
    else
      (lhs, rhs) match {
        case (variable, value)
            if (freevars(variable)) &&
            (variable.replace(variable, value) == value) =>
          Some(Map(variable -> value))
        // case (value, variable) if (freevars(variable)) =>
        //   Some(Map(variable -> value))
        case (PiDefn(a: Term, b: Typ[v]), PiDefn(c: Term, d: Typ[w])) =>
          unifyAll(freevars)(a -> c, b -> d)
        case (PiTyp(f), PiTyp(g)) => unify(f, g, freevars)
        case (SigmaTyp(f), SigmaTyp(g)) => unify(f, g, freevars)
        case (FuncTyp(a: Typ[u], b: Typ[v]), FuncTyp(c: Typ[w], d: Typ[x])) =>
          unifyAll(freevars)(a -> c, b -> d)
        case (PlusTyp(a: Typ[u], b: Typ[v]), PlusTyp(c: Typ[w], d: Typ[x])) =>
          unifyAll(freevars)(a -> c, b -> d)
        case (IdentityTyp(dom1: Typ[u], a: Term, b: Term),
              IdentityTyp(dom2: Typ[v], c: Term, d: Term)) =>
          unifyAll(freevars)(dom1 -> dom2, a -> c, b -> d)
        case (x: AbsPair[_, _], y: AbsPair[_, _]) =>
          unifyAll(freevars)(x.first -> y.first, x.second -> y.second)
        case (f1 @ FormalAppln(a, b), f2 @ FormalAppln(c, d)) =>
          unifyAll(freevars)(a -> c, b -> d, f1.typ -> f2.typ)
        case (f: LambdaLike[u, v], g: LambdaLike[w, x]) =>
          unify(f.variable, g.variable, freevars) flatMap
          ((m) =>
                {
                  val xx = multisub(f.variable, m)
                  val yy = multisub(f.value, m)
                  val newvars =
                    (x: Term) => freevars(x) && (!(m.keySet contains x))
                  unify(yy.subs(xx, g.variable), g.value, newvars) map (m ++ _)
              })
        case _ => None
      }
  }

  def subsApply(func: Term,
                arg: Term,
                unifMap: Map[Term, Term],
                freeVars: Vector[Term]) = {
    val fn = multisub(func, unifMap)
    val x = multisub(arg, unifMap)
    val lambdaVars = freeVars filter ((x) => !(unifMap.keySet contains x))
    import Fold._
    Try(polyLambda(lambdaVars.toList, fn(x))).toOption
  }

  def unifApply(func: Term, arg: Term, freeVars: Vector[Term]) = func match {
    case fn: FuncLike[u, v] =>
      unify(fn.dom, arg.typ, (t) => freeVars contains t) flatMap
      (subsApply(func, arg, _, freeVars))
    case _ => None
  }

  def appln(
      func: Term, arg: Term, freeVars: Vector[Term] = Vector()): Option[Term] =
    unifApply(func, arg, freeVars) orElse
    (func match {
          case fn: FuncLike[u, v] =>
            val l = funcToLambda(fn)
            appln(l.value, arg, l.variable +: freeVars)
          case _ => None
        })

  def purgeInv(r1: Term,
               inv1: Set[(Term, Term)],
               r2: Term,
               inv2: Set[(Term, Term)],
               freeVars: Term => Boolean) = {
    val imageOpt =
      unify(r1, r2, freeVars) map
      ((uniMap) =>
            inv2 map {
              case (f, x) =>
                (multisub(f, uniMap), multisub(x, uniMap))
          })
    inv2 -- imageOpt.getOrElse(Set())
  }

  import annotation.tailrec

  def purgeVector(r2: Term,
                  inv2: Set[(Term, Term)],
                  invVector: Vector[(Term, Set[(Term, Term)])],
                  freeVars: Term => Boolean) =
    invVector.foldRight((r2, inv2)) {
      case (ri1, ri2) =>
        (ri2._1, purgeInv(ri1._1, ri1._2, ri2._1, ri2._2, freeVars))
    }

  @tailrec
  def purgedPairsList(fxs: List[(Term, Term)],
                      accum: List[(Term, Term)] = List()): List[(Term, Term)] =
    fxs match {
      case List() => accum
      case head :: tail =>
        val needHead = (tail find
            ((fx) =>
                  !unifyAll(isVar)(fx._1 -> head._1, fx._2 -> head._2).isEmpty)).isEmpty
        if (needHead) purgedPairsList(tail, head :: accum)
        else purgedPairsList(tail, accum)
    }

  def purgedPairs(fxs: Set[(Term, Term)]) = purgedPairsList(fxs.toList).toSet

  @tailrec
  def purgedInvVector(invVector: Vector[(Term, Set[(Term, Term)])],
                      accum: Vector[(Term, Set[(Term, Term)])] = Vector(),
                      freeVars: Term => Boolean = HoTT.isVar)
    : Vector[(Term, Set[(Term, Term)])] =
    invVector match {
      case Vector() => accum
      case head +: tail =>
        val newhead = purgeVector(head._1, head._2, accum, freeVars)

        if (newhead._2 != Set()) {
          val newaccum =
            accum map {
              case (r2, inv2) =>
                (r2, purgeInv(head._1, newhead._2, r2, inv2, freeVars))
            } filter (_._2 != Set())
          purgedInvVector(tail, newhead +: newaccum, freeVars)
        } else purgedInvVector(tail, accum, freeVars)
    }
}
