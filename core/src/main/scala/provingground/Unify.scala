package provingground

import provingground.HoTT._
//import provingground.Contexts._
import scala.util.Try
//import scala.reflect.runtime.universe.{Try => UnivTry, Function => FunctionUniv, _}

object Unify {
  def multisub[U <: Term with Subs[U]](x: U, m: Map[Term, Term]): U =
    m.toList match {
      case List() => x
      case (a, b) :: tail => multisub(x.subs(a, b), tail.toMap)
    }

  def dependsOn(term: Term): List[Term] => Boolean = {
    case List() => false
    case x :: ys => term.dependsOn(x) || dependsOn(term)(ys)
  }

  def mergeMaps[U, V](x: Map[U, V], y: Map[U, V]): Option[Map[U, V]] =
    x.keys match {
      case List() => Some(y)
      case a :: bs =>
        if (y.getOrElse(a, x(a)) == x(a)) Some(y + ((a, x(a)))) else None
    }

  def mergeOptMaps[U, V](
      x: Option[Map[U, V]], y: Option[Map[U, V]]): Option[Map[U, V]] =
    x flatMap ((a) => y flatMap ((b) => mergeMaps(a, b)))

  def mergeAll[U, V](xs: Option[Map[U, V]]*): Option[Map[U, V]] =
    xs.toList match {
      case List() => Some(Map())
      case x :: ys =>
        mergeOptMaps(x, mergeAll(ys: _*))
    }

  def unifyList(xys: List[(Term, Term)],
                freeVars: Term => Boolean): Option[Map[Term, Term]] =
    xys match {
      case List() => None
      case List((x, y)) => unify(x, y, freeVars)
      case head :: tail =>
        unify(head._1, head._2, freeVars) flatMap ((subMap) => {
              val newVars =
                (x: Term) => freeVars(x) && !(subMap.keySet contains x)
              val newTail =
                tail map {
                  case (a, b) => (multisub(a, subMap), multisub(b, subMap))
                }
              val tailMapOpt = unifyList(newTail, newVars)
              val mapOpt = tailMapOpt map ((tm) => subMap ++ tm)
              mapOpt
            })
    }

  def unifyAll(freeVars: Term => Boolean)(xys: (Term, Term)*) =
    unifyList(xys.toList, freeVars)

  def unify(lhs: Term,
            rhs: Term,
            freevars: Term => Boolean): Option[Map[Term, Term]] = {
    if (lhs == rhs) Some(Map())
    else
      (lhs, rhs) match {
        case (variable, value) if (freevars(variable)) && (variable.replace(variable, value) == value) =>
          Some(Map(variable -> value))
        // case (value, variable) if (freevars(variable)) =>
        //   Some(Map(variable -> value))
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
          unify(f.variable.typ, g.variable.typ, freevars) flatMap ((m) => {
                val xx = multisub(f.variable, m)
                val yy = multisub(f.value, m)
                val newvars =
                  (x: Term) => freevars(x) && (!(m.keySet contains x))
                unify(yy.subs(xx, g.variable), g.value, newvars)
              })
        case _ => None
      }
  }

  def subsApply(func: Term,
                arg: Term,
                unifMap: Map[Term, Term],
                freeVars: List[Term]) = {
    val fn = multisub(func, unifMap)
    val x = multisub(arg, unifMap)
    val lambdaVars = freeVars filter ((x) => !(unifMap.keySet contains x))
    import Fold._
    Try(polyLambda(lambdaVars, fn(x))).toOption
  }

  def unifApply(func: Term, arg: Term, freeVars: List[Term]) = func match {
    case fn: FuncLike[u, v] =>
      unify(fn.dom, arg.typ, (t) => freeVars contains t) flatMap (subsApply(
              func, arg, _, freeVars))
    case _ => None
  }

  def appln(
      func: Term, arg: Term, freeVars: List[Term] = List()): Option[Term] =
    unifApply(func, arg, freeVars) orElse (func match {
          case fn: FuncLike[u, v] =>
            val l = funcToLambda(fn)
            appln(l.value, arg, l.variable :: freeVars)
          case _ => None
        })
}
