package provingground.learning
import provingground._

import provingground.HoTT._
//import provingground.Contexts._
import scala.util.Try
//import scala.reflect.runtime.universe.{Try => UnivTry, Function => FunctionUniv, _}

object Unify {
  def multisub[U <: Term with Subs[U]](x: U, m: Map[Term, Term]): U =
    m.toVector match {
      case Vector()       => x
      case (a, b) +: tail => multisub(x.replace(a, b), tail.toMap)
    }

  def extraVars(v: Vector[Term], m: Map[Term, Term]): Vector[Term] = 
    v match {
      case Vector() => Vector()
      case head +: tail =>
        m.get(head).map{value => 
          val newTail = tail.map(z => z.replace(head, value))
          extraVars(newTail, m)
        }.getOrElse(head +: extraVars(tail, m))        
    }

  def dependsOn(term: Term): Vector[Term] => Boolean = {
    case Vector() => false
    case x +: ys  => term.dependsOn(x) || dependsOn(term)(ys)
  }

  def mergeMaps[U, V](x: Map[U, V], y: Map[U, V]): Option[Map[U, V]] =
    x.keys.toVector match {
      case Vector() => Some(y)
      case a +: bs =>
        if (y.getOrElse(a, x(a)) == x(a)) Some(y + ((a, x(a)))) else None
    }

  def mergeOptMaps[U, V](x: Option[Map[U, V]],
                         y: Option[Map[U, V]]): Option[Map[U, V]] =
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
      case Vector()       => None
      case Vector((x, y)) => unify(x, y, freeVars)
      case head +: tail =>
        unify(head._1, head._2, freeVars) flatMap
          ((subMap) => {
            val dualFreeVars = 
              (x: Term) => freeVars(multisub(x, subMap.map{case (a, b) => (b, a)}))
            val newVars =                
              (x: Term) => dualFreeVars(x) && !(subMap.keySet contains x)
            val newTail =
              tail map {
                case (a, b) =>
                  (multisub(a, subMap), multisub(b, subMap))
              }
            val tailMapOpt = unifyVector(newTail, newVars)
            val mapOpt     = tailMapOpt map ((tm) => subMap ++ tm)
            mapOpt
          })
    }

  def unifyAll(freeVars: Term => Boolean)(
      xys: (Term, Term)*): Option[Map[Term, Term]] =
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
        case (PiTyp(f), PiTyp(g))       => unify(f, g, freevars)
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
        case (f1 @ MiscAppln(a, b), f2 @ MiscAppln(c, d)) =>
          unifyAll(freevars)(a -> c, b -> d, f1.typ -> f2.typ)
        case (f: LambdaLike[u, v], g: LambdaLike[w, x]) =>
          unify(f.variable, g.variable, freevars) flatMap
            ((m) => {
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
                freeVars: Vector[Term]): Option[Term] = {
    val fn         = multisub(func, unifMap)
    val x          = Try(multisub(arg, unifMap)).fold(
      fa => {
        pprint.log(func)
        pprint.log(arg)
        pprint.log(unifMap)
        throw fa
    }, identity)
    val lambdaVars = freeVars filter ((x) => !(unifMap.keySet contains x))
    import Fold._
    Try(polyLambda(lambdaVars.toList, fn(x))).toOption
  }

  def unifApply(func: Term, arg: Term, freeVars: Vector[Term]): Option[Term] =
    func match {
      case fn: FuncLike[u, v] =>
        unify(fn.dom, arg.typ, (t) => freeVars contains t) flatMap
          (subsApply(func, arg, _, freeVars))
      case _ => None
    }

  def appln(func: Term,
            arg: Term,
            freeVars: Vector[Term] = Vector()): Option[Term] =
    unifApply(func, arg, freeVars) orElse
      (func match {
        case fn: FuncLike[u, v] =>
          val l = funcToLambda(fn)
          appln(l.value, arg, l.variable +: freeVars)
        case _ => None
      })

  /**
   * Given a function and a target type, optionally returns a function with eventual codomain the given type;
   * this is done by attempting to unify, filling in parameters where they are determined and returing lambdas if all parameters work. 
   */
  def targetCodomain(func: Term,
                codomain: Term,
                freeVars: Vector[Term] = Vector()): Option[Term] =
    unify(func.typ, codomain, (t) => freeVars.contains(t)).map{
      unifMap => 
        val value = multisub(func, unifMap)
        // pprint.log(unifMap.keySet -- freeVars.toSet)
        val exVars = extraVars(freeVars, unifMap)
        polyLambda(exVars.reverse.toList, value)
    }.orElse{
      func match {
        case fn: FuncLike[u, v] =>
          val l = funcToLambda(fn)
          targetCodomain(l.value, codomain, freeVars :+ l.variable).orElse{
            codomain match {
              case pd: PiDefn[a, b] if pd.domain == fn.dom => 
                val target = pd.value.replace(pd.variable, l.variable)
                targetCodomain(l.value, target, freeVars).map{t => lambda(l.variable)(t)} 
              case pd: PiDefn[a, b] =>
                unify(fn.dom, pd.domain, (t) => freeVars.contains(t)).flatMap{unifMap => 
                  val variable = multisub(pd.variable, unifMap)
                  val value = multisub(pd.value, unifMap)
                  pprint.log(unifMap.keySet -- freeVars.toSet)
                  val exVars = extraVars(freeVars, unifMap)
                  val target = value.replace(variable, l.variable)
                  targetCodomain(value, target, exVars).map{t => lambda(variable)(t)} 
                }

              case _ =>
                None
            }
          }
        case _ => None
      }
    }

  def purgeInv(r1: Term,
               inv1: Set[(Term, Term)],
               r2: Term,
               inv2: Set[(Term, Term)],
               freeVars: Term => Boolean): Set[(Term, Term)] = {
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
                  freeVars: Term => Boolean): (Term, Set[(Term, Term)]) =
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
        val needHead = !(tail exists ((fx) =>
          unifyAll(isVar)(fx._1 -> head._1, fx._2 -> head._2).isDefined))
        if (needHead) purgedPairsList(tail, head :: accum)
        else purgedPairsList(tail, accum)
    }

  def purgedPairs(fxs: Set[(Term, Term)]): Set[(Term, Term)] =
    purgedPairsList(fxs.toList).toSet

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
