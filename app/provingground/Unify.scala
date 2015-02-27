import provingground.HoTT._
import provingground.Contexts._
import scala.util._
import scala.reflect.runtime.universe.{Try => UnivTry, Function => FunctionUniv, _}

object Unify{
  def multisub[U <: Term with Subs[U]](x: U, m : Map[Term, Term]): U = m.toList  match {
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
      case a :: bs => if (y.getOrElse(a, x(a)) == x(a)) Some(y + ((a, x(a)))) else None
    }

  def mergeOptMaps[U, V](x: Option[Map[U, V]], y: Option[Map[U, V]]): Option[Map[U, V]] =
    x flatMap ((a) =>
      y flatMap ((b) =>
        mergeMaps(a, b)
  ))

  def unify[U<: Term with Subs[U]](lhs: U, rhs: U, freevars: List[Term]): Option[Map[Term, Term]] = {
    if (!dependsOn(lhs)(freevars))
      {if (lhs == rhs) Some(Map()) else None}
    else
      (lhs, rhs) match {
      case (PiTyp(f), PiTyp(g)) => unify(f,g, freevars)
//      case (SigmaTyp(f), SigmaTyp(g)) => unify(f,g, freevars)
      case (FuncTyp(a, b), FuncTyp(c, d)) =>
        mergeOptMaps(unify(a,c, freevars), unify(b,d,freevars))
      case (PlusTyp(a, b), PlusTyp(c, d)) =>
        mergeOptMaps(unify(a,c, freevars), unify(b,d,freevars))
      case (x: AbsPair[_, _], y: AbsPair[_, _])=>
        mergeOptMaps(unify(x.first, y.first, freevars), unify(x.second, y.second,freevars))
      case (applptnterm(a, b), applptnterm(c, d)) =>
        mergeOptMaps(unify(a,c, freevars), unify(b,d,freevars))
      case _ => None
    }
  }

  def mkLambda[U <: Term with Subs[U] , V<: Term with Subs[V] ]: FuncLike[U, V] => FuncLike[U, V] = {
    case lm: Lambda[U, V] => lm
    case f: FuncLike[U,V] => {
    	val newvar = f.dom.obj.asInstanceOf[U]
    	lambda(newvar)(f(newvar))
    }
  }

  def mkLmbda[U <: Term with Subs[U] , V<: Term with Subs[V] ] :Func[U, V] => Func[U, V] = {
    case lm: LambdaFixed[U, V] => lm
    case f: Func[U,V] => {
    	val newvar = f.dom.obj.asInstanceOf[U]
    	lmbda(newvar)(f(newvar))
    }
  }


object Old{
  def multisub(x: Term, m : Map[Term, Term]): Term = m.toList  match {
    case List() => x
    case (a, b) :: tail => multisub(x.subs(a, b), tail.toMap)
  }

  def unify(source: Term, target: Term, freevars: Set[Term]) : Option[Map[Term,Term]] = (source, target) match {
    case (x, y) if freevars contains x => Some(Map(x -> y))
    case (x: Symbolic, y: Symbolic) if x.typ != y.typ =>
      unify(x.typ, y.typ, freevars) flatMap((m) =>
      unify(multisub(x, m), y, freevars -- m.keySet))
    case (applptnterm(f, x), applptnterm(g, y)) =>
      for (mx <- unify(f, g, freevars); my <- unify(multisub(x, mx), y, freevars -- mx.keySet)) yield (mx ++ my)
    case (FuncTyp(a, b), FuncTyp(c, d)) =>
      for (mx <- unify(a, c, freevars); my <- unify(multisub(b, mx), d, freevars -- mx.keySet)) yield (mx ++ my)
    case (Lambda(a, b : Term), Lambda(c, d : Term)) =>
      for (mx <- unify(a, c, freevars); my <- unify(multisub(b, mx), d, freevars -- mx.keySet)) yield (mx ++ my)
    case (PiTyp(a), PiTyp(c)) =>
      unify(a, c, freevars)
    case (SigmaTyp(a), SigmaTyp(c)) =>
      unify(a, c, freevars)
    case _ => None
  }

  def unifyctx(source: Term, target: Term, freevars: Set[Term], ctx: Context[Term, Term]) : Option[Map[Term,Term]] = ctx match {
    case _ : Context.empty[_] => unify(source, target, freevars)
    case LambdaMixin(x, tail, _) => unifyctx(source, target, freevars + x, tail)
    case _ => unifyctx(source, target, freevars, ctx.tail)
  }
}
}
