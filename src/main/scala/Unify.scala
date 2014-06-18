import provingGround.HoTT._
import provingGround.Contexts._

object Unify{
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