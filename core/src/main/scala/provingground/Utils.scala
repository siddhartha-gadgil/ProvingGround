package provingground
import HoTT._
import scala.util.Try

object Utils {
  import monix.eval._

  def refinedTask[A](
      init: A,
      task: Task[A],
      refine: Task[A] => Task[A]
  ): Task[A] =
    task.materialize.flatMap { t =>
      t.fold((_) => Task.now(init), (a) => refinedTask(a, refine(task), refine))
    }

  def bestTask[A](
      taskSeq: Seq[Task[A]],
      accum: Option[A] = None
  ): Task[Option[A]] =
    taskSeq.headOption
      .map(
        _.materialize.flatMap(
          t =>
            t.fold((_) => Task.now(accum), a => bestTask(taskSeq.tail, Some(a)))
        )
      )
      .getOrElse(Task.now(accum))

  def proofsEqual(x: Term, y: Term) =
    isProp(x.typ) && isProp(y.typ) && (x.typ == y.typ)

  def deducedEqual(x: Term, y: Term, base: (Term, Term) => Boolean): Boolean =
    (x == y) || base(x, y) || {
      (x, y) match {
        case (MiscAppln(f1, a1), MiscAppln(f2, a2)) =>
          deducedEqual(f1, f2, base) && deducedEqual(a1, a2, base)
        case (l1: LambdaLike[u1, v1], l2: LambdaLike[u2, v2]) =>
          deducedEqual(l1.variable.typ, l2.variable.typ, base) && deducedEqual(
            l1.value,
            l2.value.replace(l2.variable, l1.variable),
            base
          )
        case (l1: PiDefn[u1, v1], l2: PiDefn[u2, v2]) =>
          deducedEqual(l1.variable.typ, l2.variable.typ, base) && deducedEqual(
            l1.value,
            l2.value.replace(l2.variable, l1.variable),
            base
          )
        case (s1: SigmaTyp[u1, v1], s2: SigmaTyp[u2, v2]) =>
          deducedEqual(s1.fibers, s2.fibers, base)
        case (p1: AbsPair[u1, v1], p2: AbsPair[u2, v2]) =>
          deducedEqual(p1.first, p2.first, base) && deducedEqual(
            p1.second,
            p2.second,
            base
          )
        case (p1: FuncTyp[u1, v1], p2: FuncTyp[u2, v2]) =>
          deducedEqual(p1.dom, p2.dom, base) && deducedEqual(
            p1.codom,
            p2.codom,
            base
          )
        case (p1: IdentityTyp[u1], p2: IdentityTyp[u2]) =>
          deducedEqual(p1.dom, p2.dom, base) && deducedEqual(
            p1.lhs,
            p2.lhs,
            base
          ) && deducedEqual(p1.rhs, p2.rhs, base)
        case (p1: PlusTyp[u1, v1], p2: PlusTyp[u2, v2]) =>
          deducedEqual(p1.first, p2.first, base) && deducedEqual(
            p1.second,
            p2.second,
            base
          )
        case (p1: PlusTyp.FirstIncl[u1, v1], p2: PlusTyp.FirstIncl[u2, v2]) =>
          deducedEqual(p1.typ, p2.typ, base) && deducedEqual(
            p1.value,
            p2.value,
            base
          )
        case (p1: PlusTyp.ScndIncl[u1, v1], p2: PlusTyp.ScndIncl[u2, v2]) =>
          deducedEqual(p1.typ, p2.typ, base) && deducedEqual(
            p1.value,
            p2.value,
            base
          )
        case (p1: Refl[u1], p2: Refl[u2]) =>
          deducedEqual(p1.typ, p2.typ, base) && deducedEqual(
            p1.value,
            p2.value,
            base
          )
        case (s1: Symbolic, s2: Symbolic) =>
          (s1.name == s2.name) && deducedEqual(s1.typ, s2.typ, base)
        case (l1: GenFuncTyp[u1, v1], l2: GenFuncTyp[u2, v2]) =>
          deducedEqual(l1.domain, l2.domain, base) && {
            val x = l1.domain.Var
            Try(
              deducedEqual(
                l1.fib(x),
                l2.fib(x.asInstanceOf[u2]),
                base
              )
            ).getOrElse(false)
          }
        case (f1: IndRecFunc[u1, v1, f1], f2: IndRecFunc[u2, v2, f2]) =>
          deducedEqual(f1.domW, f2.domW, base) && deducedEqual(
            f1.codom,
            f2.codom,
            base
          ) && vecDeducedEqual(f1.defnData, f2.defnData, base) &&
            vecDeducedEqual(f1.index, f2.index, base)
        case (f1: RecFunc[u1, v1], f2: RecFunc[u2, v2]) =>
          deducedEqual(f1.dom, f2.dom, base) && deducedEqual(
            f1.codom,
            f2.codom,
            base
          ) && vecDeducedEqual(f1.defnData, f2.defnData, base)
        case (
            f1: IndInducFuncLike[u1, v1, f1, i1],
            f2: IndInducFuncLike[u2, v2, f2, i2]
            ) =>
          deducedEqual(f1.domW, f2.domW, base) && deducedEqual(
            f1.codXs,
            f2.codXs,
            base
          ) && vecDeducedEqual(f1.defnData, f2.defnData, base) &&
            vecDeducedEqual(f1.index, f2.index, base)

        case (f1: InducFuncLike[u1, v1], f2: InducFuncLike[u2, v2]) =>
          val x = f1.dom.Var
          deducedEqual(f1.dom, f2.dom, base) && deducedEqual(
            f1.depcodom(x),
            f2.depcodom(x.asInstanceOf[u2]),
            base
          ) && vecDeducedEqual(f1.defnData, f2.defnData, base)
        case _ => false
      }
    }

  def vecDeducedEqual(
      v1: Vector[Term],
      v2: Vector[Term],
      base: (Term, Term) => Boolean
  ) =
    (v1.size == v2.size) &&
      (v1.zip(v2)).forall { case (x, y) => deducedEqual(x, y, base) }
}
