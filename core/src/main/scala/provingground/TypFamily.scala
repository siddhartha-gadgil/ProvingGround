package provingground

import HoTT._

import scala.language.existentials

trait Subst[A]{
  def subst(a: A)(x: Term, y: Term): A
}

object Subst{
  implicit def  termSubst[U <: Term with Subs[U]] : Subst[U] =
    new Subst[U]{
    def subst(a: U)(x: Term, y: Term) = a.replace(x, y)
  }

  implicit object UnitSubst extends Subst[Unit]{
    def subst(a: Unit)(x: Term, y: Term) = a
  }

  implicit def pairSubst[U : Subst, V: Subst] : Subst[(U, V)]  =
    new Subst[(U, V)]{
      def subst(a: (U, V))(x: Term, y: Term) =
        ( implicitly[Subst[U]].subst(a._1)(x, y),
          implicitly[Subst[V]].subst(a._2)(x, y))
  }

  implicit class SubstOp[A: Subst](a: A){
    def subst(x: Term, y: Term) = implicitly[Subst[A]].subst(a)(x, y)
}
}

object TestS{ implicitly[Subst[(Unit, Term)]]}


sealed abstract class TypFamilyPtn[H <: Term with Subs[H], F <: Term with Subs[F], Index: Subst] {
  def getIndex(w: F, typ: Typ[H]) : Option[Index]

  def typ(w: F, index: Index): Typ[H]

  def subs(ind: Index)(x: Term, y: Term) : Index // not needed, use `Subst`

  def mapper[C <: Term with Subs[C]] :
    TypFamilyMapper[H, F, C, Index, IF, IDF, IDFT] forSome{
      type IF <: Term with Subs[IF];
      type IDF <: Term with Subs[IDF];
      type IDFT <: Term with Subs[IDFT]
  }

  def mapped[C <: Term with Subs[C]] = mapper[C].mapper(this)
}

object TypFamilyPtn{
  import TypFamilyMapper._

  case class IdTypFamily[H <: Term with Subs[H]]() extends TypFamilyPtn[H, Typ[H], Unit]{
    def getIndex(w: Typ[H], typ: Typ[H]) = Some(())

    def typ(w: Typ[H], index: Unit): Typ[H] = w

    def subs(ind: Unit)(x: Term, y: Term)  = ind

    def mapper[C <: Term with Subs[C]] = idTypFamilyMapper[H, C]


  }

  case class FuncTypFamily[
    U<: Term with Subs[U],
    H <: Term with Subs[H], TF <: Term with Subs[TF], TI : Subst](
    head: Typ[U], tail: TypFamilyPtn[H, TF, TI]) extends TypFamilyPtn[H, Func[U, TF], (U, TI)]{

    def getIndex(w: Func[U, TF], typ: Typ[H]) = {
      val argOpt = getArg(w)(typ)
      argOpt flatMap {(arg) => tail.getIndex(w(arg), typ).map((arg, _))}
    }

    def typ(w: Func[U, TF], index: (U, TI)): Typ[H] = tail.typ(w(index._1), index._2)

    def subs(ind: (U, TI))(x: Term, y: Term) = (ind._1.replace(x, y), tail.subs(ind._2)(x, y))

    def mapper[C <: Term with Subs[C]] =
      funcTypFamilyMapper(tail.mapper[C], implicitly[Subst[TI]])
  }

  case class DepFuncTypFamily[
    U<: Term with Subs[U],
    H <: Term with Subs[H], TF <: Term with Subs[TF], TI : Subst](
    head: Typ[U], tailfibre: U =>TypFamilyPtn[H, TF, TI]) extends TypFamilyPtn[H, Func[U, TF], (U, TI)]{

    def getIndex(w: Func[U, TF], typ: Typ[H]) = {
      val argOpt = getArg(w)(typ)
      argOpt flatMap {(arg) => tailfibre(arg).getIndex(w(arg), typ).map((arg, _))}
    }

    def typ(w: Func[U, TF], index: (U, TI)): Typ[H] =
      tailfibre(index._1).typ(w(index._1), index._2)

    def subs(ind: (U, TI))(x: Term, y: Term) =
      (ind._1.replace(x, y), tailfibre(ind._1).subs(ind._2)(x, y))

    def mapper[C <: Term with Subs[C]] =
      depFuncTypFamilyMapper(tailfibre(head.Var).mapper[C], implicitly[Subst[TI]])
  }


}

sealed trait TypFamilyMap[
    H <: Term with Subs[H],
    F <: Term with Subs[F],
    C <: Term with Subs[C],
    Index,
    IF <: Term with Subs[IF],
    IDF <: Term with Subs[IDF],
    IDFT <: Term with Subs[IDFT]]{

    val pattern: TypFamilyPtn[H, F, Index]

    def iterFuncTyp(w: Typ[H], x : Typ[C]) : Typ[IF]

    def iterDepFuncTyp(w: Typ[H], xs: IDFT) : Typ[IDF]

    def iterFunc(funcs: Index => Func[H, C]): IF

    def restrict(f: IF, ind: Index) : Func[H, C]

    def depRestrict(f: IDF, ind: Index): FuncLike[H, C]

    def typRestrict(xs: IDFT, ind: Index) : Func[H, Typ[C]]

    def subs(x: Term, y: Term) : TypFamilyMap[H, F, C, Index, IF, IDF, IDFT]
  }

object TypFamilyMap{

  import TypFamilyPtn._

  case class IdTypFamilyMap[
    H<: Term with Subs[H],
    C<: Term with Subs[C]]() extends
    TypFamilyMap[
      H, Typ[H], C, Unit, Func[H, C], FuncLike[H, C], Func[H, Typ[C]]]{

    val pattern = IdTypFamily[H]

    def iterFuncTyp(w: Typ[H], x: Typ[C]) = w ->: x

    def iterDepFuncTyp(w: Typ[H], xs: Func[H, Typ[C]]) = PiTyp(xs)

    def iterFunc(funcs: Unit => Func[H, C]) = funcs(())

    def restrict(f: Func[H, C], ind: Unit) = f

    def depRestrict(f: FuncLike[H, C], ind: Unit) = f

    def typRestrict(xs: Func[H, Typ[C]], ind: Unit) = xs

    def subs(x: Term, y: Term) = this
  }

  case class FuncTypFamilyMap[
    U <: Term with Subs[U],
    H <: Term with Subs[H],
    TF <: Term with Subs[TF],
    C <: Term with Subs[C],
    TIndex : Subst,
    TIF <: Term with Subs[TIF],
    TIDF <: Term with Subs[TIDF],
    TIDFT <: Term with Subs[TIDFT]](
        head: Typ[U],
        tail : TypFamilyMap[H, TF, C, TIndex, TIF, TIDF, TIDFT]) extends
        TypFamilyMap[
          H, Func[U, TF], C, (U, TIndex), Func[U, TIF], FuncLike[U, TIDF], FuncLike[U, TIDFT]]{

    val pattern = FuncTypFamily(head, tail.pattern)

    def iterFuncTyp(w: Typ[H], x : Typ[C]) = head ->: tail.iterFuncTyp(w, x)

    def iterDepFuncTyp(w: Typ[H], xs: FuncLike[U, TIDFT]) = {
      val x = head.Var
      x ~>: (tail.iterDepFuncTyp(w, xs(x)))
    }

    def iterFunc(funcs: ((U, TIndex)) => Func[H, C]) = {
      val x = head.Var
      x :-> (tail.iterFunc((ti : TIndex) => funcs((x, ti))))
    }

    def restrict(f: Func[U, TIF], ind: (U, TIndex)) = tail.restrict(f(ind._1), ind._2)

    def depRestrict(f: FuncLike[U, TIDF], ind: (U, TIndex)) =
      tail.depRestrict(f(ind._1), ind._2)

    def typRestrict(xs: FuncLike[U, TIDFT], ind: (U, TIndex)) =
      tail.typRestrict(xs(ind._1), ind._2)

    def subs(x: Term, y: Term) = FuncTypFamilyMap(head.replace(x, y), tail.subs(x,y))
  }

  case class DepFuncTypFamilyMap[
    U <: Term with Subs[U],
    H <: Term with Subs[H],
    TF <: Term with Subs[TF],
    C <: Term with Subs[C],
    TIndex : Subst,
    TIF <: Term with Subs[TIF],
    TIDF <: Term with Subs[TIDF],
    TIDFT <: Term with Subs[TIDFT]](
        head: Typ[U],
        tailfibre : U => TypFamilyMap[H, TF, C, TIndex, TIF, TIDF, TIDFT]) extends
        TypFamilyMap[
          H, Func[U, TF], C, (U, TIndex), FuncLike[U, TIF], FuncLike[U, TIDF], FuncLike[U, TIDFT]]{

    val pattern = DepFuncTypFamily(head, (u: U) => tailfibre(u).pattern)

    def iterFuncTyp(w: Typ[H], x : Typ[C]) =
      {
      val y = head.Var
      y ~>: tailfibre(y).iterFuncTyp(w, x)
      }

    def iterDepFuncTyp(w: Typ[H], xs: FuncLike[U, TIDFT]) = {
      val x = head.Var
      x ~>: (tailfibre(x).iterDepFuncTyp(w, xs(x)))
    }

    def iterFunc(funcs: ((U, TIndex)) => Func[H, C]) = {
      val x = head.Var
      x :~> (tailfibre(x).iterFunc((ti : TIndex) => funcs((x, ti))))
    }

    def restrict(f: FuncLike[U, TIF], ind: (U, TIndex)) =
      tailfibre(ind._1).restrict(f(ind._1), ind._2)

    def depRestrict(f: FuncLike[U, TIDF], ind: (U, TIndex)) =
      tailfibre(ind._1).depRestrict(f(ind._1), ind._2)

     def typRestrict(xs: FuncLike[U, TIDFT], ind: (U, TIndex)) =
      tailfibre(ind._1).typRestrict(xs(ind._1), ind._2)

     def subs(x: Term, y: Term) =
       DepFuncTypFamilyMap(head.replace(x, y), (u: U) => tailfibre(u).subs(x, y))
  }

}

sealed trait TypFamilyMapper[
    H <: Term with Subs[H],
    F <: Term with Subs[F],
    C <: Term with Subs[C],
    Index,
    IF <: Term with Subs[IF],
    IDF <: Term with Subs[IDF],
    IDFT <: Term with Subs[IDFT]]{

  val mapper : TypFamilyPtn[H, F, Index] => TypFamilyMap[H, F, C, Index, IF, IDF, IDFT]
}

object TypFamilyMapper{
  import TypFamilyMap._

  import TypFamilyPtn._

  implicit def idTypFamilyMapper[H <: Term with Subs[H], C <: Term with Subs[C]] =
    new TypFamilyMapper[H, Typ[H], C, Unit, Func[H, C], FuncLike[H, C], Func[H, Typ[C]]]{
      val  mapper = (x: TypFamilyPtn[H, Typ[H], Unit]) => IdTypFamilyMap[H, C]
  }

  implicit def funcTypFamilyMapper[
    U <: Term with Subs[U],
    H <: Term with Subs[H],
    TF <: Term with Subs[TF],
    C <: Term with Subs[C],
    TIndex ,
    TIF <: Term with Subs[TIF],
    TIDF <: Term with Subs[TIDF],
    TIDFT <: Term with Subs[TIDFT]](
        implicit tail : TypFamilyMapper[H, TF, C, TIndex, TIF, TIDF, TIDFT],
        subst: Subst[TIndex]) =
      new TypFamilyMapper[
          H, Func[U, TF], C, (U, TIndex), Func[U, TIF], FuncLike[U, TIDF], FuncLike[U, TIDFT]]{

    val mapper : TypFamilyPtn[H, Func[U, TF], (U, TIndex)] => TypFamilyMap[
          H, Func[U, TF], C, (U, TIndex), Func[U, TIF], FuncLike[U, TIDF], FuncLike[U, TIDFT]] =
      {case FuncTypFamily(h , t) =>
        FuncTypFamilyMap(h, tail.mapper(t))}
  }

  implicit def depFuncTypFamilyMapper[
    U <: Term with Subs[U],
    H <: Term with Subs[H],
    TF <: Term with Subs[TF],
    C <: Term with Subs[C],
    TIndex,
    TIF <: Term with Subs[TIF],
    TIDF <: Term with Subs[TIDF],
    TIDFT <: Term with Subs[TIDFT]](
        implicit tail : TypFamilyMapper[H, TF, C, TIndex, TIF, TIDF, TIDFT], subst: Subst[TIndex]) =
      new TypFamilyMapper[
          H, Func[U, TF], C, (U, TIndex), FuncLike[U, TIF], FuncLike[U, TIDF], FuncLike[U, TIDFT]]{

    val mapper : TypFamilyPtn[H, Func[U, TF], (U, TIndex)] => TypFamilyMap[
          H, Func[U, TF], C, (U, TIndex), FuncLike[U, TIF], FuncLike[U, TIDF], FuncLike[U, TIDFT]] =
      {case DepFuncTypFamily(h , tf) =>
        DepFuncTypFamilyMap(h, (u: U) => tail.mapper(tf(u)))}
  }
}
