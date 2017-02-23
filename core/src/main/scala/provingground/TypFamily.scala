package provingground

import HoTT._

import scala.language.existentials

import shapeless._

import HList._

trait Subst[A] {
  def subst(a: A)(x: Term, y: Term): A
}

object Subst {
  implicit def termSubst[U <: Term with Subs[U]]: Subst[U] =
    new Subst[U] {
      def subst(a: U)(x: Term, y: Term) = a.replace(x, y)
    }

  // implicit object UnitSubst extends Subst[Unit] {
  //   def subst(a: Unit)(x: Term, y: Term) = a
  // }

  implicit object HNilSubst extends Subst[HNil]{
    def subst(a: HNil)(x: Term, y: Term) = a
  }

  implicit def hConsSubst[U : Subst, V <: HList : Subst]: Subst[U :: V] =
    new Subst[U :: V]{
    def subst(a: U :: V)(x: Term, y: Term) =
      implicitly[Subst[U]].subst(a.head)(x, y) ::
         implicitly[Subst[V]].subst(a.tail)(x, y)
  }

  // implicit def pairSubst[U: Subst, V: Subst]: Subst[(U, V)] =
  //   new Subst[(U, V)] {
  //     def subst(a: (U, V))(x: Term, y: Term) =
  //       (implicitly[Subst[U]].subst(a.head)(x, y),
  //        implicitly[Subst[V]].subst(a.tail)(x, y))
  //   }

  implicit class SubstOp[A: Subst](a: A) {
    def subst(x: Term, y: Term) = implicitly[Subst[A]].subst(a)(x, y)
  }
}

object TestS { implicitly[Subst[Term :: HNil]] }

sealed abstract class TypFamilyPtn[
    H <: Term with Subs[H], F <: Term with Subs[F], Index <: HList : Subst] {
  def getIndex(w: F, typ: Typ[H]): Option[Index]

  def typ(w: F, index: Index): Typ[H]

  def subs(x: Term, y: Term): TypFamilyPtn[H, F, Index]

  def mapper[C <: Term with Subs[C]]
    : TypFamilyMapper[H, F, C, Index, IF, IDF, IDFT] forSome {
      type IF <: Term with Subs[IF];
      type IDF <: Term with Subs[IDF];
      type IDFT <: Term with Subs[IDFT]
    }

  def getMapper[C <: Term with Subs[C], IF <: Term with Subs[IF],
      IDF <: Term with Subs[IDF],
      IDFT <: Term with Subs[IDFT]](cod: Typ[C])(implicit mpr : TypFamilyMapper[H, F, C, Index, IF, IDF, IDFT]) = mpr

  def mapped[C <: Term with Subs[C]] = mapper[C].mapper(this)
}

object TypFamilyPtn {
  def apply[H <: Term with Subs[H], F <: Term with Subs[F], Index <: HList : Subst](w: F)(
      implicit g: TypFamilyPtnGetter[F, H, Index]) =
    g.get(w)

  import TypFamilyMapper._

  case class IdTypFamily[H <: Term with Subs[H]]()
      extends TypFamilyPtn[H, Typ[H], HNil] {
    def getIndex(w: Typ[H], typ: Typ[H]) = Some(HNil)

    def typ(w: Typ[H], index: HNil): Typ[H] = w

    def subs(x: Term, y: Term) = this

    def mapper[C <: Term with Subs[C]] = idTypFamilyMapper[H, C]
  }

  case class FuncTypFamily[U <: Term with Subs[U],
                           H <: Term with Subs[H],
                           TF <: Term with Subs[TF],
                           TI <: HList : Subst](head: Typ[U],
                                      tail: TypFamilyPtn[H, TF, TI])
      extends TypFamilyPtn[H, Func[U, TF], U :: TI] {

    def getIndex(w: Func[U, TF], typ: Typ[H]) = {
      val argOpt = getArg(w)(typ)
      argOpt flatMap { (arg) =>
        tail.getIndex(w(arg), typ).map((arg :: _))
      }
    }

    def typ(w: Func[U, TF], index: (U :: TI)): Typ[H] =
      tail.typ(w(index.head), index.tail)

    def subs(x: Term, y: Term) =
      FuncTypFamily(head.replace(x, y), tail.subs(x, y))

    def mapper[C <: Term with Subs[C]] =
      funcTypFamilyMapper(tail.mapper[C], implicitly[Subst[TI]])
  }

  case class DepFuncTypFamily[U <: Term with Subs[U],
                              H <: Term with Subs[H],
                              TF <: Term with Subs[TF],
                              TI <: HList : Subst](
      head: Typ[U],
      tailfibre: U => TypFamilyPtn[H, TF, TI])
      extends TypFamilyPtn[H, FuncLike[U, TF], U :: TI] {

    def getIndex(w: FuncLike[U, TF], typ: Typ[H]) = {
      val argOpt = getArg(w)(typ)
      argOpt flatMap { (arg) =>
        tailfibre(arg).getIndex(w(arg), typ).map((arg :: _))
      }
    }

    def typ(w: FuncLike[U, TF], index: U :: TI) : Typ[H] =
      tailfibre(index.head).typ(w(index.head), index.tail)

    def subs(x: Term, y: Term) =
      DepFuncTypFamily(head.replace(x, y), (u: U) => tailfibre(u).subs(x, y))

    def mapper[C <: Term with Subs[C]] =
      depFuncTypFamilyMapper(tailfibre(head.Var).mapper[C],
                             implicitly[Subst[TI]])
  }
}

sealed trait TypFamilyMap[H <: Term with Subs[H],
                          F <: Term with Subs[F],
                          C <: Term with Subs[C],
                          Index <: HList,
                          IF <: Term with Subs[IF],
                          IDF <: Term with Subs[IDF],
                          IDFT <: Term with Subs[IDFT]] {

  val pattern: TypFamilyPtn[H, F, Index]

  def iterFuncTyp(w: Typ[H], x: Typ[C]): Typ[IF]

  def iterDepFuncTyp(w: Typ[H], xs: IDFT): Typ[IDF]

  def iterFunc(funcs: Index => Func[H, C]): IF

  def iterDepFunc(funcs: Index => FuncLike[H, C]): IDF

  def restrict(f: IF, ind: Index): Func[H, C]

  def depRestrict(f: IDF, ind: Index): FuncLike[H, C]

  def typRestrict(xs: IDFT, ind: Index): Func[H, Typ[C]]

  def subs(x: Term, y: Term): TypFamilyMap[H, F, C, Index, IF, IDF, IDFT]
}

object TypFamilyMap {

  import TypFamilyPtn._

  case class IdTypFamilyMap[H <: Term with Subs[H], C <: Term with Subs[C]]()
      extends TypFamilyMap[H,
                           Typ[H],
                           C,
                           HNil,
                           Func[H, C],
                           FuncLike[H, C],
                           Func[H, Typ[C]]] {

    val pattern = IdTypFamily[H]

    def iterFuncTyp(w: Typ[H], x: Typ[C]) = w ->: x

    def iterDepFuncTyp(w: Typ[H], xs: Func[H, Typ[C]]) = PiDefn(xs)

    def iterFunc(funcs: HNil => Func[H, C]) = funcs(HNil)

    def iterDepFunc(funcs: HNil => FuncLike[H, C]) = funcs(HNil)

    def restrict(f: Func[H, C], ind: HNil) = f

    def depRestrict(f: FuncLike[H, C], ind: HNil) = f

    def typRestrict(xs: Func[H, Typ[C]], ind: HNil) = xs

    def subs(x: Term, y: Term) = this
  }

    case class IdSubTypFamilyMap[H <: Term with Subs[H], C <: Term with Subs[C], TC <: Typ[C] with Subs[TC]]()
      extends TypFamilyMap[H,
                           Typ[H],
                           C,
                           HNil,
                           Func[H, C],
                           FuncLike[H, C],
                           Func[H, TC]] {

    val pattern = IdTypFamily[H]

    def iterFuncTyp(w: Typ[H], x: Typ[C]) = w ->: x

    def iterDepFuncTyp(w: Typ[H], xs: Func[H, TC]) = PiDefn(xs)

    def iterFunc(funcs: HNil => Func[H, C]) = funcs(HNil)

    def iterDepFunc(funcs: HNil => FuncLike[H, C]) = funcs(HNil)

    def restrict(f: Func[H, C], ind: HNil) = f

    def depRestrict(f: FuncLike[H, C], ind: HNil) = f

    def typRestrict(xs: Func[H, TC], ind: HNil) = xs

    def subs(x: Term, y: Term) = this
  }



  case class FuncTypFamilyMap[U <: Term with Subs[U],
                              H <: Term with Subs[H],
                              TF <: Term with Subs[TF],
                              C <: Term with Subs[C],
                              TIndex <: HList: Subst,
                              TIF <: Term with Subs[TIF],
                              TIDF <: Term with Subs[TIDF],
                              TIDFT <: Term with Subs[TIDFT]](
      head: Typ[U],
      tail: TypFamilyMap[H, TF, C, TIndex, TIF, TIDF, TIDFT])
      extends TypFamilyMap[H,
                           Func[U, TF],
                           C,
                           U :: TIndex,
                           FuncLike[U, TIF],
                           FuncLike[U, TIDF],
                           FuncLike[U, TIDFT]] {

    val pattern = FuncTypFamily(head, tail.pattern)

    def iterFuncTyp(w: Typ[H], x: Typ[C]) = head ->: tail.iterFuncTyp(w, x)

    def iterDepFuncTyp(w: Typ[H], xs: FuncLike[U, TIDFT]) = {
      val x = head.Var
      x ~>: (tail.iterDepFuncTyp(w, xs(x)))
    }

    def iterFunc(funcs: (U :: TIndex) => Func[H, C]) = {
      val x = head.Var
      x :~> (tail.iterFunc((ti: TIndex) => funcs(x :: ti)))
    }

    def iterDepFunc(funcs: ((U :: TIndex)) => FuncLike[H, C]) = {
      val x = head.Var
      x :~> (tail.iterDepFunc((ti: TIndex) => funcs(x :: ti)))
    }

    def restrict(f: FuncLike[U, TIF], ind: (U :: TIndex)) =
      tail.restrict(f(ind.head), ind.tail)

    def depRestrict(f: FuncLike[U, TIDF], ind: (U :: TIndex)) =
      tail.depRestrict(f(ind.head), ind.tail)

    def typRestrict(xs: FuncLike[U, TIDFT], ind: (U :: TIndex)) =
      tail.typRestrict(xs(ind.head), ind.tail)

    def subs(x: Term, y: Term) =
      FuncTypFamilyMap(head.replace(x, y), tail.subs(x, y))
  }

  case class DepFuncTypFamilyMap[U <: Term with Subs[U],
                                 H <: Term with Subs[H],
                                 TF <: Term with Subs[TF],
                                 C <: Term with Subs[C],
                                 TIndex <: HList : Subst,
                                 TIF <: Term with Subs[TIF],
                                 TIDF <: Term with Subs[TIDF],
                                 TIDFT <: Term with Subs[TIDFT]](
      head: Typ[U],
      tailfibre: U => TypFamilyMap[H, TF, C, TIndex, TIF, TIDF, TIDFT])
      extends TypFamilyMap[H,
                           FuncLike[U, TF],
                           C,
                           (U :: TIndex),
                           FuncLike[U, TIF],
                           FuncLike[U, TIDF],
                           FuncLike[U, TIDFT]] {

    val pattern = DepFuncTypFamily(head, (u: U) => tailfibre(u).pattern)

    def iterFuncTyp(w: Typ[H], x: Typ[C]) = {
      val y = head.Var
      y ~>: tailfibre(y).iterFuncTyp(w, x)
    }

    def iterDepFuncTyp(w: Typ[H], xs: FuncLike[U, TIDFT]) = {
      val x = head.Var
      x ~>: (tailfibre(x).iterDepFuncTyp(w, xs(x)))
    }

    def iterFunc(funcs: ((U :: TIndex)) => Func[H, C]) = {
      val x = head.Var
      x :~> (tailfibre(x).iterFunc((ti: TIndex) => funcs(x :: ti)))
    }

    def iterDepFunc(funcs: ((U :: TIndex)) => FuncLike[H, C]) = {
      val x = head.Var
      x :~> (tailfibre(x).iterDepFunc((ti: TIndex) => funcs(x :: ti)))
    }

    def restrict(f: FuncLike[U, TIF], ind: (U :: TIndex)) =
      tailfibre(ind.head).restrict(f(ind.head), ind.tail)

    def depRestrict(f: FuncLike[U, TIDF], ind: (U :: TIndex)) =
      tailfibre(ind.head).depRestrict(f(ind.head), ind.tail)

    def typRestrict(xs: FuncLike[U, TIDFT], ind: (U :: TIndex)) =
      tailfibre(ind.head).typRestrict(xs(ind.head), ind.tail)

    def subs(x: Term, y: Term) =
      DepFuncTypFamilyMap(head.replace(x, y),
                          (u: U) => tailfibre(u).subs(x, y))
  }
}

sealed trait TypFamilyMapper[H <: Term with Subs[H],
                             F <: Term with Subs[F],
                             C <: Term with Subs[C],
                             Index <: HList,
                             IF <: Term with Subs[IF],
                             IDF <: Term with Subs[IDF],
                             IDFT <: Term with Subs[IDFT]] {

  val mapper: TypFamilyPtn[H, F, Index] => TypFamilyMap[H,
                                                        F,
                                                        C,
                                                        Index,
                                                        IF,
                                                        IDF,
                                                        IDFT]
}

trait WeakImplicit{
  implicit def idSubTypFamilyMapper[H <: Term with Subs[H],
                                 C <: Term with Subs[C],
                                 TC <: Typ[C] with Subs[TC]] =
    new TypFamilyMapper[H,
                        Typ[H],
                        C,
                        HNil,
                        Func[H, C],
                        FuncLike[H, C],
                        Func[H, TC]] {
      val mapper = (x: TypFamilyPtn[H, Typ[H], HNil]) => TypFamilyMap.IdSubTypFamilyMap[H, C, TC]
    }
}

object TypFamilyMapper extends WeakImplicit {
  import TypFamilyMap._

  import TypFamilyPtn._

  implicit def idTypFamilyMapper[H <: Term with Subs[H],
                                 C <: Term with Subs[C]] =
    new TypFamilyMapper[H,
                        Typ[H],
                        C,
                        HNil,
                        Func[H, C],
                        FuncLike[H, C],
                        Func[H, Typ[C]]] {
      val mapper = (x: TypFamilyPtn[H, Typ[H], HNil]) => IdTypFamilyMap[H, C]
    }

  implicit def funcTypFamilyMapper[U <: Term with Subs[U],
                                   H <: Term with Subs[H],
                                   TF <: Term with Subs[TF],
                                   C <: Term with Subs[C],
                                   TIndex <: HList,
                                   TIF <: Term with Subs[TIF],
                                   TIDF <: Term with Subs[TIDF],
                                   TIDFT <: Term with Subs[TIDFT]](
      implicit tail: TypFamilyMapper[H, TF, C, TIndex, TIF, TIDF, TIDFT],
      subst: Subst[TIndex]) =
    new TypFamilyMapper[H,
                        Func[U, TF],
                        C,
                        (U :: TIndex),
                        FuncLike[U, TIF],
                        FuncLike[U, TIDF],
                        FuncLike[U, TIDFT]] {

      val mapper: TypFamilyPtn[H, Func[U, TF], (U :: TIndex)] => TypFamilyMap[
        H,
        Func[U, TF],
        C,
        (U :: TIndex),
        FuncLike[U, TIF],
        FuncLike[U, TIDF],
        FuncLike[U, TIDFT]] = {
        case FuncTypFamily(h, t) =>
          FuncTypFamilyMap(h, tail.mapper(t))
      }
    }

  implicit def depFuncTypFamilyMapper[U <: Term with Subs[U],
                                      H <: Term with Subs[H],
                                      TF <: Term with Subs[TF],
                                      C <: Term with Subs[C],
                                      TIndex <: HList,
                                      TIF <: Term with Subs[TIF],
                                      TIDF <: Term with Subs[TIDF],
                                      TIDFT <: Term with Subs[TIDFT]](
      implicit tail: TypFamilyMapper[H, TF, C, TIndex, TIF, TIDF, TIDFT],
      subst: Subst[TIndex]) =
    new TypFamilyMapper[H,
                        FuncLike[U, TF],
                        C,
                        (U :: TIndex),
                        FuncLike[U, TIF],
                        FuncLike[U, TIDF],
                        FuncLike[U, TIDFT]] {

      val mapper: TypFamilyPtn[H, FuncLike[U, TF], (U :: TIndex)] => TypFamilyMap[
        H,
        FuncLike[U, TF],
        C,
        (U :: TIndex),
        FuncLike[U, TIF],
        FuncLike[U, TIDF],
        FuncLike[U, TIDFT]] = {
        case DepFuncTypFamily(h, tf) =>
          DepFuncTypFamilyMap(h, (u: U) => tail.mapper(tf(u)))
      }
    }
}

trait TypFamilyPtnGetter[F <: Term with Subs[F], H <: Term with Subs[H], Index <: HList] {

//  type Index

  def get(w: F): TypFamilyPtn[H, F, Index]

  implicit val subst: Subst[Index]
}

object TypFamilyPtnGetter {
  implicit def idGetter[H <: Term with Subs[H]]
    : TypFamilyPtnGetter[Typ[H], H, HNil] =
    new TypFamilyPtnGetter[Typ[H], H, HNil] {
//      type Index = HNil

      def get(w: Typ[H]) = TypFamilyPtn.IdTypFamily[H]

      val subst = Subst.HNilSubst
    }

  implicit def funcTypFamilyGetter[TF <: Term with Subs[TF],
                                   U <: Term with Subs[U],
                                   H <: Term with Subs[H],
                                   TI <: HList](
      implicit tail: TypFamilyPtnGetter[TF, H, TI])
    : TypFamilyPtnGetter[Func[U, TF], H, U :: TI] =
    new TypFamilyPtnGetter[Func[U, TF], H, U :: TI] {

      def get(w: Func[U, TF]) =
        TypFamilyPtn.FuncTypFamily(w.dom, tail.get(w(w.dom.Var)))

      // type Index = (U :: TI)

      implicit val ts: Subst[TI] = tail.subst

      val subst = Subst.hConsSubst[U, TI]
    }

  implicit def depFuncTypFamilyGetter[TF <: Term with Subs[TF],
                                      U <: Term with Subs[U],
                                      H <: Term with Subs[H],
                                      TI <: HList](
      implicit tail: TypFamilyPtnGetter[TF, H, TI])
    : TypFamilyPtnGetter[FuncLike[U, TF], H, U :: TI] =
    new TypFamilyPtnGetter[FuncLike[U, TF], H, U :: TI] {
      def get(w: FuncLike[U, TF]) =
        TypFamilyPtn.DepFuncTypFamily(w.dom, (u: U) => tail.get(w(u)))

      // type Index = (U, tail.Index)

      implicit val ts: Subst[TI] = tail.subst

      val subst = Subst.hConsSubst[U, TI]
    }
}
