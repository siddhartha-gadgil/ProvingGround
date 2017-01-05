package provingground

import HoTT._
import math._
import scala.language.existentials

import shapeless._

abstract class IndexedIterFuncPtnMap[S <: Term with Subs[S],
                                     H <: Term with Subs[H],
                                     Fb <: Term with Subs[Fb],
                                     Index<: HList : Subst,
                                     C <: Term with Subs[C],
                                     F <: Term with Subs[F],
                                     TT <: Term with Subs[TT],
                                     DT <: Term with Subs[DT],
                                     IF <: Term with Subs[IF],
                                     IDF <: Term with Subs[IDF],
                                     IDFT <: Term with Subs[IDFT]] {

  /**
    * the universe containing the type
    */
  val univLevel: Int

  val family: TypFamilyMap[H, Fb, C, Index, IF, IDF, IDFT]

  // /**
  //   * scala type of target for induced functions
  //   */
  // type TargetType <: Term with Subs[TargetType]
  //
  // /**
  //   * Note that the target may be a type family, not a type,
  //   *  so this is the actual type of objects.
  //   */
  // type DepTargetType <: Term with Subs[DepTargetType]

  /**
    * returns the type corresponding to the pattern, such as A -> W, given the (inductive) type W,
    *  this is used mainly for constructor patterns, with the W being fixed.
    */
  def apply(W: Fb): Typ[F]

  /**
    * target scala type.
    */
  def target(x: Typ[C]): Typ[TT]

  /**
    * dependent target scala type.
    */
  def depTarget(xs: IDFT): F => Typ[DT]

  def subs(x: Term, y: Term)
    : IndexedIterFuncPtnMap[S, H, Fb, Index, C, F, TT, DT, IF, IDF, IDFT]

  /**
    * function induced by f: W -> X of type (A -> W) -> (A -> X) etc
    *
    * @param f function from which to induce
    *
    * @param W the inductive type
    *
    * @param X codomain of the given function
    */
  def induced(f: IF): F => TT

  /**
    * dependent function induced by dependent f: W -> X(s) of type (A -> W) -> ((a : A) ~> Xs(a)) etc
    *
    * @param f dependent function from which to induce
    *
    * @param W the inductive type
    *
    * @param Xs family of codomains of the given dependent function
    */
  def inducedDep(f: IDF): F => DT
}

object IndexedIterFuncPtnMap {

  /**
    * The identity family
    */
  case class IdIterPtnMap[H <: Term with Subs[H],
                          Fb <: Term with Subs[Fb],
                          Index<: HList : Subst,
                          C <: Term with Subs[C],
                          F <: Term with Subs[F],
                          IF <: Term with Subs[IF],
                          IDF <: Term with Subs[IDF],
                          IDFT <: Term with Subs[IDFT]](
      family: TypFamilyMap[H, Fb, C, Index, IF, IDF, IDFT],
      index: Index)
      extends IndexedIterFuncPtnMap[HeadTerm,
                                    H,
                                    Fb,
                                    Index,
                                    C,
                                    H,
                                    C,
                                    C,
                                    IF,
                                    IDF,
                                    IDFT] {

    def apply(W: Fb) = family.pattern.typ(W, index)

    //    type Family =  O

    type FamilyType = Typ[H]

    def target(x: Typ[C]) = x

    def depTarget(xs: IDFT) = family.typRestrict(xs, index)

    def subs(x: Term, y: Term) = this

    //    type Cod = C

    //    type MemberType = O

    val univLevel = 0

    /**
      * induced function is the given one.
      */
    def induced(f: IF) = family.restrict(f, index)

    /**
      * induced function is the given one.
      */
    def inducedDep(f: IDF) = family.depRestrict(f, index)
  }

  case class IndexedFuncIterPtnMap[HS <: Term with Subs[HS],
                                   TT <: Term with Subs[TT],
                                   V <: Term with Subs[V],
                                   T <: Term with Subs[T],
                                   D <: Term with Subs[D],
                                   H <: Term with Subs[H],
                                   Fb <: Term with Subs[Fb],
                                   Index<: HList : Subst,
                                   C <: Term with Subs[C],
                                   F <: Term with Subs[F],
                                   IF <: Term with Subs[IF],
                                   IDF <: Term with Subs[IDF],
                                   IDFT <: Term with Subs[IDFT]](
      tail: Typ[TT],
      head: IndexedIterFuncPtnMap[HS, H, Fb, Index, C, V, T, D, IF, IDF, IDFT]
  ) extends IndexedIterFuncPtnMap[Func[TT, HS],
                                    H,
                                    Fb,
                                    Index,
                                    C,
                                    Func[TT, V],
                                    Func[TT, T],
                                    FuncLike[TT, D],
                                    IF,
                                    IDF,
                                    IDFT] { self =>
    def apply(W: Fb) = FuncTyp[TT, V](tail, head(W))

    val family = head.family

    type DepTargetType = FuncLike[TT, D]

    type TargetType = Func[TT, T]

    def target(x: Typ[C]) = tail ->: head.target(x)

    def depTarget(xs: IDFT) =
      (fmly: Func[TT, V]) => {
        val a         = tail.Var
        val b         = fmly(a)
        val targfibre = lmbda(a)(headfibre(a).depTarget(xs)(b))
        piDefn(a)(headfibre(a).depTarget(xs)(b))
      }

    //    type Cod = head.Cod

    def subs(x: Term, y: Term) =
      IndexedFuncIterPtnMap(tail, head.subs(x, y))

    val headfibre = (arg: Term) => head

    val univLevel = max(head.univLevel, univlevel(tail.typ))

    /**
      * inductively defining the induced function.
      * maps (g : tail --> head(W)) to func : tail --> head(X) given (head(W) --> head(X))
      *
      */
    def induced(f: IF): Func[TT, V] => TargetType =
      (g: Func[TT, V]) => {
        val x = tail.Var
        // val g = apply(f).Var
        // lmbda(g)(
        lmbda(x)(head.induced(f)(g(x)))
        // )
        // ???
      }

    /**
      * inductively defining the induced function.
      * maps (g : tail --> head(W)) to func : (t : tail) ~> head(Xs(t)) given (head(W) --> (t: tail) ~> head(Xs(t)))
      *
      */
    def inducedDep(f: IDF): Func[TT, V] => DepTargetType =
      (g: Func[TT, V]) => {
        val x = tail.Var
        // val g = apply(f.dom).Var
        // lambda(g)(
        lambda(x)(head.inducedDep(f)(g(x)))
        // )
        // ???
      }
  }

  case class IndexedDepFuncIterPtnMap[HS <: Term with Subs[HS],
                                      TT <: Term with Subs[TT],
                                      V <: Term with Subs[V],
                                      T <: Term with Subs[T],
                                      D <: Term with Subs[D],
                                      H <: Term with Subs[H],
                                      Fb <: Term with Subs[Fb],
                                      Index<: HList : Subst,
                                      C <: Term with Subs[C],
                                      F <: Term with Subs[F],
                                      IF <: Term with Subs[IF],
                                      IDF <: Term with Subs[IDF],
                                      IDFT <: Term with Subs[IDFT]](
      tail: Typ[TT],
      headfibre: TT => IndexedIterFuncPtnMap[HS,
                                             H,
                                             Fb,
                                             Index,
                                             C,
                                             V,
                                             T,
                                             D,
                                             IF,
                                             IDF,
                                             IDFT]
  ) extends IndexedIterFuncPtnMap[FuncLike[TT, HS],
                                    H,
                                    Fb,
                                    Index,
                                    C,
                                    FuncLike[TT, V],
                                    FuncLike[TT, T],
                                    FuncLike[TT, D],
                                    IF,
                                    IDF,
                                    IDFT] { self =>
    def apply(W: Fb) = {
      val x = tail.Var
      x ~>: headfibre(x)(W)
    }

    val family = headfibre(tail.Var).family

    type DepTargetType = FuncLike[TT, D]

    type TargetType = Func[TT, T]

    def target(x: Typ[C]) = {
      val y = tail.Var
      y ~>: headfibre(y).target(x)
    }

    def depTarget(xs: IDFT) =
      (fmly: FuncLike[TT, V]) => {
        val a         = tail.Var
        val b         = fmly(a)
        val targfibre = lmbda(a)(headfibre(a).depTarget(xs)(b))
        piDefn(a)(headfibre(a).depTarget(xs)(b))
      }

    //    type Cod = head.Cod

    def subs(x: Term, y: Term) =
      IndexedDepFuncIterPtnMap(tail, (t: TT) => headfibre(t).subs(x, y))

//    val headfibre = (arg: Term) => head

    val univLevel = max(headfibre(tail.Var).univLevel, univlevel(tail.typ))

    /**
      * inductively defining the induced function.
      * maps (g : tail --> head(W)) to func : tail --> head(X) given (head(W) --> head(X))
      *
      */
    def induced(f: IF): FuncLike[TT, V] => TargetType =
      (g: FuncLike[TT, V]) => {
        val x = tail.Var
        // val g = apply(f).Var
        // lmbda(g)(
        lmbda(x)(headfibre(x).induced(f)(g(x)))
        // )
        // ???
      }

    /**
      * inductively defining the induced function.
      * maps (g : tail --> head(W)) to func : (t : tail) ~> head(Xs(t)) given (head(W) --> (t: tail) ~> head(Xs(t)))
      *
      */
    def inducedDep(f: IDF): FuncLike[TT, V] => DepTargetType =
      (g: FuncLike[TT, V]) => {
        val x = tail.Var
        // val g = apply(f.dom).Var
        // lambda(g)(
        lambda(x)(headfibre(x).inducedDep(f)(g(x)))
        // )
        // ???
      }
  }
}

abstract class IndexedIterFuncShape[S <: Term with Subs[S],
                                    H <: Term with Subs[H],
                                    Fb <: Term with Subs[Fb],
                                    Index<: HList : Subst] {
  def subs(x: Term, y: Term): IndexedIterFuncShape[S, H, Fb, Index]

  val family: TypFamilyPtn[H, Fb, Index]

  def mapper[C <: Term with Subs[C], IF <: Term with Subs[IF],
  IDF <: Term with Subs[IDF],
  IDFT <: Term with Subs[IDFT]](implicit fmlyMapper: TypFamilyMapper[
    H,
    Fb,
    C,
    Index,
    IF,
    IDF,
    IDFT]): IndexedIterFuncPtnMapper[S,
                                     H,
                                     Fb,
                                     Index,
                                     C,
                                     F,
                                     TT,
                                     DT,
                                     IF,
                                     IDF,
                                     IDFT] forSome {
    type F <: Term with Subs[F]; type TT <: Term with Subs[TT];
    type DT <: Term with Subs[DT]
  }

  def mapped[C <: Term with Subs[C],
             IF <: Term with Subs[IF],
             IDF <: Term with Subs[IDF],
             IDFT <: Term with Subs[IDFT]](
      implicit fmlyMapper: TypFamilyMapper[H, Fb, C, Index, IF, IDF, IDFT]) =
    mapper(fmlyMapper).mapper(fmlyMapper)(this)
}

import Subst._

object IndexedIterFuncShape {
  import IndexedIterFuncPtnMapper._

  case class IdIterShape[H <: Term with Subs[H],
                         Fb <: Term with Subs[Fb],
                         Index<: HList : Subst](
      family: TypFamilyPtn[H, Fb, Index],
      index: Index
  ) extends IndexedIterFuncShape[HeadTerm, H, Fb, Index] {
    def subs(x: Term, y: Term) =
      IdIterShape(family.subs(x, y), index.subst(x, y))

    def mapper[C <: Term with Subs[C],
               IF <: Term with Subs[IF],
               IDF <: Term with Subs[IDF],
               IDFT <: Term with Subs[IDFT]](
        implicit fmlyMapper: TypFamilyMapper[H, Fb, C, Index, IF, IDF, IDFT])
      : IndexedIterFuncPtnMapper[HeadTerm,
                                 H,
                                 Fb,
                                 Index,
                                 C,
                                 F,
                                 TT,
                                 DT,
                                 IF,
                                 IDF,
                                 IDFT] forSome {
        type F <: Term with Subs[F]; type TT <: Term with Subs[TT];
        type DT <: Term with Subs[DT]
      } = idIterPtnMapper
  }

  case class FuncShape[HS <: Term with Subs[HS],
                       TT <: Term with Subs[TT],
                       H <: Term with Subs[H],
                       Fb <: Term with Subs[Fb],
                       Index<: HList : Subst](
      head: Typ[TT],
      tail: IndexedIterFuncShape[HS, H, Fb, Index])
      extends IndexedIterFuncShape[Func[TT, HS], H, Fb, Index] {
    val family = tail.family

    def subs(x: Term, y: Term) = FuncShape(head.subs(x, y), tail.subs(x, y))

    def mapper[
        C <: Term with Subs[C],
        IF <: Term with Subs[IF],
        IDF <: Term with Subs[IDF],
        IDFT <: Term with Subs[IDFT]](implicit fmlyMapper: TypFamilyMapper[
      H,
      Fb,
      C,
      Index,
      IF,
      IDF,
      IDFT]) = {
      indexedFuncIterPtnMapper(implicitly[Subst[Index]],
                               tail.mapper(fmlyMapper))
    }
  }

  case class DepFuncShape[HS <: Term with Subs[HS],
                          TT <: Term with Subs[TT],
                          H <: Term with Subs[H],
                          Fb <: Term with Subs[Fb],
                          Index<: HList : Subst](
      head: Typ[TT],
      tailfibre: TT => IndexedIterFuncShape[HS, H, Fb, Index])
      extends IndexedIterFuncShape[FuncLike[TT, HS], H, Fb, Index] {
    val family = tailfibre(head.Var).family

    def subs(x: Term, y: Term) =
      DepFuncShape(head.subs(x, y), (t: TT) => tailfibre(t).subs(x, y))

    def mapper[
        C <: Term with Subs[C],
        IF <: Term with Subs[IF],
        IDF <: Term with Subs[IDF],
        IDFT <: Term with Subs[IDFT]](implicit fmlyMapper: TypFamilyMapper[
      H,
      Fb,
      C,
      Index,
      IF,
      IDF,
      IDFT]) = {
      indexedDepFuncIterPtnMapper(implicitly[Subst[Index]],
                                  tailfibre(head.Var).mapper(fmlyMapper))
    }
  }
}

abstract class IndexedIterFuncPtnMapper[S <: Term with Subs[S],
                                        H <: Term with Subs[H],
                                        Fb <: Term with Subs[Fb],
                                        Index<: HList : Subst,
                                        C <: Term with Subs[C],
                                        F <: Term with Subs[F],
                                        TT <: Term with Subs[TT],
                                        DT <: Term with Subs[DT],
                                        IF <: Term with Subs[IF],
                                        IDF <: Term with Subs[IDF],
                                        IDFT <: Term with Subs[IDFT]] {
  def mapper(
      implicit fmlyMapper: TypFamilyMapper[H, Fb, C, Index, IF, IDF, IDFT])
    : IndexedIterFuncShape[S, H, Fb, Index] => IndexedIterFuncPtnMap[S,
                                                                     H,
                                                                     Fb,
                                                                     Index,
                                                                     C,
                                                                     F,
                                                                     TT,
                                                                     DT,
                                                                     IF,
                                                                     IDF,
                                                                     IDFT]
}

object IndexedIterFuncPtnMapper {
  import IndexedIterFuncShape._

  import IndexedIterFuncPtnMap._

  implicit def idIterPtnMapper[H <: Term with Subs[H],
                               Fb <: Term with Subs[Fb],
                               Index<: HList : Subst,
                               C <: Term with Subs[C],
                               F <: Term with Subs[F],
                               IF <: Term with Subs[IF],
                               IDF <: Term with Subs[IDF],
                               IDFT <: Term with Subs[IDFT]] =
    new IndexedIterFuncPtnMapper[HeadTerm,
                                 H,
                                 Fb,
                                 Index,
                                 C,
                                 H,
                                 C,
                                 C,
                                 IF,
                                 IDF,
                                 IDFT] {
      def mapper(implicit fmlyMapper: TypFamilyMapper[H,
                                                      Fb,
                                                      C,
                                                      Index,
                                                      IF,
                                                      IDF,
                                                      IDFT]) = {
        case IdIterShape(fmly, index) =>
          IdIterPtnMap(fmlyMapper.mapper(fmly), index)
      }
    }

  implicit def indexedFuncIterPtnMapper[TT <: Term with Subs[TT],
                                        HS <: Term with Subs[HS],
                                        V <: Term with Subs[V],
                                        T <: Term with Subs[T],
                                        D <: Term with Subs[D],
                                        H <: Term with Subs[H],
                                        Fb <: Term with Subs[Fb],
                                        Index<: HList : Subst,
                                        C <: Term with Subs[C],
                                        F <: Term with Subs[F],
                                        IF <: Term with Subs[IF],
                                        IDF <: Term with Subs[IDF],
                                        IDFT <: Term with Subs[IDFT]](
      implicit hm: IndexedIterFuncPtnMapper[HS,
                                            H,
                                            Fb,
                                            Index,
                                            C,
                                            V,
                                            T,
                                            D,
                                            IF,
                                            IDF,
                                            IDFT]
  ) =
    new IndexedIterFuncPtnMapper[Func[TT, HS],
                                 H,
                                 Fb,
                                 Index,
                                 C,
                                 Func[TT, V],
                                 Func[TT, T],
                                 FuncLike[TT, D],
                                 IF,
                                 IDF,
                                 IDFT] {
      def mapper(implicit fmlyMapper: TypFamilyMapper[H,
                                                      Fb,
                                                      C,
                                                      Index,
                                                      IF,
                                                      IDF,
                                                      IDFT]) = {
        case FuncShape(tail, head) =>
          IndexedFuncIterPtnMap(tail, hm.mapper(fmlyMapper)(head))
      }
    }

  implicit def indexedDepFuncIterPtnMapper[HS <: Term with Subs[HS],
                                           TT <: Term with Subs[TT],
                                           V <: Term with Subs[V],
                                           T <: Term with Subs[T],
                                           D <: Term with Subs[D],
                                           H <: Term with Subs[H],
                                           Fb <: Term with Subs[Fb],
                                           Index<: HList : Subst,
                                           C <: Term with Subs[C],
                                           F <: Term with Subs[F],
                                           IF <: Term with Subs[IF],
                                           IDF <: Term with Subs[IDF],
                                           IDFT <: Term with Subs[IDFT]](
      implicit hm: IndexedIterFuncPtnMapper[HS,
                                            H,
                                            Fb,
                                            Index,
                                            C,
                                            V,
                                            T,
                                            D,
                                            IF,
                                            IDF,
                                            IDFT]
  ) =
    new IndexedIterFuncPtnMapper[FuncLike[TT, HS],
                                 H,
                                 Fb,
                                 Index,
                                 C,
                                 FuncLike[TT, V],
                                 FuncLike[TT, T],
                                 FuncLike[TT, D],
                                 IF,
                                 IDF,
                                 IDFT] {
      def mapper(implicit fmlyMapper: TypFamilyMapper[H,
                                                      Fb,
                                                      C,
                                                      Index,
                                                      IF,
                                                      IDF,
                                                      IDFT]) = {
        case DepFuncShape(tail, headfibre) =>
          IndexedDepFuncIterPtnMap(
            tail,
            (t: TT) => hm.mapper(fmlyMapper)(headfibre(t)))
      }
    }
}
