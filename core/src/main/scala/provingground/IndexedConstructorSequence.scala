package provingground

import HoTT._

import scala.language.existentials

import Subst._

import shapeless._

abstract class IndexedConstructorSeqMap[C <: Term with Subs[C],
                                        H <: Term with Subs[H],
                                        RecType <: Term with Subs[RecType],
                                        InducType <: Term with Subs[InducType],
                                        Intros <: HList,
                                        F <: Term with Subs[F],
                                        Index<: HList : Subst,
                                        IF <: Term with Subs[IF],
                                        IDF <: Term with Subs[IDF],
                                        IDFT <: Term with Subs[IDFT]] {

  val family: TypFamilyMap[H, F, C, Index, IF, IDF, IDFT]

  def recDefn(
      X: Typ[C]): IndexedRecursiveDefinition[H, F, C, Index, IF, IDF, IDFT]

  val W: F

  // type RecType <: Term with Subs[RecType]

  def recDataLambda(X: Typ[C]): IF => RecType

  def rec(X: Typ[C]): RecType =
    recDataLambda(X: Typ[C])(recDefn(X: Typ[C]).iterFunc)

  def inducDefn(
      fibre: IDFT): IndexedInductiveDefinition[H, F, C, Index, IF, IDF, IDFT]

  // type InducType <: Term with Subs[InducType]

  def inducDataLambda(fibre: IDFT): IDF => InducType

  def induc(fibre: IDFT) =
    inducDataLambda(fibre)(inducDefn(fibre).iterDepFunc)
}

object IndexedConstructorSeqMap {
  case class Empty[C <: Term with Subs[C],
                   H <: Term with Subs[H],
                   F <: Term with Subs[F],
                   Index<: HList : Subst,
                   IF <: Term with Subs[IF],
                   IDF <: Term with Subs[IDF],
                   IDFT <: Term with Subs[IDFT]](
      W: F,
      family: TypFamilyMap[H, F, C, Index, IF, IDF, IDFT])
      extends IndexedConstructorSeqMap[C,
                                       H,
                                       IF,
                                       IDF,
                                       HNil,
                                       F,
                                       Index,
                                       IF,
                                       IDF,
                                       IDFT] {
    def recDefn(X: Typ[C]) = IndexedRecursiveDefinition.Empty(W, X, family)

    // type RecType = Func[H, C]

    def recDataLambda(X: Typ[C]) = (f) => f

    def inducDefn(Xs: IDFT) =
      IndexedInductiveDefinition.Empty(W, Xs, family)

    // type InducType = FuncLike[H, C]

    def inducDataLambda(fibre: IDFT) = (f) => f

    val intros = List()
  }

  import ConstructorSeqMap.{RecSym, InducSym}

  object Cons {
    def sym[
            C <: Term with Subs[C],
            H <: Term with Subs[H],
            Cod <: Term with Subs[Cod],
            RD <: Term with Subs[RD],
            ID <: Term with Subs[ID],
            TR <: Term with Subs[TR],
            TI <: Term with Subs[TI],
            TIntros <: HList,
            F <: Term with Subs[F],
            Index<: HList : Subst,
            IF <: Term with Subs[IF],
            IDF <: Term with Subs[IDF],
            IDFT <: Term with Subs[IDFT]](
        name: AnySym,
        pattern: IndexedConstructorPatternMap[
                                              Cod,
                                              C,
                                              H,
                                              RD,
                                              ID,
                                              F,
                                              Index,
                                              IF,
                                              IDF,
                                              IDFT],
        tail: IndexedConstructorSeqMap[Cod,
                                       H,
                                       TR,
                                       TI,
                                       TIntros,
                                       F,
                                       Index,
                                       IF,
                                       IDF,
                                       IDFT]
    ) = {
      val W    = tail.W
      val cons = pattern.symbcons(name, W)
      Cons(cons, pattern, tail)
    }
  }

  case class Cons[
                  C <: Term with Subs[C],
                  H <: Term with Subs[H],
                  Cod <: Term with Subs[Cod],
                  RD <: Term with Subs[RD],
                  ID <: Term with Subs[ID],
                  TR <: Term with Subs[TR],
                  TI <: Term with Subs[TI],
                  TIntros <: HList,
                  F <: Term with Subs[F],
                  Index<: HList : Subst,
                  IF <: Term with Subs[IF],
                  IDF <: Term with Subs[IDF],
                  IDFT <: Term with Subs[IDFT]](
      cons: C,
      pattern: IndexedConstructorPatternMap[
                                            Cod,
                                            C,
                                            H,
                                            RD,
                                            ID,
                                            F,
                                            Index,
                                            IF,
                                            IDF,
                                            IDFT],
      tail: IndexedConstructorSeqMap[Cod,
                                     H,
                                     TR,
                                     TI,
                                     TIntros,
                                     F,
                                     Index,
                                     IF,
                                     IDF,
                                     IDFT]
  ) extends IndexedConstructorSeqMap[Cod,
                                       H,
                                       Func[RD, TR],
                                       Func[ID, TI],
                                       C :: TIntros,
                                       F,
                                       Index,
                                       IF,
                                       IDF,
                                       IDFT] {

    val W = tail.W

    val family = tail.family

    def data(X: Typ[Cod]): RD =
      pattern.recDataTyp(W, X).symbObj(RecSym(cons))

    val defn = (d: RD) => (f: IF) => pattern.recDefCase(cons, d, f)

    def recDefn(X: Typ[Cod]) =
      IndexedRecursiveDefinition.DataCons(data(X), defn, tail.recDefn(X))

    // type RecType = Func[cons.pattern.RecDataType, tail.RecType]

    def recDataLambda(X: Typ[Cod]) =
      f => LambdaFixed(data(X), tail.recDataLambda(X)(f))

    // type InducType = Func[cons.pattern.InducDataType, tail.InducType]

    def inducData(fibre: IDFT) =
      pattern.inducDataTyp(W, fibre)(cons).symbObj(InducSym(cons))

    val inducDefn = (d: ID) => (f: IDF) => pattern.inducDefCase(cons, d, f)

    def inducDefn(fibre: IDFT) =
      IndexedInductiveDefinition.DataCons(inducData(fibre),
                                          inducDefn,
                                          tail.inducDefn(fibre))

    def inducDataLambda(fibre: IDFT) =
      (f) => LambdaFixed(inducData(fibre), tail.inducDataLambda(fibre)(f))
  }
}

abstract class IndexedConstructorSeqDom[
    H <: Term with Subs[H], F <: Term with Subs[F], Index<: HList : Subst, Intros <: HList] {
  val family: TypFamilyPtn[H, F, Index]

  val W: F

  val intros: List[Term]

  def mapped[C <: Term with Subs[C], IF <: Term with Subs[IF],
  IDF <: Term with Subs[IDF], IDFT <: Term with Subs[IDFT]](
      implicit fmlyMapper: TypFamilyMapper[H, F, C, Index, IF, IDF, IDFT])
    : IndexedConstructorSeqMap[C,
                               H,
                               RecType,
                               InducType,
                               Intros,
                               F,
                               Index,
                               IF,
                               IDF,
                               IDFT] forSome {
      type RecType <: Term with Subs[RecType];
      type InducType <: Term with Subs[InducType];
    }

  def rec[C <: Term with Subs[C]](x: Typ[C]) = {
    implicit val mp = family.mapper[C]
    val mpd         = mapped
    mpd.rec(x)
  }

  def |:[HC <: Term with Subs[HC]](head: IndexedConstructor[H, F, HC, Index]) =
    IndexedConstructorSeqDom.Cons(head.name, head.shape, this)
}

object IndexedConstructorSeqDom {
  def get[
          H <: Term with Subs[H],
          F <: Term with Subs[F],
          Index <: HList](w: F)(implicit g: TypFamilyPtnGetter[F, H, Index]) = {
    val family = g.get(w)

    implicit val gs: Subst[Index] = g.subst
    Empty(w, family)
  }

  case class Empty[H <: Term with Subs[H],
                   F <: Term with Subs[F],
                   Index<: HList : Subst](W: F, family: TypFamilyPtn[H, F, Index])
      extends IndexedConstructorSeqDom[H, F, Index, HNil] {
    def mapped[C <: Term with Subs[C],
               IF <: Term with Subs[IF],
               IDF <: Term with Subs[IDF],
               IDFT <: Term with Subs[IDFT]](
        implicit fmlyMapper: TypFamilyMapper[H, F, C, Index, IF, IDF, IDFT])
      // : IndexedConstructorSeqMap[C,
      //                            H,
      //                            RecType,
      //                            InducType,
      //                            HNil,
      //                            F,
      //                            Index,
      //                            IF,
      //                            IDF,
      //                            IDFT] forSome {
      //   type RecType <: Term with Subs[RecType];
      //   type InducType <: Term with Subs[InducType];
      // }
      =
      IndexedConstructorSeqMap.Empty[C, H, F, Index, IF, IDF, IDFT](
        W,
        fmlyMapper.mapper(family))

    val intros = List()
  }

  case class Cons[
                  H <: Term with Subs[H],
                  F <: Term with Subs[F],
                  HC <: Term with Subs[HC],
                  Index<: HList : Subst,
                  TIntro <: HList](
      name: AnySym,
      pattern: IndexedConstructorShape[H, F, HC, Index],
      tail: IndexedConstructorSeqDom[H, F, Index, TIntro])
      extends IndexedConstructorSeqDom[H, F, Index, HC :: TIntro] {
    val W = tail.W

    val family = tail.family

    def mapped[C <: Term with Subs[C],
               IF <: Term with Subs[IF],
               IDF <: Term with Subs[IDF],
               IDFT <: Term with Subs[IDFT]](
        implicit fmlyMapper: TypFamilyMapper[H, F, C, Index, IF, IDF, IDFT]
    )
    : IndexedConstructorSeqMap[C,
                                H,
                                RecType,
                                InducType,
                                HC :: TIntro,
                                F,
                                Index,
                                IF,
                                IDF,
                                IDFT] forSome {
      type RecType <: Term with Subs[RecType];
      type InducType <: Term with Subs[InducType];
    }
    =
      IndexedConstructorSeqMap.Cons.sym(name,
                                        pattern.mapped(fmlyMapper),
                                        tail.mapped(fmlyMapper))

    val intros = pattern.symbcons(name, W) :: tail.intros
  }
}
