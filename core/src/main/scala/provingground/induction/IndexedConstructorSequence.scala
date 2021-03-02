package provingground.induction

import provingground._, HoTT._


import TermList._

import shapeless._

/**
  * indexed version of [[ConstructorSeqMap]], giving definitions of  indexed recursion and induction functions.
  *
  *
  * @tparam H the scala type of terms of an inductive type that can have this constructor.
  * @tparam Cod the scala type of the codomain.
  * @tparam Fb scala type of the inductive  type family
  * @tparam Index scala type of the index
  * @tparam Intros scala type of the introduction rules
  * @tparam RecType scala type of the recursion function
  * @tparam InducType scala type of the induction function
  * @tparam IF scala type of an iterated function on the inductive type family, with codomain with terms of type `Cod`.
  * @tparam IDF scala type of an iterated  dependent function on the inductive type family, with codomain with terms of type `Cod`.
  * @tparam IDFT scala type of an iterated type family on the inductive type family, i.e.,  with codomain with terms of type `Typ[Cod]`
  *
  */
sealed abstract class IndexedConstructorSeqMap[
    C <: Term with Subs[C],
    H <: Term with Subs[H],
    RecType <: Term with Subs[RecType],
    InducType <: Term with Subs[InducType],
    Intros <: HList,
    F <: Term with Subs[F],
    Index <: HList: TermList,
    IF <: Term with Subs[IF],
    IDF <: Term with Subs[IDF],
    IDFT <: Term with Subs[IDFT]] {

  /**
    * the inductive type family
    */
  val family: TypFamilyMap[H, F, C, Index, IF, IDF, IDFT]

  def subs(x: Term, y: Term): IndexedConstructorSeqMap[C,
                                                       H,
                                                       RecType,
                                                       InducType,
                                                       Intros,
                                                       F,
                                                       Index,
                                                       IF,
                                                       IDF,
                                                       IDFT]

  /**
    * the raw recursive definition, from which the recursion function is built by lambdas
    */
  def recDefn(
      X: Typ[C]): IndexedRecursiveDefinition[H, F, C, Index, IF, IDF, IDFT]

  /**
    * the index type family
    */
  val W: F

  // type RecType <: Term with Subs[RecType]

  def recDataLambda(X: Typ[C]): IF => RecType

  /**
    * the recursion function
    */
  def rec(X: Typ[C]): RecType =
    recDataLambda(X: Typ[C])(recDefn(X: Typ[C]).iterFunc)

  /**
    * the raw inductive definition, from which the induction function is built by lambdas
    */
  def inducDefn(
      fibre: IDFT): IndexedInductiveDefinition[H, F, C, Index, IF, IDF, IDFT]

  // type InducType <: Term with Subs[InducType]

  def inducDataLambda(fibre: IDFT): IDF => InducType

  /**
    * the induction function
    */
  def induc(fibre: IDFT) =
    inducDataLambda(fibre)(inducDefn(fibre).iterDepFunc)

  /**
    * the induction function with type casting
    */
  def inducF(fibre: Term) = induc(fibre.asInstanceOf[IDFT])
}

object IndexedConstructorSeqMap {
  case class Empty[C <: Term with Subs[C],
                   H <: Term with Subs[H],
                   F <: Term with Subs[F],
                   Index <: HList: TermList,
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

    def subs(x: Term, y: Term) = Empty(W.replace(x, y), family.subs(x, y))

    // type RecType = Func[H, C]

    def recDataLambda(X: Typ[C]) = (f) => f

    def inducDefn(Xs: IDFT) =
      IndexedInductiveDefinition.Empty(W, Xs, family)

    // type InducType = FuncLike[H, C]

    def inducDataLambda(fibre: IDFT) = (f) => f

    val intros = HNil
  }

  import ConstructorSeqMap.{RecDataSym, InducDataSym}

  case class Cons[C <: Term with Subs[C],
                  H <: Term with Subs[H],
                  Cod <: Term with Subs[Cod],
                  RD <: Term with Subs[RD],
                  ID <: Term with Subs[ID],
                  TR <: Term with Subs[TR],
                  TI <: Term with Subs[TI],
                  TIntros <: HList,
                  F <: Term with Subs[F],
                  Index <: HList: TermList,
                  IF <: Term with Subs[IF],
                  IDF <: Term with Subs[IDF],
                  IDFT <: Term with Subs[IDFT]](
      cons: C,
      pattern: IndexedConstructorPatternMap[Cod,
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
                                     IDFT])
      extends IndexedConstructorSeqMap[Cod,
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

    def subs(x: Term, y: Term) =
      Cons(cons.replace(x, y), pattern.subs(x, y), tail.subs(x, y))

    def data(X: Typ[Cod]): RD =
      pattern.recDataTyp(W, X).symbObj(RecDataSym(cons))

    val defn = (d: RD) => (f: IF) => pattern.recDefCase(cons, d, f)

    def recDefn(X: Typ[Cod]) = dataCons(X)

    def dataCons(X: Typ[Cod]): IndexedRecursiveDefinition.DataCons[H,
                                                                   F,
                                                                   Cod,
                                                                   Index,
                                                                   IF,
                                                                   IDF,
                                                                   IDFT,
                                                                   RD] =
      IndexedRecursiveDefinition.DataCons[H, F, Cod, Index, IF, IDF, IDFT, RD](
        data(X),
        cons,
        defn,
        tail.recDefn(X),
        (x: Term) =>
          (y: Term) =>
            (cod: Typ[Cod]) =>
              if (W.replace(x, y) == W) None
              else Some(subs(x, y).dataCons(cod.replace(x, y))))

    // type RecType = Func[cons.pattern.RecDataType, tail.RecType]

    def recDataLambda(X: Typ[Cod]) =
      f => lmbda(data(X))(tail.recDataLambda(X)(f))

    // type InducType = Func[cons.pattern.InducDataType, tail.InducType]

    def inducData(fibre: IDFT) =
      pattern.inducDataTyp(W, fibre)(cons).symbObj(InducDataSym(cons))

    val inducDefn = (d: ID) => (f: IDF) => pattern.inducDefCase(cons, d, f)

    def indDataCons(fibre: IDFT): IndexedInductiveDefinition.DataCons[H,
                                                                      F,
                                                                      Cod,
                                                                      Index,
                                                                      IF,
                                                                      IDF,
                                                                      IDFT,
                                                                      ID] =
      IndexedInductiveDefinition.DataCons[H, F, Cod, Index, IF, IDF, IDFT, ID](
        inducData(fibre),
        cons,
        inducDefn,
        tail.inducDefn(fibre),
        (x) =>
          (y) =>
            (fib) =>
              if (W.replace(x, y) == W) None
              else Some(subs(x, y).indDataCons(fib.replace(x, y))))

    def inducDefn(fibre: IDFT) = indDataCons(fibre)

    def inducDataLambda(fibre: IDFT) =
      (f) => lmbda(inducData(fibre))(tail.inducDataLambda(fibre)(f))
  }
}

/**
  * bride between [[IndexedConstructorSeqDom]] and [[IndexedConstructorSeqMap]]
  */
sealed abstract class IndexedConstructorSeqMapper[
    SS <: HList,
    C <: Term with Subs[C],
    H <: Term with Subs[H],
    RecType <: Term with Subs[RecType],
    InducType <: Term with Subs[InducType],
    Intros <: HList,
    F <: Term with Subs[F],
    Index <: HList: TermList,
    IF <: Term with Subs[IF],
    IDF <: Term with Subs[IDF],
    IDFT <: Term with Subs[IDFT]] {
  def mapped(seqdom: IndexedConstructorSeqDom[SS, H, F, Index, Intros])(
      W: F,
      family: TypFamilyPtn[H, F, Index]): IndexedConstructorSeqMap[C,
                                                                   H,
                                                                   RecType,
                                                                   InducType,
                                                                   Intros,
                                                                   F,
                                                                   Index,
                                                                   IF,
                                                                   IDF,
                                                                   IDFT]
}

object IndexedConstructorSeqMapper {
  implicit def empty[H <: Term with Subs[H],
                     C <: Term with Subs[C],
                     F <: Term with Subs[F],
                     Index <: HList,
                     IF <: Term with Subs[IF],
                     IDF <: Term with Subs[IDF],
                     IDFT <: Term with Subs[IDFT]](
      implicit subst: TermList[Index],
      fmlyMapper: TypFamilyMapper[H, F, C, Index, IF, IDF, IDFT])
    : IndexedConstructorSeqMapper[HNil,
                                  C,
                                  H,
                                  IF,
                                  IDF,
                                  HNil,
                                  F,
                                  Index,
                                  IF,
                                  IDF,
                                  IDFT] =
    new IndexedConstructorSeqMapper[HNil,
                                    C,
                                    H,
                                    IF,
                                    IDF,
                                    HNil,
                                    F,
                                    Index,
                                    IF,
                                    IDF,
                                    IDFT] {
      def mapped(seqdom: IndexedConstructorSeqDom[HNil, H, F, Index, HNil])(
          W: F,
          family: TypFamilyPtn[H, F, Index]) =
        IndexedConstructorSeqMap.Empty(W, fmlyMapper.mapper(family))
    }
  implicit def cons[TSS <: HList,
                    HShape <: HList,
                    H <: Term with Subs[H],
                    Cod <: Term with Subs[Cod],
                    ConstructorType <: Term with Subs[ConstructorType],
                    TIntros <: HList,
                    RD <: Term with Subs[RD],
                    ID <: Term with Subs[ID],
                    TR <: Term with Subs[TR],
                    TI <: Term with Subs[TI],
                    F <: Term with Subs[F],
                    Index <: HList,
                    IF <: Term with Subs[IF],
                    IDF <: Term with Subs[IDF],
                    IDFT <: Term with Subs[IDFT]](
      implicit patternMapper: IndexedConstructorPatternMapper[HShape,
                                                              Cod,
                                                              ConstructorType,
                                                              H,
                                                              RD,
                                                              ID,
                                                              F,
                                                              Index,
                                                              IF,
                                                              IDF,
                                                              IDFT],
      tailMapper: IndexedConstructorSeqMapper[TSS,
                                              Cod,
                                              H,
                                              TR,
                                              TI,
                                              TIntros,
                                              F,
                                              Index,
                                              IF,
                                              IDF,
                                              IDFT],
      subst: TermList[Index],
      fmlyMapper: TypFamilyMapper[H, F, Cod, Index, IF, IDF, IDFT])
    : IndexedConstructorSeqMapper[HShape :: TSS,
                                  Cod,
                                  H,
                                  Func[RD, TR],
                                  Func[ID, TI],
                                  ConstructorType :: TIntros,
                                  F,
                                  Index,
                                  IF,
                                  IDF,
                                  IDFT] =
    new IndexedConstructorSeqMapper[HShape :: TSS,
                                    Cod,
                                    H,
                                    Func[RD, TR],
                                    Func[ID, TI],
                                    ConstructorType :: TIntros,
                                    F,
                                    Index,
                                    IF,
                                    IDF,
                                    IDFT] {
      def mapped(seqdom: IndexedConstructorSeqDom[HShape :: TSS,
                                                  H,
                                                  F,
                                                  Index,
                                                  ConstructorType :: TIntros])(
          W: F,
          family: TypFamilyPtn[H, F, Index]) =
        seqdom match {
          case IndexedConstructorSeqDom.Cons(name, pattern, tail) =>
            val patternMap = patternMapper.mapper(fmlyMapper)(pattern)
            val tailMap    = tailMapper.mapped(tail)(W, family)
            IndexedConstructorSeqMap.Cons(pattern.symbcons(name, W),
                                          patternMap,
                                          tailMap)
        }
    }
}

/**
  * Essentially the definition of an indexed inductive type;
  *
  *
  * @tparam SS ``shape sequence`` - a formal type for lifting information about introduction rules to type level.
  * @tparam H the scala type of terms in an inductive type of this shape
  * @tparam Intros the scala type of the introduction rules
  *
  */
sealed abstract class IndexedConstructorSeqDom[SS <: HList,
                                               H <: Term with Subs[H],
                                               F <: Term with Subs[F],
                                               Index <: HList: TermList,
                                               Intros <: HList] {

  /**
    * the index family
    */
  val family: TypFamilyPtn[H, F, Index]

  /**
    * the inductive type family
    */
  val W: F

  /**
    * the introduction rules
    */
  val intros: Intros

  /**
    * existentitally typed lift given codomain
    */
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

  /**
    * existentitally typed recursion function definition
    */
  def recE[C <: Term with Subs[C]](x: Typ[C]) = {
    implicit val mp: provingground.induction.TypFamilyMapper[H,
                                                             F,
                                                             C,
                                                             Index,
                                                             IF,
                                                             IDF,
                                                             IDFT] forSome {
      type IF <: provingground.HoTT.Term with provingground.HoTT.Subs[IF];
      type IDF <: provingground.HoTT.Term with provingground.HoTT.Subs[IDF];
      type IDFT <: provingground.HoTT.Term with provingground.HoTT.Subs[IDFT]
    }       = family.mapper[C]
    val mpd = mapped
    mpd.rec(x)
  }

  /**
    * recursion function definition based on implcits
    */
  def rec[C <: Term with Subs[C],
          IF <: Term with Subs[IF],
          IDF <: Term with Subs[IDF],
          IDFT <: Term with Subs[IDFT],
          RecType <: Term with Subs[RecType],
          InducType <: Term with Subs[InducType]](X: Typ[C])(
      implicit mapper: IndexedConstructorSeqMapper[SS,
                                                   C,
                                                   H,
                                                   RecType,
                                                   InducType,
                                                   Intros,
                                                   F,
                                                   Index,
                                                   IF,
                                                   IDF,
                                                   IDFT]) =
    mapper.mapped(this)(W, family).rec(X)

  def getInduc[C <: Term with Subs[C],
               IDFT <: Term with Subs[IDFT],
               IF <: Term with Subs[IF],
               IDF <: Term with Subs[IDF],
               RecType <: Term with Subs[RecType],
               InducType <: Term with Subs[InducType]](X: Typ[C])(
      implicit mapper: IndexedConstructorSeqMapper[SS,
                                                   C,
                                                   H,
                                                   RecType,
                                                   InducType,
                                                   Intros,
                                                   F,
                                                   Index,
                                                   IF,
                                                   IDF,
                                                   IDFT]) =
    mapper.mapped(this)(W, family).induc(_)

  def getMapper[C <: Term with Subs[C],
                IDFT <: Term with Subs[IDFT],
                IF <: Term with Subs[IF],
                IDF <: Term with Subs[IDF],
                RecType <: Term with Subs[RecType],
                InducType <: Term with Subs[InducType]](X: Typ[C])(
      implicit mapper: IndexedConstructorSeqMapper[SS,
                                                   C,
                                                   H,
                                                   RecType,
                                                   InducType,
                                                   Intros,
                                                   F,
                                                   Index,
                                                   IF,
                                                   IDF,
                                                   IDFT]) =
    mapper.mapped(this)(W, family)

  /**
    * induction function based on implicits
    */
  def induc[IDFT <: Term with Subs[IDFT],
            C <: Term with Subs[C],
            IF <: Term with Subs[IF],
            IDF <: Term with Subs[IDF],
            RecType <: Term with Subs[RecType],
            InducType <: Term with Subs[InducType]](Xs: IDFT)(
      implicit mapper: IndexedConstructorSeqMapper[SS,
                                                   C,
                                                   H,
                                                   RecType,
                                                   InducType,
                                                   Intros,
                                                   F,
                                                   Index,
                                                   IF,
                                                   IDF,
                                                   IDFT]) =
    mapper.mapped(this)(W, family).induc(Xs)

  /**
    * existentitally typed induction function
    */
  def inducE[IDFT <: Term with Subs[IDFT]](Xs: IDFT) =
    family.finalCod(Xs) match {
      case tp: Typ[u] =>
        implicit val mp: provingground.induction.TypFamilyMapper[H,
                                                                 F,
                                                                 u,
                                                                 Index,
                                                                 IF,
                                                                 IDF,
                                                                 IDFT] forSome {
          type IF <: provingground.HoTT.Term with provingground.HoTT.Subs[IF];
          type IDF <: provingground.HoTT.Term with provingground.HoTT.Subs[IDF];
          type IDFT <: provingground.HoTT.Term with provingground.HoTT.Subs[
            IDFT]
        }       = family.mapper[u]
        val mpd = mapped
        mpd.inducF(Xs)
    }

  /**
    * prepend introduction rule
    */
  def |:[HShape <: HList, HC <: Term with Subs[HC]](
      head: IndexedConstructor[HShape, H, F, HC, Index])
    : IndexedConstructorSeqDom[HShape :: SS, H, F, Index, HC :: Intros] =
    IndexedConstructorSeqDom.Cons(head.name, head.shape, this)

  def subs(x: Term, y: Term): IndexedConstructorSeqDom[SS, H, F, Index, Intros]
}

object IndexedConstructorSeqDom {
  implicit def substIndConsSeqDom[SS <: HList,
                                  H <: Term with Subs[H],
                                  F <: Term with Subs[F],
                                  Index <: HList,
                                  Intros <: HList](implicit tl: TermList[Index])
    : Subst[IndexedConstructorSeqDom[SS, H, F, Index, Intros]] =
    new Subst[IndexedConstructorSeqDom[SS, H, F, Index, Intros]] {
      def subst(a: IndexedConstructorSeqDom[SS, H, F, Index, Intros])(
          x: Term,
          y: Term) = {
        a.subs(x, y)
      }
    }

  def get[H <: Term with Subs[H], F <: Term with Subs[F], Index <: HList](w: F)(
      implicit g: TypFamilyPtnGetter[F, H, Index]) = {
    val family = g.get(w)

    implicit val gs: TermList[Index] = g.subst
    Empty(w, family)
  }

  case class Empty[H <: Term with Subs[H],
                   F <: Term with Subs[F],
                   Index <: HList: TermList](W: F,
                                             family: TypFamilyPtn[H, F, Index])
      extends IndexedConstructorSeqDom[HNil, H, F, Index, HNil] {
    def mapped[C <: Term with Subs[C],
               IF <: Term with Subs[IF],
               IDF <: Term with Subs[IDF],
               IDFT <: Term with Subs[IDFT]](
        implicit fmlyMapper: TypFamilyMapper[H, F, C, Index, IF, IDF, IDFT]) =
      IndexedConstructorSeqMap.Empty[C, H, F, Index, IF, IDF, IDFT](
        W,
        fmlyMapper.mapper(family))

    val intros = HNil

    def subs(x: Term, y: Term) = Empty(W.replace(x, y), family.subs(x, y))
  }

  object Cons {
    implicit def substIndConsSeqDomCons[TSS <: HList,
                                        HShape <: HList,
                                        H <: Term with Subs[H],
                                        F <: Term with Subs[F],
                                        HC <: Term with Subs[HC],
                                        Index <: HList: TermList,
                                        TIntro <: HList](
        implicit tl: TermList[Index]): Subst[
      IndexedConstructorSeqDom.Cons[TSS, HShape, H, F, HC, Index, TIntro]] =
      new Subst[
        IndexedConstructorSeqDom.Cons[TSS, HShape, H, F, HC, Index, TIntro]] {
        def subst(a: IndexedConstructorSeqDom.Cons[TSS,
                                                   HShape,
                                                   H,
                                                   F,
                                                   HC,
                                                   Index,
                                                   TIntro])(
            x: Term,
            y: Term
        ) = a.subs(x, y)
      }
  }

  case class Cons[TSS <: HList,
                  HShape <: HList,
                  H <: Term with Subs[H],
                  F <: Term with Subs[F],
                  HC <: Term with Subs[HC],
                  Index <: HList: TermList,
                  TIntro <: HList](
      name: AnySym,
      pattern: IndexedConstructorShape[HShape, H, F, HC, Index],
      tail: IndexedConstructorSeqDom[TSS, H, F, Index, TIntro])
      extends IndexedConstructorSeqDom[HShape :: TSS, H, F, Index, HC :: TIntro] {
    val W = tail.W

    val family = tail.family

    def mapped[C <: Term with Subs[C],
               IF <: Term with Subs[IF],
               IDF <: Term with Subs[IDF],
               IDFT <: Term with Subs[IDFT]](
        implicit fmlyMapper: TypFamilyMapper[H, F, C, Index, IF, IDF, IDFT])
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
      } =
      IndexedConstructorSeqMap.Cons(pattern.symbcons(name, W),
                                    pattern.mapped(fmlyMapper),
                                    tail.mapped(fmlyMapper))

    val intros = pattern.symbcons(name, W) :: tail.intros

    def subs(x: Term, y: Term) =
      Cons(name.subs(x, y), pattern.subs(x, y), tail.subs(x, y))
  }
}
