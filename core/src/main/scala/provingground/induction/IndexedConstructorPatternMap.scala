package provingground.induction

import provingground._, HoTT._

// import IterFuncPatternMap._

// import IndexedIterFuncPatternMap._

import TermList._

import scala.language.existentials

import shapeless._

/**
  * Introduction rule for an indexed inductive type,
  * as in [[IndexedConstructorShape]] with the ''scala'' type of the codomain specified;
  * hence the scala type of the recurion and induction types are determined.
  * The definitions of recursion and induction
  * functions for the case matching the introduction rule are the abstract methods
  * [[recDefCase]] and [[inducDefCase]].
  *
  * @tparam H the scala type of terms of an inductive type that can have this constructor.
  * @tparam Cod the scala type of the codomain.
  * @tparam Fb scala type of the inductive  type family
  * @tparam Index scala type of the index
  * @tparam ConstructorType the scala type of the introduction rule
  * @tparam RecDataType type of data for recursive definitions for the case corresponding to this introduction rule.
  * @tparam InducDataType type of data for inductive definitions for  the case corresponding to this introduction rule.
  * @tparam IF scala type of an iterated function on the inductive type family, with codomain with terms of type `Cod`.
  * @tparam IDF scala type of an iterated  dependent function on the inductive type family, with codomain with terms of type `Cod`.
  * @tparam IDFT scala type of an iterated type family on the inductive type family, i.e.,  with codomain with terms of type `Typ[Cod]`
  *
  * this is used indirectly through [[IndexedConstructorSeqMap]]
  *
  */
sealed abstract class IndexedConstructorPatternMap[
    Cod <: Term with Subs[Cod],
    ConstructorType <: Term with Subs[ConstructorType],
    H <: Term with Subs[H],
    RecDataType <: Term with Subs[RecDataType],
    InducDataType <: Term with Subs[InducDataType],
    Fb <: Term with Subs[Fb],
    Index <: HList: TermList,
    IF <: Term with Subs[IF],
    IDF <: Term with Subs[IDF],
    IDFT <: Term with Subs[IDFT]] { self =>

  /**
    * the inductive type family
    */
  val family: TypFamilyMap[H, Fb, Cod, Index, IF, IDF, IDFT]

  def subs(x: Term, y: Term): IndexedConstructorPatternMap[Cod,
                                                           ConstructorType,
                                                           H,
                                                           RecDataType,
                                                           InducDataType,
                                                           Fb,
                                                           Index,
                                                           IF,
                                                           IDF,
                                                           IDFT]

  /**
    * returns HoTT type corresponding to the pattern given the (inductive) type family W (to be the head).
    */
  def apply(tp: Fb): Typ[ConstructorType]

  // def symbcons(name: AnySym, tp: Fb): ConstructorType =
  //   apply(tp).symbObj(name)

  /**
    * domain containing the recursion data for the constructor, i.e., the HoTT type of recursion data.
    */
  def recDataTyp(wb: Fb, x: Typ[Cod]): Typ[RecDataType]

  /**
    * domain containing the induction data for the constructor, i.e., the HoTT type of the induction data.
    */
  def inducDataTyp(w: Fb, xs: IDFT)(cons: ConstructorType): Typ[InducDataType]

  /**
    * given a term, matches to see if this is the image of a given (quasi)-constructor, with `this` constructor pattern.
    * optionally returns simplification (if the term matches), determined by the recursion data.
    * @param cons constructor, actually quasi-constructor, with which to match.
    * @param data definition data for the image of the constructor.
    * @param f the function being defined recursively, to be used recursively in definition.
    */
  def recDefCase(cons: ConstructorType,
                 data: RecDataType,
                 f: => IF): H => Option[Cod]

  /**
    * given a term, matches to see if this is the image of a given (quasi)-constructor, with `this` constructor pattern.
    * optionally returns simplification (if the term matches).
    * @param cons constructor, actually quasi-constructor, with which to match.
    * @param data definition data for the image of the constructor.
    * @param f the function being defined inductively, to be used recursively in definition.
    */
  def inducDefCase(cons: ConstructorType,
                   data: InducDataType,
                   f: => IDF): H => Option[Cod]
  val univLevel: Int
}

object IndexedConstructorPatternMap {
  case class IndexedIdMap[C <: Term with Subs[C],
                          H <: Term with Subs[H],
                          Fb <: Term with Subs[Fb],
                          Index <: HList: TermList,
                          IF <: Term with Subs[IF],
                          IDF <: Term with Subs[IDF],
                          IDFT <: Term with Subs[IDFT]](
      family: TypFamilyMap[H, Fb, C, Index, IF, IDF, IDFT],
      index: Index)
      extends IndexedConstructorPatternMap[C,
                                           H,
                                           H,
                                           C,
                                           C,
                                           Fb,
                                           Index,
                                           IF,
                                           IDF,
                                           IDFT] {

    def apply(W: Fb) = family.pattern.typ(W, index)

    val univLevel = 0

    def recDataTyp(w: Fb, x: Typ[C]) = x

    def inducDataTyp(w: Fb, xs: IDFT)(cons: H) =
      family.typRestrict(xs, index)(cons)

    def subs(x: Term, y: Term) =
      IndexedIdMap(family.subs(x, y), index.subst(x, y))

    def recDefCase(cons: H, data: C, f: => IF): H => Option[C] = {
      case (t: Term) if t == cons => Some(data)
      case _                      => None
    }

    def inducDefCase(cons: H, data: C, f: => IDF): Term => Option[C] = {
      case (t: Term) if t == cons => Some(data)
      case _                      => None
    }
  }

  sealed abstract class IndexedRecursiveConstructorPatternMap[
      Cod <: Term with Subs[Cod],
      ArgType <: Term with Subs[ArgType],
      HeadConstructorType <: Term with Subs[HeadConstructorType],
      CT <: FuncLike[ArgType, HeadConstructorType] with Subs[CT],
      H <: Term with Subs[H],
      RecDataType <: Term with Subs[RecDataType],
      InducDataType <: Term with Subs[InducDataType],
      HeadRecDataType <: Term with Subs[HeadRecDataType],
      HeadInducDataType <: Term with Subs[HeadInducDataType],
      Fb <: Term with Subs[Fb],
      Index <: HList: TermList,
      IF <: Term with Subs[IF],
      IDF <: Term with Subs[IDF],
      IDFT <: Term with Subs[IDFT]]
      extends IndexedConstructorPatternMap[Cod,
                                           CT,
                                           H,
                                           RecDataType,
                                           InducDataType,
                                           Fb,
                                           Index,
                                           IF,
                                           IDF,
                                           IDFT] { self =>

    /**
      * The head pattern, constant T for A -> T and T(a) for A ~> T(a)
      */
    val headfibre: ArgType => IndexedConstructorPatternMap[Cod,
                                                           HeadConstructorType,
                                                           H,
                                                           HeadRecDataType,
                                                           HeadInducDataType,
                                                           Fb,
                                                           Index,
                                                           IF,
                                                           IDF,
                                                           IDFT]

    /**
      * returns data for recursion to be passed on to the head given an argument (when matching with the constructor).
      */
    def headData(data: RecDataType, arg: ArgType, f: => IF): HeadRecDataType

    def recDefCase(cons: CT, data: RecDataType, f: => IF): H => Option[Cod] = {
      t =>
        for (arg <- getArg(cons)(t);
             term <- headfibre(arg).recDefCase(cons(arg),
                                               headData(data, arg, f),
                                               f)(t)) yield term
    }

    def headInducData(data: InducDataType,
                      arg: ArgType,
                      f: => IDF): HeadInducDataType

    def inducDefCase(cons: CT,
                     data: InducDataType,
                     f: => IDF): H => Option[Cod] = { t =>
      for (arg <- getArg(cons)(t);
           term <- headfibre(arg).inducDefCase(cons(arg),
                                               headInducData(data, arg, f),
                                               f)(t)) yield term
    }
  }

  case class IndexedFuncPtnMap[C <: Term with Subs[C],
                               F <: Term with Subs[F],
                               HC <: Term with Subs[HC],
                               H <: Term with Subs[H],
                               HR <: Term with Subs[HR],
                               HI <: Term with Subs[HI],
                               TT <: Term with Subs[TT],
                               DT <: Term with Subs[DT],
                               Fb <: Term with Subs[Fb],
                               Index <: HList: TermList,
                               IF <: Term with Subs[IF],
                               IDF <: Term with Subs[IDF],
                               IDFT <: Term with Subs[IDFT]](
      tail: IterFuncPtnMap[H, C, F, TT, DT],
      head: IndexedConstructorPatternMap[C,
                                         HC,
                                         H,
                                         HR,
                                         HI,
                                         Fb,
                                         Index,
                                         IF,
                                         IDF,
                                         IDFT],
      ind: Index)
      extends IndexedRecursiveConstructorPatternMap[C,
                                                    F,
                                                    HC,
                                                    Func[F, HC],
                                                    H,
                                                    Func[F, Func[TT, HR]],
                                                    FuncLike[F, Func[DT, HI]],
                                                    HR,
                                                    HI,
                                                    Fb,
                                                    Index,
                                                    IF,
                                                    IDF,
                                                    IDFT] { self =>

    def subs(x: Term, y: Term) =
      IndexedFuncPtnMap(tail.subs(x, y), head.subs(x, y), ind.subst(x, y))

    val family = head.family

    val headfibre = (t: F) => head

    //    type ConstructorType = Func[ArgType, head.ConstructorType]

    def recDataTyp(w: Fb, x: Typ[C]) =
      tail(family.pattern.typ(w, ind)) ->: tail.target(x) ->: head.recDataTyp(w,
                                                                              x)

    def inducDataTyp(w: Fb, xs: IDFT)(
        cons: Func[F, HC]): Typ[FuncLike[F, Func[DT, HI]]] = {
      val a        = tail(family.pattern.typ(w, ind)).Var //; println(s"247: $a")
      val headcons = cons(a)
      val xss      = family.typRestrict(xs, ind)
      // val fibre =
      //   lmbda(a)(tail.depTarget(xss)(a) ->: head.inducDataTyp(w, xs)(headcons))
      piDefn(a)(tail.depTarget(xss)(a) ->: head.inducDataTyp(w, xs)(headcons))
    }

    def headData(data: Func[F, Func[TT, HR]], arg: F, f: => IF): HR = {
      data(arg)(tail.induced(family.restrict(f, ind))(arg))
    }

    def headInducData(data: FuncLike[F, Func[DT, HI]],
                      arg: F,
                      f: => IDF): HI = {
      val induced = tail.inducedDep(family.depRestrict(f, ind))
      data(arg)(tail.inducedDep(family.depRestrict(f, ind))(arg))
    }

    def apply(W: Fb) =
      tail(family.pattern.typ(W, ind)) ->: head(W)

    val univLevel = math.max(head.univLevel, tail.univLevel)
  }

  case class IndexedIndexedFuncPtnMap[C <: Term with Subs[C],
                                      F <: Term with Subs[F],
                                      HC <: Term with Subs[HC],
                                      H <: Term with Subs[H],
                                      HR <: Term with Subs[HR],
                                      HI <: Term with Subs[HI],
                                      TT <: Term with Subs[TT],
                                      DT <: Term with Subs[DT],
                                      Fb <: Term with Subs[Fb],
                                      Index <: HList: TermList,
                                      IF <: Term with Subs[IF],
                                      IDF <: Term with Subs[IDF],
                                      IDFT <: Term with Subs[IDFT]](
      tail: IndexedIterFuncPtnMap[H, Fb, Index, C, F, TT, DT, IF, IDF, IDFT],
      head: IndexedConstructorPatternMap[C,
                                         HC,
                                         H,
                                         HR,
                                         HI,
                                         Fb,
                                         Index,
                                         IF,
                                         IDF,
                                         IDFT],
      ind: Index)
      extends IndexedRecursiveConstructorPatternMap[C,
                                                    F,
                                                    HC,
                                                    Func[F, HC],
                                                    H,
                                                    Func[F, Func[TT, HR]],
                                                    FuncLike[F, Func[DT, HI]],
                                                    HR,
                                                    HI,
                                                    Fb,
                                                    Index,
                                                    IF,
                                                    IDF,
                                                    IDFT] { self =>

    def subs(x: Term, y: Term) =
      IndexedIndexedFuncPtnMap(tail.subs(x, y),
                               head.subs(x, y),
                               ind.subst(x, y))

    val family = head.family

    val headfibre = (t: F) => head

    //    type ConstructorType = Func[ArgType, head.ConstructorType]

    def recDataTyp(w: Fb, x: Typ[C]) =
      tail(w) ->: tail.target(x) ->: head.recDataTyp(w, x)

    def inducDataTyp(w: Fb, xs: IDFT)(
        cons: Func[F, HC]): Typ[FuncLike[F, Func[DT, HI]]] = {
      val a        = tail(w).Var //; println(s"336: $a")
      val headcons = cons(a)
      // val xss = family.typRestrict(xs, ind)
      // val fibre =
      //   lmbda(a)(tail.depTarget(xs)(a) ->: head.inducDataTyp(w, xs)(headcons))
      piDefn(a)(tail.depTarget(xs)(a) ->: head.inducDataTyp(w, xs)(headcons))
    }

    def headData(data: Func[F, Func[TT, HR]], arg: F, f: => IF): HR = {
      data(arg)(tail.induced(f)(arg))
    }

    def headInducData(data: FuncLike[F, Func[DT, HI]],
                      arg: F,
                      f: => IDF): HI = {
      data(arg)(tail.inducedDep(f)(arg))
    }

    def apply(W: Fb) =
      tail(W) ->: head(W)

    val univLevel = math.max(head.univLevel, tail.univLevel)
  }

  case class IndexedCnstFncPtnMap[T <: Term with Subs[T],
                                  Cod <: Term with Subs[Cod],
                                  HC <: Term with Subs[HC],
                                  H <: Term with Subs[H],
                                  HR <: Term with Subs[HR],
                                  HI <: Term with Subs[HI],
                                  Fb <: Term with Subs[Fb],
                                  Index <: HList: TermList,
                                  IF <: Term with Subs[IF],
                                  IDF <: Term with Subs[IDF],
                                  IDFT <: Term with Subs[IDFT]](
      tail: Typ[T],
      head: IndexedConstructorPatternMap[Cod,
                                         HC,
                                         H,
                                         HR,
                                         HI,
                                         Fb,
                                         Index,
                                         IF,
                                         IDF,
                                         IDFT])
      extends IndexedRecursiveConstructorPatternMap[Cod,
                                                    T,
                                                    HC,
                                                    Func[T, HC],
                                                    H,
                                                    Func[T, HR],
                                                    FuncLike[T, HI],
                                                    HR,
                                                    HI,
                                                    Fb,
                                                    Index,
                                                    IF,
                                                    IDF,
                                                    IDFT] { self =>

    val family = head.family

    def subs(x: Term, y: Term) =
      IndexedCnstFncPtnMap(tail.subs(x, y), head.subs(x, y))

    val headfibre = (t: T) => head

    def recDataTyp(w: Fb, x: Typ[Cod]) = tail ->: head.recDataTyp(w, x)

    def inducDataTyp(w: Fb, xs: IDFT)(cons: Func[T, HC]) = {
      val a        = tail.Var; //; println(s"406: $a")
      val headcons = cons(a)
      val fibre    = lmbda(a)(head.inducDataTyp(w, xs)(headcons))
      piDefn(a)(head.inducDataTyp(w, xs)(headcons))
    }

    //   type ConstructorType = Func[Term, head.ConstructorType]

    def headData(data: Func[T, HR], arg: T, f: => IF): HR =
      data(arg)

    def headInducData(data: FuncLike[T, HI], arg: T, f: => IDF): HI = data(arg)

    def apply(W: Fb) = FuncTyp(tail, head(W))

    val univLevel = head.univLevel
  }

  case class IndexedCnstDepFncPtnMap[T <: Term with Subs[T],
                                     Cod <: Term with Subs[Cod],
                                     HC <: Term with Subs[HC],
                                     H <: Term with Subs[H],
                                     HR <: Term with Subs[HR],
                                     HI <: Term with Subs[HI],
                                     Fb <: Term with Subs[Fb],
                                     Index <: HList: TermList,
                                     IF <: Term with Subs[IF],
                                     IDF <: Term with Subs[IDF],
                                     IDFT <: Term with Subs[IDFT]](
      tail: Typ[T],
      headfibre: T => IndexedConstructorPatternMap[Cod,
                                                   HC,
                                                   H,
                                                   HR,
                                                   HI,
                                                   Fb,
                                                   Index,
                                                   IF,
                                                   IDF,
                                                   IDFT])
      extends IndexedRecursiveConstructorPatternMap[Cod,
                                                    T,
                                                    HC,
                                                    FuncLike[T, HC],
                                                    H,
                                                    FuncLike[T, HR],
                                                    FuncLike[T, HI],
                                                    HR,
                                                    HI,
                                                    Fb,
                                                    Index,
                                                    IF,
                                                    IDF,
                                                    IDFT] { self =>

    val family = headfibre(tail.Var).family

    def subs(x: Term, y: Term) =
      IndexedCnstDepFncPtnMap(tail.subs(x, y),
                              (t: T) => headfibre(t).subs(x, y))

    def recDataTyp(w: Fb, x: Typ[Cod]) = {
      val a     = tail.Var
      val fibre = lmbda(a)(headfibre(a).recDataTyp(w, x))
      piDefn(a)(headfibre(a).recDataTyp(w, x))
    }

    def inducDataTyp(w: Fb, xs: IDFT)(cons: FuncLike[T, HC]) = {
      val a        = tail.Var //; println(s"475: $a")
      val headcons = cons(a)
      val fibre    = lmbda(a)(headfibre(a).inducDataTyp(w, xs)(headcons))
      piDefn(a)(headfibre(a).inducDataTyp(w, xs)(headcons))
    }

    //   type ConstructorType = Func[Term, head.ConstructorType]

    def headData(data: FuncLike[T, HR], arg: T, f: => IF): HR =
      data(arg)

    def headInducData(data: FuncLike[T, HI], arg: T, f: => IDF): HI = data(arg)

    def apply(W: Fb) = {
      val a = tail.Var
      // val fiber = lmbda(a)(headfibre(a)(W))
      piDefn(a)(headfibre(a)(W))
    }

    val univLevel = headfibre(tail.Var).univLevel
  }
}

/**
  * The introduction rule for an indexed inductive type;
  * typically (A -> B -> W(x))-> C -> W(y) -> (D -> W(y)) -> W(z) as a function of W
  * May have Pi-types instead of function types.
  *
  * Usually constructed using the DSL in [[TLImplicits]]
  *
  * @tparam H scala type of the terms of the inductive type.
  * @tparam Fb scala type of the inudctive type family
  * @tparam ConstructorType  scala type of a constructor (introduction rule) corresponding to this pattern.
  * @tparam S formal type to capture information at type level
  * @tparam Index scala type of the  index
  *
  */
sealed abstract class IndexedConstructorShape[S <: HList,
                                              H <: Term with Subs[H],
                                              Fb <: Term with Subs[Fb],
                                              ConstructorType <: Term with Subs[
                                                ConstructorType],
                                              Index <: HList: TermList] {
  val family: TypFamilyPtn[H, Fb, Index]

  /**
    * returns HoTT type corresponding to the pattern given the (inductive) type family W (to be the head).
    */
  def apply(tp: Fb): Typ[ConstructorType]

  /**
    * returns term giving introduction rule given inductive type and name
    */
  def symbcons(name: AnySym, tp: Fb): ConstructorType =
    apply(tp).symbObj(name)

  def subs(x: Term,
           y: Term): IndexedConstructorShape[S, H, Fb, ConstructorType, Index]

  /**
    * helper to give [[ConstructorPatternMap]] when scala type of codomain is specified.
    */
  def mapper[C <: Term with Subs[C], IF <: Term with Subs[IF],
  IDF <: Term with Subs[IDF], IDFT <: Term with Subs[IDFT]](
      implicit fmlyMapper: TypFamilyMapper[H, Fb, C, Index, IF, IDF, IDFT])
    : IndexedConstructorPatternMapper[S,
                                      C,
                                      ConstructorType,
                                      H,
                                      RecDataType,
                                      InducDataType,
                                      Fb,
                                      Index,
                                      IF,
                                      IDF,
                                      IDFT] forSome {
      type RecDataType <: Term with Subs[RecDataType];
      type InducDataType <: Term with Subs[InducDataType];
    }

  /**
    * returns a variable to act as an introduction rule.
    */
  def :::(name: AnySym) = IndexedConstructor(name, this)

  /**
    * lift to [[IndexedConstructorPatternMap]] using the result of the [[mapper]] method.
    */
  def mapped[C <: Term with Subs[C],
             IF <: Term with Subs[IF],
             IDF <: Term with Subs[IDF],
             IDFT <: Term with Subs[IDFT]](
      implicit fmlyMapper: TypFamilyMapper[H, Fb, C, Index, IF, IDF, IDFT]) =
    mapper(fmlyMapper).mapper(fmlyMapper)(this)

  // def symbcons(name: AnySym, tp: Fb) = {
  //   implicit val mpr = family.mapper[Term]
  //   val mp           = mapped
  //   mp.symbcons(name, tp)
  // }

  import IndexedConstructorShape._

  // def -->>:[F <: Term with Subs[F]](that: IterFuncShape[H, F], ind: Index) = // FIXME - should use the indexed versions
  //   IndexedFuncConsShape(that, this, ind)
  //

  /**
    * returns shape `that -> this' where `that` is of the form `W(z)`, `A -> W(z)` etc;
    * invoking this is an error if we `that` is independent of `W`
    */
  def -->>:[F <: Term with Subs[F]](that: IndexedIterFuncShape[H, F, Fb, Index],
                                    ind: Index) =
    IndexedIndexedFuncConsShape(that, this, ind)

  // def -->>:(that: IndexedIdShape[H, Fb, Index]) = {
  //   IndexedFuncConsShape(IdIterShape[H], this, that.index)
  // }

  /**
    * returns shape `tail ->: this` where tail must be independent of the inductive type `W` being defined.
    */
  def ->>:[T <: Term with Subs[T]](tail: Typ[T]) =
    IndexedCnstFuncConsShape(tail, this)

  /**
    * returns dependent shape `tail ~>: this` where tail must be independent of the inductive type `W` being defined.
    */
  def ~>>:[T <: Term with Subs[T]](tailVar: T) = {
    import SubstInstances._
    val fib = Subst.Lambda(tailVar, this)
    // val fibre = (t: T) => this.subs(tailVar, t)
    IndexedCnstDepFuncConsShape(tailVar.typ.asInstanceOf[Typ[T]], fib)
  }
}

object IndexedConstructorShape {
  def get[H <: Term with Subs[H],
          F <: Term with Subs[F],
          Index <: HList: TermList](w: F, typ: Typ[H])(
      implicit g: TypFamilyPtnGetter[F, H, Index]) = {
    val family = g.get(w)
    val index  = family.getIndex(w, typ).get

    // implicit val ts: TermList[Index] = g.subst

    IndexedIdShape(family, index)
  }

  import IndexedConstructorPatternMapper._

  case class IndexedIdShape[H <: Term with Subs[H],
                            F <: Term with Subs[F],
                            Index <: HList: TermList](
      family: TypFamilyPtn[H, F, Index],
      index: Index)
      extends IndexedConstructorShape[HNil, H, F, H, Index] {
    def apply(W: F) = family.typ(W, index)

    def subs(x: Term, y: Term) =
      IndexedIdShape(family.subs(x, y), index.subst(x, y))

    def mapper[C <: Term with Subs[C],
               IF <: Term with Subs[IF],
               IDF <: Term with Subs[IDF],
               IDFT <: Term with Subs[IDFT]](
        implicit fmlyMapper: TypFamilyMapper[H, F, C, Index, IF, IDF, IDFT])
      : IndexedConstructorPatternMapper[HNil,
                                        C,
                                        H,
                                        H,
                                        RecDataType,
                                        InducDataType,
                                        F,
                                        Index,
                                        IF,
                                        IDF,
                                        IDFT] forSome {
        type RecDataType <: Term with Subs[RecDataType];
        type InducDataType <: Term with Subs[InducDataType];
      } = fmlyMapper match {
      case tfmp: TypFamilyMapper[H, F, C, Index, u, v, w] =>
        indexedIdMapper(implicitly[TermList[Index]], tfmp)
    }
  }

  object IndexedFuncConsShape

  @deprecated("wrong", "don't use")
  case class IndexedFuncConsShape[HShape <: HList,
                                  H <: Term with Subs[H],
                                  F <: Term with Subs[F],
                                  FI <: Term with Subs[FI],
                                  HC <: Term with Subs[HC],
                                  Index <: HList: TermList](
      tail: IterFuncShape[H, FI],
      head: IndexedConstructorShape[HShape, H, F, HC, Index],
      ind: Index)
      extends IndexedConstructorShape[IndexedFuncConsShape.type :: HShape,
                                      H,
                                      F,
                                      Func[FI, HC],
                                      Index] {

    def apply(W: F) =
      tail(family.typ(W, ind)) ->: head(W)

    val family = head.family

    def mapper[C <: Term with Subs[C],
               IF <: Term with Subs[IF],
               IDF <: Term with Subs[IDF],
               IDFT <: Term with Subs[IDFT]](
        implicit fmlyMapper: TypFamilyMapper[H, F, C, Index, IF, IDF, IDFT])
      : IndexedConstructorPatternMapper[IndexedFuncConsShape.type :: HShape,
                                        C,
                                        Func[FI, HC],
                                        H,
                                        RecDataType,
                                        InducDataType,
                                        F,
                                        Index,
                                        IF,
                                        IDF,
                                        IDFT] forSome {
        type RecDataType <: Term with Subs[RecDataType];
        type InducDataType <: Term with Subs[InducDataType];
      } =
      indexedFuncPtnMap(implicitly[TermList[Index]],
                        tail.mapper[C],
                        head.mapper(fmlyMapper))

    def subs(x: Term, y: Term) =
      IndexedFuncConsShape(tail.subs(x, y), head.subs(x, y), ind.subst(x, y))
  }

  object IndexedIndexedFuncConsShape

  case class IndexedIndexedFuncConsShape[HShape <: HList,
                                         H <: Term with Subs[H],
                                         F <: Term with Subs[F],
                                         HC <: Term with Subs[HC],
                                         Fb <: Term with Subs[Fb],
                                         Index <: HList: TermList](
      tail: IndexedIterFuncShape[H, F, Fb, Index],
      head: IndexedConstructorShape[HShape, H, Fb, HC, Index],
      ind: Index)
      extends IndexedConstructorShape[
        IndexedIndexedFuncConsShape.type :: HShape,
        H,
        Fb,
        Func[F, HC],
        Index] {

    def apply(W: Fb) =
      tail(W) ->: head(W)

    val family = head.family

    def mapper[C <: Term with Subs[C],
               IF <: Term with Subs[IF],
               IDF <: Term with Subs[IDF],
               IDFT <: Term with Subs[IDFT]](
        implicit fmlyMapper: TypFamilyMapper[H, Fb, C, Index, IF, IDF, IDFT])
      : IndexedConstructorPatternMapper[
        IndexedIndexedFuncConsShape.type :: HShape,
        C,
        Func[F, HC],
        H,
        RecDataType,
        InducDataType,
        Fb,
        Index,
        IF,
        IDF,
        IDFT] forSome {
        type RecDataType <: Term with Subs[RecDataType];
        type InducDataType <: Term with Subs[InducDataType];
      } =
      indexedIndexedFuncPtnMap(implicitly[TermList[Index]],
                               tail.mapper(fmlyMapper),
                               head.mapper(fmlyMapper))

    def subs(x: Term, y: Term) =
      IndexedIndexedFuncConsShape(tail.subs(x, y),
                                  head.subs(x, y),
                                  ind.subst(x, y))
  }

  object IndexedCnstFuncConsShape

  case class IndexedCnstFuncConsShape[HShape <: HList,
                                      T <: Term with Subs[T],
                                      H <: Term with Subs[H],
                                      F <: Term with Subs[F],
                                      HC <: Term with Subs[HC],
                                      Index <: HList: TermList](
      tail: Typ[T],
      head: IndexedConstructorShape[HShape, H, F, HC, Index])
      extends IndexedConstructorShape[IndexedCnstFuncConsShape.type :: HShape,
                                      H,
                                      F,
                                      Func[T, HC],
                                      Index] {

    def apply(W: F) =
      tail ->: head(W)

    val family = head.family

    def mapper[C <: Term with Subs[C],
               IF <: Term with Subs[IF],
               IDF <: Term with Subs[IDF],
               IDFT <: Term with Subs[IDFT]](
        implicit fmlyMapper: TypFamilyMapper[H, F, C, Index, IF, IDF, IDFT])
      : IndexedConstructorPatternMapper[IndexedCnstFuncConsShape.type :: HShape,
                                        C,
                                        Func[T, HC],
                                        H,
                                        RecDataType,
                                        InducDataType,
                                        F,
                                        Index,
                                        IF,
                                        IDF,
                                        IDFT] forSome {
        type RecDataType <: Term with Subs[RecDataType];
        type InducDataType <: Term with Subs[InducDataType];
      } =
      indexedCnstFncPtnMapper(implicitly[TermList[Index]],
                              head.mapper(fmlyMapper))

    def subs(x: Term, y: Term) =
      IndexedCnstFuncConsShape(tail.replace(x, y), head.subs(x, y))
  }

  object IndexedCnstDepFuncConsShape

  case class IndexedCnstDepFuncConsShape[HShape <: HList,
                                         T <: Term with Subs[T],
                                         H <: Term with Subs[H],
                                         F <: Term with Subs[F],
                                         HC <: Term with Subs[HC],
                                         Index <: HList: TermList](
      tail: Typ[T],
      headfibre: T => IndexedConstructorShape[HShape, H, F, HC, Index])
      extends IndexedConstructorShape[
        IndexedCnstDepFuncConsShape.type :: HShape,
        H,
        F,
        FuncLike[T, HC],
        Index] {
    def apply(W: F) = {
      val a = tail.Var
      piDefn(a)(headfibre(a)(W))
    }

    val family = headfibre(tail.Var).family

    def mapper[C <: Term with Subs[C],
               IF <: Term with Subs[IF],
               IDF <: Term with Subs[IDF],
               IDFT <: Term with Subs[IDFT]](
        implicit fmlyMapper: TypFamilyMapper[H, F, C, Index, IF, IDF, IDFT]) =
      indexedCnstDepFncPtnMapper(implicitly[TermList[Index]],
                                 headfibre(tail.Var).mapper(fmlyMapper))

    def subs(x: Term, y: Term) =
      IndexedCnstDepFuncConsShape(tail.replace(x, y),
                                  (t: T) => headfibre(t).subs(x, y))
  }
}

/**
  * bridge between [[IndexedConstructorShape]] and [[IndexedConstructorPatternMap]]
  */
sealed abstract class IndexedConstructorPatternMapper[
    S <: HList,
    Cod <: Term with Subs[Cod],
    ConstructorType <: Term with Subs[ConstructorType],
    H <: Term with Subs[H],
    RecDataType <: Term with Subs[RecDataType],
    InducDataType <: Term with Subs[InducDataType],
    F <: Term with Subs[F],
    Index <: HList: TermList,
    IF <: Term with Subs[IF],
    IDF <: Term with Subs[IDF],
    IDFT <: Term with Subs[IDFT]] {
  def mapper(
      implicit fmlyMapper: TypFamilyMapper[H, F, Cod, Index, IF, IDF, IDFT])
    : IndexedConstructorShape[S, H, F, ConstructorType, Index] => IndexedConstructorPatternMap[
      Cod,
      ConstructorType,
      H,
      RecDataType,
      InducDataType,
      F,
      Index,
      IF,
      IDF,
      IDFT]
}

object IndexedConstructorPatternMapper {
  import IndexedConstructorShape._
  import IndexedConstructorPatternMap._

  implicit def indexedIdMapper[C <: Term with Subs[C],
                               H <: Term with Subs[H],
                               F <: Term with Subs[F],
                               Index <: HList: TermList,
                               IF <: Term with Subs[IF],
                               IDF <: Term with Subs[IDF],
                               IDFT <: Term with Subs[IDFT]](
      implicit family: TypFamilyMapper[H, F, C, Index, IF, IDF, IDFT])
    : _root_.provingground.induction.IndexedConstructorPatternMapper[
      _root_.shapeless.HNil,
      C,
      H,
      H,
      C,
      C,
      F,
      Index,
      IF,
      IDF,
      IDFT] =
    new IndexedConstructorPatternMapper[HNil,
                                        C,
                                        H,
                                        H,
                                        C,
                                        C,
                                        F,
                                        Index,
                                        IF,
                                        IDF,
                                        IDFT] {
      def mapper(implicit fmlyMapper: TypFamilyMapper[H,
                                                      F,
                                                      C,
                                                      Index,
                                                      IF,
                                                      IDF,
                                                      IDFT]) = {
        case IndexedIdShape(fam, ind) =>
          IndexedIdMap(fmlyMapper.mapper(fam), ind)
      }
    }

  implicit def indexedFuncPtnMap[HShape <: HList,
                                 C <: Term with Subs[C],
                                 F <: Term with Subs[F],
                                 FI <: Term with Subs[FI],
                                 HC <: Term with Subs[HC],
                                 H <: Term with Subs[H],
                                 HR <: Term with Subs[HR],
                                 HI <: Term with Subs[HI],
                                 TT <: Term with Subs[TT],
                                 DT <: Term with Subs[DT],
                                 Fb <: Term with Subs[Fb],
                                 Index <: HList: TermList,
                                 IF <: Term with Subs[IF],
                                 IDF <: Term with Subs[IDF],
                                 IDFT <: Term with Subs[IDFT]](
      implicit tail: IterFuncMapper[H, C, FI, TT, DT],
      head: IndexedConstructorPatternMapper[
        HShape,
        C,
        HC,
        H,
        HR,
        HI,
        Fb,
        Index,
        IF,
        IDF,
        IDFT]): _root_.provingground.induction.IndexedConstructorPatternMapper[
    _root_.shapeless.::[
      _root_.provingground.induction.IndexedConstructorShape.IndexedFuncConsShape.type,
      HShape],
    C,
    _root_.provingground.HoTT.Func[FI, HC],
    H,
    _root_.provingground.HoTT.Func[FI, _root_.provingground.HoTT.Func[TT, HR]],
    _root_.provingground.HoTT.FuncLike[FI,
                                       _root_.provingground.HoTT.Func[DT, HI]],
    Fb,
    Index,
    IF,
    IDF,
    IDFT] =
    new IndexedConstructorPatternMapper[IndexedFuncConsShape.type :: HShape,
                                        C,
                                        Func[FI, HC],
                                        H,
                                        Func[FI, Func[TT, HR]],
                                        FuncLike[FI, Func[DT, HI]],
                                        Fb,
                                        Index,
                                        IF,
                                        IDF,
                                        IDFT] {
      def mapper(
          implicit fmlyMapper: TypFamilyMapper[H,
                                               Fb,
                                               C,
                                               Index,
                                               IF,
                                               IDF,
                                               IDFT]) = {
        case IndexedFuncConsShape(t, h, ind) =>
          IndexedFuncPtnMap(tail.mapper(t), head.mapper(fmlyMapper)(h), ind)
      }
    }

  implicit def indexedIndexedFuncPtnMap[HShape <: HList,
                                        C <: Term with Subs[C],
                                        F <: Term with Subs[F],
                                        HC <: Term with Subs[HC],
                                        H <: Term with Subs[H],
                                        HR <: Term with Subs[HR],
                                        HI <: Term with Subs[HI],
                                        TT <: Term with Subs[TT],
                                        DT <: Term with Subs[DT],
                                        Fb <: Term with Subs[Fb],
                                        Index <: HList: TermList,
                                        IF <: Term with Subs[IF],
                                        IDF <: Term with Subs[IDF],
                                        IDFT <: Term with Subs[IDFT]](
      implicit tail: IndexedIterFuncPtnMapper[H,
                                              Fb,
                                              Index,
                                              C,
                                              F,
                                              TT,
                                              DT,
                                              IF,
                                              IDF,
                                              IDFT],
      head: IndexedConstructorPatternMapper[
        HShape,
        C,
        HC,
        H,
        HR,
        HI,
        Fb,
        Index,
        IF,
        IDF,
        IDFT]): _root_.provingground.induction.IndexedConstructorPatternMapper[
    _root_.shapeless.::[
      _root_.provingground.induction.IndexedConstructorShape.IndexedIndexedFuncConsShape.type,
      HShape],
    C,
    _root_.provingground.HoTT.Func[F, HC],
    H,
    _root_.provingground.HoTT.Func[F, _root_.provingground.HoTT.Func[TT, HR]],
    _root_.provingground.HoTT.FuncLike[F,
                                       _root_.provingground.HoTT.Func[DT, HI]],
    Fb,
    Index,
    IF,
    IDF,
    IDFT] =
    new IndexedConstructorPatternMapper[
      IndexedIndexedFuncConsShape.type :: HShape,
      C,
      Func[F, HC],
      H,
      Func[F, Func[TT, HR]],
      FuncLike[F, Func[DT, HI]],
      Fb,
      Index,
      IF,
      IDF,
      IDFT] {
      def mapper(
          implicit fmlyMapper: TypFamilyMapper[H,
                                               Fb,
                                               C,
                                               Index,
                                               IF,
                                               IDF,
                                               IDFT]) = {
        case IndexedIndexedFuncConsShape(t, h, ind) =>
          IndexedIndexedFuncPtnMap(tail.mapper(fmlyMapper)(t),
                                   head.mapper(fmlyMapper)(h),
                                   ind)
      }
    }

  implicit def indexedCnstFncPtnMapper[HShape <: HList,
                                       T <: Term with Subs[T],
                                       Cod <: Term with Subs[Cod],
                                       HC <: Term with Subs[HC],
                                       H <: Term with Subs[H],
                                       HR <: Term with Subs[HR],
                                       HI <: Term with Subs[HI],
                                       Fb <: Term with Subs[Fb],
                                       Index <: HList: TermList,
                                       IF <: Term with Subs[IF],
                                       IDF <: Term with Subs[IDF],
                                       IDFT <: Term with Subs[IDFT]](
      implicit head: IndexedConstructorPatternMapper[HShape,
                                                     Cod,
                                                     HC,
                                                     H,
                                                     HR,
                                                     HI,
                                                     Fb,
                                                     Index,
                                                     IF,
                                                     IDF,
                                                     IDFT])
    : _root_.provingground.induction.IndexedConstructorPatternMapper[
      _root_.shapeless.::[
        _root_.provingground.induction.IndexedConstructorShape.IndexedCnstFuncConsShape.type,
        HShape],
      Cod,
      _root_.provingground.HoTT.Func[T, HC],
      H,
      _root_.provingground.HoTT.Func[T, HR],
      _root_.provingground.HoTT.FuncLike[T, HI],
      Fb,
      Index,
      IF,
      IDF,
      IDFT] =
    new IndexedConstructorPatternMapper[IndexedCnstFuncConsShape.type :: HShape,
                                        Cod,
                                        Func[T, HC],
                                        H,
                                        Func[T, HR],
                                        FuncLike[T, HI],
                                        Fb,
                                        Index,
                                        IF,
                                        IDF,
                                        IDFT] {
      def mapper(
          implicit fmlyMapper: TypFamilyMapper[H,
                                               Fb,
                                               Cod,
                                               Index,
                                               IF,
                                               IDF,
                                               IDFT]) = {
        case IndexedCnstFuncConsShape(t, h) =>
          IndexedCnstFncPtnMap(t, head.mapper(fmlyMapper)(h))
      }
    }

  implicit def indexedCnstDepFncPtnMapper[HShape <: HList,
                                          T <: Term with Subs[T],
                                          Cod <: Term with Subs[Cod],
                                          HC <: Term with Subs[HC],
                                          H <: Term with Subs[H],
                                          HR <: Term with Subs[HR],
                                          HI <: Term with Subs[HI],
                                          Fb <: Term with Subs[Fb],
                                          Index <: HList: TermList,
                                          IF <: Term with Subs[IF],
                                          IDF <: Term with Subs[IDF],
                                          IDFT <: Term with Subs[IDFT]](
      implicit head: IndexedConstructorPatternMapper[HShape,
                                                     Cod,
                                                     HC,
                                                     H,
                                                     HR,
                                                     HI,
                                                     Fb,
                                                     Index,
                                                     IF,
                                                     IDF,
                                                     IDFT])
    : _root_.provingground.induction.IndexedConstructorPatternMapper[
      _root_.shapeless.::[
        _root_.provingground.induction.IndexedConstructorShape.IndexedCnstDepFuncConsShape.type,
        HShape],
      Cod,
      _root_.provingground.HoTT.FuncLike[T, HC],
      H,
      _root_.provingground.HoTT.FuncLike[T, HR],
      _root_.provingground.HoTT.FuncLike[T, HI],
      Fb,
      Index,
      IF,
      IDF,
      IDFT] =
    new IndexedConstructorPatternMapper[
      IndexedCnstDepFuncConsShape.type :: HShape,
      Cod,
      FuncLike[T, HC],
      H,
      FuncLike[T, HR],
      FuncLike[T, HI],
      Fb,
      Index,
      IF,
      IDF,
      IDFT] {
      def mapper(
          implicit fmlyMapper: TypFamilyMapper[H,
                                               Fb,
                                               Cod,
                                               Index,
                                               IF,
                                               IDF,
                                               IDFT]) = {
        case IndexedCnstDepFuncConsShape(t, hf) =>
          IndexedCnstDepFncPtnMap(t, (x: T) => head.mapper(fmlyMapper)(hf(x)))
      }
    }
}

case class IndexedConstructor[S <: HList,
                              H <: Term with Subs[H],
                              F <: Term with Subs[F],
                              ConstructorType <: Term with Subs[
                                ConstructorType],
                              Index <: HList: TermList](
    name: AnySym,
    shape: IndexedConstructorShape[S, H, F, ConstructorType, Index])

object IndexedConstructor {}
