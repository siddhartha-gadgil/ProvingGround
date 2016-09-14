package provingground

import HoTT._

import IterFuncPatternMap.IterFuncPtnMap

import Subst._

abstract class IndexedConstructorPatternMap[
    S <: Term with Subs[S],
    Cod <: Term with Subs[Cod],
    ConstructorType <: Term with Subs[ConstructorType],
    H <: Term with Subs[H],
    RecDataType <: Term with Subs[RecDataType],
    InducDataType <: Term with Subs[InducDataType],
    F <: Term with Subs[F],
    Index: Subst,
    IF <: Term with Subs[IF],
    IDF <: Term with Subs[IDF],
    IDFT <: Term with Subs[IDFT]
] { self =>

  val family: TypFamilyMap[H, F, Cod, Index, IF, IDF, IDFT]

  def subs(x: Term, y: Term): IndexedConstructorPatternMap[S,
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

  /**
    * returns HoTT type corresponding to the pattern given the (inductive) type W (to be the head).
    */
  def apply(tp: F): Typ[ConstructorType]

//  def symbcons(name: AnySym, tp: Typ[H]): ConstructorType =
//    apply(tp).symbObj(name)

  /**
    * domain containing the recursion data for the constructor, i.e., the HoTT type of recursion data.
    */
  def recDataTyp(w: F, x: Typ[Cod]): Typ[RecDataType]

  /**
    * domain containing the induction data for the constructor, i.e., the HoTT type of the induction data.
    */
  def inducDataTyp(w: F, xs: IDFT)(cons: ConstructorType): Typ[InducDataType]

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
                          F <: Term with Subs[F],
                          Index: Subst,
                          IF <: Term with Subs[IF],
                          IDF <: Term with Subs[IDF],
                          IDFT <: Term with Subs[IDFT]](
      family: TypFamilyMap[H, F, C, Index, IF, IDF, IDFT], index: Index)
      extends IndexedConstructorPatternMap[
          HeadTerm, C, H, H, C, C, F, Index, IF, IDF, IDFT] {

    def apply(W: F) = family.pattern.typ(W, index)

    val univLevel = 0

    def recDataTyp(w: F, x: Typ[C]) = x

    def inducDataTyp(w: F, xs: IDFT)(cons: H) =
      family.typRestrict(xs, index)(cons)

    def subs(x: Term, y: Term) = this

    def recDefCase(cons: H, data: C, f: => IF): H => Option[C] = {
      case (t: Term) if t == cons => Some(data)
      case _ => None
    }

    def inducDefCase(cons: H, data: C, f: => IDF): Term => Option[C] = {
      case (t: Term) if t == cons => Some(data)
      case _ => None
    }
  }

  abstract class IndexedRecursiveConstructorPatternMap[
      HS <: Term with Subs[HS],
      S <: Term with Subs[S],
      Cod <: Term with Subs[Cod],
      ArgType <: Term with Subs[ArgType],
      HeadConstructorType <: Term with Subs[HeadConstructorType],
      CT <: FuncLike[ArgType, HeadConstructorType] with Subs[CT],
      H <: Term with Subs[H],
      RecDataType <: Term with Subs[RecDataType],
      InducDataType <: Term with Subs[InducDataType],
      HeadRecDataType <: Term with Subs[HeadRecDataType],
      HeadInducDataType <: Term with Subs[HeadInducDataType],
      F <: Term with Subs[F],
      Index: Subst,
      IF <: Term with Subs[IF],
      IDF <: Term with Subs[IDF],
      IDFT <: Term with Subs[IDFT]]
      extends IndexedConstructorPatternMap[
          S,
          Cod,
          CT,
          H,
          RecDataType,
          InducDataType,
          F,
          Index,
          IF,
          IDF,
          IDFT
      ] { self =>

    /**
      * The head pattern, constant T for A -> T and T(a) for A ~> T(a)
      */
    val headfibre: ArgType => IndexedConstructorPatternMap[HS,
                                                           Cod,
                                                           HeadConstructorType,
                                                           H,
                                                           HeadRecDataType,
                                                           HeadInducDataType,
                                                           F,
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
             term <- headfibre(arg).recDefCase(
                        cons(arg), headData(data, arg, f), f)(t)) yield term
    }

    def headInducData(data: InducDataType,
                      arg: ArgType,
                      f: => IDF): HeadInducDataType

    def inducDefCase(
        cons: CT, data: InducDataType, f: => IDF): H => Option[Cod] = { t =>
      for (arg <- getArg(cons)(t);
           term <- headfibre(arg).inducDefCase(
                      cons(arg), headInducData(data, arg, f), f)(t)) yield term
    }
  }

  case class IndexedFuncPtnMap[HS <: Term with Subs[HS],
                               TS <: Term with Subs[TS],
                               C <: Term with Subs[C],
                               F <: Term with Subs[F],
                               HC <: Term with Subs[HC],
                               H <: Term with Subs[H],
                               HR <: Term with Subs[HR],
                               HI <: Term with Subs[HI],
                               TT <: Term with Subs[TT],
                               DT <: Term with Subs[DT],
                               Fb <: Term with Subs[Fb],
                               Index: Subst,
                               IF <: Term with Subs[IF],
                               IDF <: Term with Subs[IDF],
                               IDFT <: Term with Subs[IDFT]](
      tail: IterFuncPtnMap[TS, H, C, F, TT, DT],
      head: IndexedConstructorPatternMap[
          HS, C, HC, H, HR, HI, Fb, Index, IF, IDF, IDFT],
      ind: Index
  )
      extends IndexedRecursiveConstructorPatternMap[HS,
                                                    Func[TS, HS],
                                                    C,
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
      tail(family.pattern.typ(w, ind)) ->: tail.target(x) ->: head.recDataTyp(
          w, x)

    def inducDataTyp(w: Fb, xs: IDFT)(
        cons: Func[F, HC]): Typ[FuncLike[F, Func[DT, HI]]] = {
      val a = tail(family.pattern.typ(w, ind)).Var
      val headcons = cons(a)
      val xss = family.typRestrict(xs, ind)
      val fibre =
        lmbda(a)(tail.depTarget(xss)(a) ->: head.inducDataTyp(w, xs)(headcons))
      PiTyp(fibre)
    }

    def headData(data: Func[F, Func[TT, HR]], arg: F, f: => IF): HR = {
      data(arg)(tail.induced(family.restrict(f, ind))(arg))
    }

    def headInducData(data: FuncLike[F, Func[DT, HI]],
                      arg: F,
                      f: => IDF): HI = {
      data(arg)(tail.inducedDep(family.depRestrict(f, ind))(arg))
    }

    def apply(W: Fb) =
      tail(family.pattern.typ(W, ind)) ->: head(W)

    val univLevel = math.max(head.univLevel, tail.univLevel)
  }

  case class IndexedCnstFncPtnMap[HS <: Term with Subs[HS],
                           T <: Term with Subs[T],
                           Cod <: Term with Subs[Cod],
                           HC <: Term with Subs[HC],
                           H <: Term with Subs[H],
                           HR <: Term with Subs[HR],
                           HI <: Term with Subs[HI],
                           Fb <: Term with Subs[Fb],
                           Index: Subst,
                           IF <: Term with Subs[IF],
                           IDF <: Term with Subs[IDF],
                           IDFT <: Term with Subs[IDFT]](
      tail: Typ[T],
      head: IndexedConstructorPatternMap[HS, Cod, HC, H, HR, HI, Fb, Index, IF, IDF, IDFT]
  )
      extends IndexedRecursiveConstructorPatternMap[HS,
                                             Func[T, HS],
                                             Cod,
                                             T,
                                             HC,
                                             Func[T, HC],
                                             H,
                                             Func[T, HR],
                                             FuncLike[T, HI],
                                             HR,
                                             HI, Fb, Index, IF, IDF, IDFT] { self =>


    val family = head.family

    def subs(x: Term, y: Term) =
      IndexedCnstFncPtnMap(tail.subs(x, y), head.subs(x, y))

    val headfibre = (t: T) => head

    def recDataTyp(w: Fb, x: Typ[Cod]) = tail ->: head.recDataTyp(w, x)

    def inducDataTyp(w: Fb, xs: IDFT)(
        cons: Func[T, HC]) = {
      val a = tail.Var
      val headcons = cons(a)
      val fibre = lmbda(a)(head.inducDataTyp(w, xs)(headcons))
      PiTyp(fibre)
    }

    //   type ConstructorType = Func[Term, head.ConstructorType]

    def headData(data: Func[T, HR], arg: T, f: => IF): HR =
      data(arg)

    def headInducData(data: FuncLike[T, HI],
                      arg: T,
                      f: => IDF): HI = data(arg)

    def apply(W: Fb) = FuncTyp(tail, head(W))

    val univLevel = head.univLevel
  }

  case class IndexedCnstDepFncPtnMap[HS <: Term with Subs[HS],
                           T <: Term with Subs[T],
                           Cod <: Term with Subs[Cod],
                           HC <: Term with Subs[HC],
                           H <: Term with Subs[H],
                           HR <: Term with Subs[HR],
                           HI <: Term with Subs[HI],
                           Fb <: Term with Subs[Fb],
                           Index: Subst,
                           IF <: Term with Subs[IF],
                           IDF <: Term with Subs[IDF],
                           IDFT <: Term with Subs[IDFT]](
      tail: Typ[T],
      headfibre: T => IndexedConstructorPatternMap[HS, Cod, HC, H, HR, HI, Fb, Index, IF, IDF, IDFT]
  )
      extends IndexedRecursiveConstructorPatternMap[HS,
                                             Func[T, HS],
                                             Cod,
                                             T,
                                             HC,
                                             FuncLike[T, HC],
                                             H,
                                             FuncLike[T, HR],
                                             FuncLike[T, HI],
                                             HR,
                                             HI, Fb, Index, IF, IDF, IDFT] { self =>


    val family = headfibre(tail.Var).family

    def subs(x: Term, y: Term) =
      IndexedCnstDepFncPtnMap(tail.subs(x, y), (t: T) => headfibre(t).subs(x, y))


    def recDataTyp(w: Fb, x: Typ[Cod]) = {
      val a = tail.Var
      val fibre = lmbda(a)(headfibre(a).recDataTyp(w, x))
      PiTyp(fibre)
    }

    def inducDataTyp(w: Fb, xs: IDFT)(
        cons: FuncLike[T, HC]) = {
      val a = tail.Var
      val headcons = cons(a)
      val fibre = lmbda(a)(headfibre(a).inducDataTyp(w, xs)(headcons))
      PiTyp(fibre)
    }

    //   type ConstructorType = Func[Term, head.ConstructorType]

    def headData(data: FuncLike[T, HR], arg: T, f: => IF): HR =
      data(arg)

    def headInducData(data: FuncLike[T, HI],
                      arg: T,
                      f: => IDF): HI = data(arg)

    def apply(W: Fb) = {
      val a = tail.Var
      val fiber = lmbda(a)(headfibre(a)(W))
      PiTyp(fiber)
    }

    val univLevel = headfibre(tail.Var).univLevel
  }


}
