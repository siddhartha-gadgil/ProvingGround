package provingground
import HoTT._
//import IterFuncPtn._
//import math.max
//import ScalaUniverses._
//import scala.util.Try
//import scala.language.existentials

//import IterFuncPattern.{IterFuncPtn => IterFuncPtn, _}

import IterFuncPatternMap._

//import RecFunction._

/**
  * constructors, their patterns, recursion
  * @author gadgil
  */
/**
  * A composite pattern for inductive types.
  * Typically (A -> B -> W)-> C -> W -> (D -> W) -> W as a function of W
  * May have Pi-types instead of function types
  * Assumed to have fixed type for codomain  X.
  *
  * @tparam H scala type of the terms of the inductive type.
  * @tparam ConstructorType  scala type of a constructor corresponding to this pattern.
  * @tparam Cod scala type of objects in codomain for recursion and induction functions.
  *
  * The type of the codomain is needed as there are inner types for data for recursion and induction functions.
  */
sealed trait ConstructorPatternMap[Cod <: Term with Subs[Cod],
                                ConstructorType <: Term with Subs[ConstructorType],
                                H <: Term with Subs[H],
                                RecDataType <: Term with Subs[RecDataType],
                                InducDataType <: Term with Subs[InducDataType]
                                ] { self =>


  def subs(x: Term, y: Term): ConstructorPatternMap[Cod, ConstructorType, H, RecDataType, InducDataType]

  /**
    * returns HoTT type corresponding to the pattern given the (inductive) type W (to be the head).
    */
  def apply(tp: Typ[H]): Typ[ConstructorType]

  /**
    * (scala) type of data for recursion corresponding to the single constructor
    */
//  type RecDataType <: Term with Subs[RecDataType]

  /**
    * (scala) type of data for recursion corresponding to the single constructor
    */
//  type InducDataType <: Term with Subs[InducDataType]

  /**
    * domain containing the recursion data for the constructor, i.e., the HoTT type of recursion data.
    */
  def recDataTyp(w: Typ[H], x: Typ[Cod]): Typ[RecDataType]

  /**
    * domain containing the induction data for the constructor, i.e., the HoTT type of the induction data.
    */
  def inducDataTyp(w: Typ[H], xs: Func[H, Typ[Cod]])(
      cons: ConstructorType): Typ[InducDataType]

  /**
    * given a term, matches to see if this is the image of a given (quasi)-constructor, with `this` constructor pattern.
    * optionally returns simplification (if the term matches), determined by the recursion data.
    * @param cons constructor, actually quasi-constructor, with which to match.
    * @param data definition data for the image of the constructor.
    * @param f the function being defined recursively, to be used recursively in definition.
    */
  def recDefCase(cons: ConstructorType,
                 data: RecDataType,
                 f: => Func[H, Cod]): H => Option[Cod]

  /**
    * given a term, matches to see if this is the image of a given (quasi)-constructor, with `this` constructor pattern.
    * optionally returns simplification (if the term matches).
    * @param cons constructor, actually quasi-constructor, with which to match.
    * @param data definition data for the image of the constructor.
    * @param f the function being defined inductively, to be used recursively in definition.
    */
  def inducDefCase(cons: ConstructorType,
                   data: InducDataType,
                   f: => FuncLike[H, Cod]): H => Option[Cod]
  val univLevel: Int
}


/**
  * The constructor pattern W - the only valid head for constructor-patterns.
  */
case class IdWMap[H <: Term with Subs[H]]()
    extends ConstructorPatternMap[Term, H, H, Term, Term] {
  def apply(W: Typ[H]) = W

  val univLevel = 0

  //    type ConstructorType = Term

  // type RecDataType = Term
  //
  // type InducDataType = Term

  def recDataTyp(w: Typ[H], x: Typ[Term]) = x

  def inducDataTyp(w: Typ[H], xs: Func[H, Typ[Term]])(
      cons: H): Typ[Term] = xs(cons)

  //    type Cod = Term

  def subs(x: Term, y: Term) = this

  def recDefCase(cons: H,
                 data: Term,
                 f: => Func[H, Term]): H => Option[Term] = {
    case (t: Term) if t == cons => Some(data)
    case _ => None
  }

  def inducDefCase(cons: H,
                   data: Term,
                   f: => FuncLike[H, Term]): Term => Option[Term] = {
    case (t: Term) if t == cons => Some(data)
    case _ => None
  }
}
case class IdTargMap[C <: Term with Subs[C], H <: Term with Subs[H]]()
    extends ConstructorPatternMap[C, H, H, C, C] {
  def apply(W: Typ[H]) = W

  val univLevel = 0

  //    type ConstructorType = Term

  type RecDataType = C

  type InducDataType = C

  //    type Cod = C

  def recDataTyp(w: Typ[H], x: Typ[C]) = x

  def inducDataTyp(w: Typ[H], xs: Func[H, Typ[C]])(
      cons: H): Typ[InducDataType] = xs(cons)

  def subs(x: Term, y: Term) = this

  def recDefCase(cons: H,
                 data: C,
                 f: => Func[H, C]): Term => Option[C] = {
    case (t: Term) if t == cons => Some(data)
    case _ => None
  }

  def inducDefCase(cons: H,
                   data: C,
                   f: => FuncLike[H, C]): Term => Option[C] = {
    case (t: Term) if t == cons => Some(data)
    case _ => None
  }
}

/**
  * Functional extension of a type pattern
  */
sealed trait RecursiveConstructorPatternMap[
    Cod <: Term with Subs[Cod],
    ArgType <: Term with Subs[ArgType],
    HeadConstructorType <: Term with Subs[HeadConstructorType],
    CT <: FuncLike[ArgType, HeadConstructorType] with Subs[CT],
    H <: Term with Subs[H],
    RecDataType <: Term with Subs[RecDataType],
    InducDataType <: Term with Subs[InducDataType],
    HeadRecDataType <: Term with Subs[HeadRecDataType],
    HeadInducDataType <: Term with Subs[HeadInducDataType]
    ]
    extends ConstructorPatternMap[Cod, CT, H, RecDataType, InducDataType] { self =>


  /**
    * The head pattern, constant T for A -> T and T(a) for A ~> T(a)
    */
  val headfibre: ArgType => ConstructorPatternMap[Cod, HeadConstructorType, H, HeadRecDataType, HeadInducDataType]
  /**
    * returns data for recursion to be passed on to the head given an argument (when matching with the constructor).
    */
  def headData(
      data: RecDataType, arg: ArgType, f: => Func[H, Cod]): HeadRecDataType

  def recDefCase(cons: CT,
                 data: RecDataType,
                 f: => Func[H, Cod]): H => Option[Cod] = { t =>
    for (arg <- getArg(cons)(t);
         term <- headfibre(arg).recDefCase(
                    cons(arg),
                    headData(data, arg, f), f)(t)) yield term
  }

  def headInducData(data: InducDataType,
                    arg: ArgType,
                    f: => FuncLike[H, Cod]): HeadInducDataType

  def inducDefCase(cons: CT,
                   data: InducDataType,
                   f: => FuncLike[H, Cod]): H => Option[Cod] = { t =>
    for (arg <- getArg(cons)(t);
         term <- headfibre(arg).inducDefCase(
                    cons(arg), headInducData(data, arg, f), f)(t)) yield term
  }
}

/**
  * Extending a constructor-pattern by a type pattern.
  */
case class FuncPtnMap[C <: Term with Subs[C],
                   F <: Term with Subs[F],
                   HC <: Term with Subs[HC],
                   H <: Term with Subs[H],
                   HR <: Term with Subs[HR],
                   HI <: Term with Subs[HI],
                   TT <: Term with Subs[TT],
                   DT <: Term with Subs[DT]](
    tail: IterFuncPtnMap[H, C, F, TT, DT],
    head: ConstructorPatternMap[C, HC, H, HR, HI]
)
    extends RecursiveConstructorPatternMap[
    C, F, HC, Func[F, HC], H,
    Func[F, Func[TT, HR]],
    FuncLike[F, Func[DT, HI]],HR, HI] { self =>
  //    type ArgType = F

  //    type HeadType = head.ConstructorType

  //    type Cod = C

  def subs(x: Term, y: Term) =
    FuncPtnMap(tail.subs(x, y), head.subs(x, y))


  val headfibre = (t: F) => head

  //    type ConstructorType = Func[ArgType, head.ConstructorType]

  def recDataTyp(w: Typ[H], x: Typ[C]) =
    tail(w) ->: tail.target(x) ->: head.recDataTyp(w, x)

  def inducDataTyp(w: Typ[H], xs: Func[H, Typ[C]])(
      cons: Func[F, HC]): Typ[FuncLike[F, Func[DT, HI]]] = {
    val a = tail(w).Var
    val headcons = cons(a)
    val fibre =
      lmbda(a)(tail.depTarget(xs)(a) ->: head.inducDataTyp(w, xs)(headcons))
    PiTyp(fibre)
  }

  def headData(
      data: Func[F, Func[TT, HR]], arg: F, f: => Func[H, C]): HR = {
    data(arg)(tail.induced(f)(arg))
  }

  def headInducData(data: FuncLike[F, Func[DT, HI]],
                    arg: F,
                    f: => FuncLike[H, C]): HI = {
    data(arg)(tail.inducedDep(f)(arg))
  }

  def apply(W: Typ[H]) =
    FuncTyp[F, HC](tail(W), head(W))

  val univLevel = math.max(head.univLevel, tail.univLevel)
}


/**
  * Extending a poly-pattern by a constant type, i.e., not depending on W.
  */
case class CnstFncPtnMap[T <: Term with Subs[T],
                      Cod <: Term with Subs[Cod],
                      HC <: Term with Subs[HC],
                      H <: Term with Subs[H],
                      HR <: Term with Subs[HR],
                      HI <: Term with Subs[HI]](
    tail: Typ[T],
    head: ConstructorPatternMap[Cod, HC, H, HR, HI]
)
    extends RecursiveConstructorPatternMap[
      Cod, T, HC, Func[T, HC], H,
      Func[T, HR], FuncLike[T, HI], HR, HI] { self =>
  //   type ArgType = Term

  //   type HeadType = head.ConstructorType

  //  type Cod = head.Cod

  def subs(x: Term, y: Term) =
    CnstFncPtnMap(tail.subs(x, y), head.subs(x, y))


  val headfibre = (t: T) => head


  def recDataTyp(w: Typ[H], x: Typ[Cod]) = tail ->: head.recDataTyp(w, x)

  def inducDataTyp(w: Typ[H], xs: Func[H, Typ[Cod]])(
      cons: Func[T, HC]): Typ[FuncLike[T, HI]] = {
    val a = tail.Var
    val headcons = cons(a)
    val fibre = lmbda(a)(head.inducDataTyp(w, xs)(headcons))
    PiTyp(fibre)
  }

  //   type ConstructorType = Func[Term, head.ConstructorType]

  def headData(
      data: Func[T, HR], arg: T, f: => Func[H, Cod]): HR =
    data(arg)

  def headInducData(data: FuncLike[T, HI],
                    arg: T,
                    f: => FuncLike[H, Cod]): HI = data(arg)

  def apply(W: Typ[H]) = FuncTyp[T, HC](tail, head(W))

  val univLevel = head.univLevel
}

case class CnstDepFuncPtnMap[T <: Term with Subs[T],
                          V <: Term with Subs[V],
                          VV <: Term with Subs[VV],
                          C <: Term with Subs[C],
                          HC <: Term with Subs[HC],
                          H <: Term with Subs[H], HR <: Term with Subs[HR],
                          HI <: Term with Subs[HI]](
    tail: Typ[T],
    headfibre: T => (ConstructorPatternMap[C, HC, H, V, VV]),
    headlevel: Int = 0
)
    extends RecursiveConstructorPatternMap[
      C, T, HC, FuncLike[T, HC], H, FuncLike[T, V], FuncLike[T, VV], V, VV] { self =>

  //    type ArgType = Term

  //    type HeadType = U

  //  type Cod = C

  def subs(x: Term, y: Term) = {

    CnstDepFuncPtnMap(tail.replace(x, y), (t: T) => headfibre(t).subs(x, y))
  }
  //    type ConstructorType = FuncLike[Term, U]

  type RecDataType = FuncLike[T, V]

  type InducDataType = FuncLike[T, VV]

  def recDataTyp(w: Typ[H], x: Typ[C]) = {
    val a = tail.Var
    val fibre = lmbda(a)(headfibre(a).recDataTyp(w, x))
    PiTyp(fibre)
  }

  def inducDataTyp(w: Typ[H], xs: Func[H, Typ[C]])(
      cons: FuncLike[T, HC]): Typ[InducDataType] = {
    val a = tail.Var
    val headcons = cons(a)
    val fibre = lmbda(a)(headfibre(a).inducDataTyp(w, xs)(headcons))
    PiTyp(fibre)
  }

  type HeadRecDataType = V

  type HeadInducDataType = VV

  def headData(
      data: RecDataType, arg: T, f: => Func[H, C]): V = {
    data(arg)
  }

  def headInducData(data: InducDataType,
                    arg: T,
                    f: => FuncLike[H, C]): HeadInducDataType = data(arg)

  def apply(W: Typ[H]): Typ[FuncLike[T, HC]] = {
    //     val fiber = typFamily[Term, U](tail,  (t : Term) => headfibre(t)(W))
    val a = tail.Var
    val fiber = lmbda(a)(headfibre(a)(W))
    PiTyp[T, HC](fiber)
  }

  //    type ConstructorType = Term

  val univLevel = headlevel
}
