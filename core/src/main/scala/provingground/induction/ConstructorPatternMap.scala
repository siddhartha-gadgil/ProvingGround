package provingground.induction
import provingground._, HoTT._
//import Iternn._
//import math.max
//import ScalaUniv._
//import scala.util.Try
//import scala.language.existentials

//import IterFuncPattern.{IterFuncPtn => IterFuncPtn, _}

import IterFuncPtnMap._
import IterFuncShape._

import shapeless._

//import RecFunction._

/**
  * Introduction rule for an inductive type,
  * as in [[ConstructorShape]] with the ''scala'' type of the codomain specified;
  * hence the scala type of the recurion and induction types are determined.
  * The definitions of recursion and induction
  * functions for the case matching the introduction rule are the abstract methods
  * [[recDefCase]] and [[inducDefCase]].
  *
  * @tparam H the scala type of terms of an inductive type that can have this constructor.
  * @tparam Cod the scala type of the codomain.
  * @tparam ConstructorType the scala type of the introduction rule
  * @tparam RecDataType type of data for recursive definitions for the case corresponding to this introduction rule.
  * @tparam InducDataType type of data for inductive definitions for  the case corresponding to this introduction rule.
  *
  * this is used indirectly through [[ConstructorSeqTL]]
  *
  */
sealed trait ConstructorPatternMap[
    Cod <: Term with Subs[Cod],
    ConstructorType <: Term with Subs[ConstructorType],
    H <: Term with Subs[H],
    RecDataType <: Term with Subs[RecDataType],
    InducDataType <: Term with Subs[InducDataType]] { self =>

  def subs(x: Term, y: Term)
    : ConstructorPatternMap[Cod, ConstructorType, H, RecDataType, InducDataType]

  /**
    * domain containing the recursion data for the constructor, i.e., the HoTT type of recursion data.
    */
  def recDataTyp(w: Typ[H], x: Typ[Cod]): Typ[RecDataType]

  def codFromData(d: RecDataType): Typ[Cod]

  /**
    * domain containing the induction data for the constructor, i.e., the HoTT type of the induction data.
    */
  def inducDataTyp(w: Typ[H], xs: Func[H, Typ[Cod]])(
      cons: ConstructorType): Typ[InducDataType]

  /**
    * given a term, matches to see if this is the image of a given (quasi)-constructor, with `this` constructor pattern.
    * optionally returns simplification (if the term matches), determined by the recursion data.
    *
    * @param cons constructor, actually quasi-constructor, with which to match.
    * @param data definition data for the image of the constructor.
    * @param f the function being defined recursively, to be used recursively in definition.
    *
    */
  def recDefCase(cons: ConstructorType,
                 data: RecDataType,
                 f: => Func[H, Cod]): H => Option[Cod]

  /**
    * given a term, matches to see if this is the image of a given (quasi)-constructor,
    * with `this` constructor pattern.
    * optionally returns simplification (if the term matches).
    *
    * @param cons constructor, actually quasi-constructor, with which to match.
    * @param data definition data for the image of the constructor.
    * @param f the function being defined inductively, to be used recursively in definition.
    *
    */
  def inducDefCase(cons: ConstructorType,
                   data: InducDataType,
                   f: => FuncLike[H, Cod]): H => Option[Cod]
  val univLevel: Int

}

object ConstructorPatternMap {

  /**
    * [[ConstructorPatternMap]] for recursion, induction corresponding to the constant shape `W`
    */
  case class IdTargMap[C <: Term with Subs[C], H <: Term with Subs[H]]()
      extends ConstructorPatternMap[C, H, H, C, C] {

    val univLevel = 0

    //    type ConstructorType = Term

    type RecDataType = C

    type InducDataType = C

    //    type Cod = C

    def recDataTyp(w: Typ[H], x: Typ[C]) = x

    def codFromData(d: RecDataType): Typ[C] = d.typ.asInstanceOf[Typ[C]]

    def inducDataTyp(w: Typ[H], xs: Func[H, Typ[C]])(
        cons: H): Typ[InducDataType] = xs(cons)

    def subs(x: Term, y: Term) = this

    def recDefCase(cons: H, data: C, f: => Func[H, C]): H => Option[C] = {
      case (t: Term) if t == cons => Some(data)
      case _                      => None
    }

    def inducDefCase(cons: H,
                     data: C,
                     f: => FuncLike[H, C]): Term => Option[C] = {
      case (t: Term) if t == cons => Some(data)
      case _                      => None
    }
  }

  /**
    * Functional extension of [[ConstructorPatternMap]],
    * Recursively defines the functions needed for recursion and induction in terms of
    * those for the `head`
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
      HeadInducDataType <: Term with Subs[HeadInducDataType]]
      extends ConstructorPatternMap[Cod, CT, H, RecDataType, InducDataType] {
    self =>

    /**
      * The head pattern map, constant T for A -> T and T(a) for A ~> T(a)
      */
    val headfibre: ArgType => ConstructorPatternMap[Cod,
                                                    HeadConstructorType,
                                                    H,
                                                    HeadRecDataType,
                                                    HeadInducDataType]

    /**
      * returns data for recursion to be passed on to the head given an argument
      * (when matching with the constructor).
      */
    def headData(data: RecDataType,
                 arg: ArgType,
                 f: => Func[H, Cod]): HeadRecDataType

    def recDefCase(cons: CT,
                   data: RecDataType,
                   f: => Func[H, Cod]): H => Option[Cod] = { t =>
      for (arg <- getArg(cons)(t);
           term <- headfibre(arg).recDefCase(cons(arg),
                                             headData(data, arg, f),
                                             f)(t)) yield {
        term
      }
    }

    /**
      * returns data for induction to be passed on to the head given an argument
      * (when matching with the constructor).
      */
    def headInducData(data: InducDataType,
                      arg: ArgType,
                      f: => FuncLike[H, Cod]): HeadInducDataType

    def inducDefCase(cons: CT,
                     data: InducDataType,
                     f: => FuncLike[H, Cod]): H => Option[Cod] = { t =>
      for (arg <- getArg(cons)(t);
           term <- headfibre(arg).inducDefCase(cons(arg),
                                               headInducData(data, arg, f),
                                               f)(t)) yield term
    }
  }

  object Debug {
    val rnd = new scala.util.Random
  }

  /**
    * [[ConstructorPatternMap]] corresponding to introduction rule [[ConstructorShape.FuncConsShape]]
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
      head: ConstructorPatternMap[C, HC, H, HR, HI])
      extends RecursiveConstructorPatternMap[C,
                                             F,
                                             HC,
                                             Func[F, HC],
                                             H,
                                             Func[F, Func[TT, HR]],
                                             FuncLike[F, Func[DT, HI]],
                                             HR,
                                             HI] { self =>

    def subs(x: Term, y: Term) =
      FuncPtnMap(tail.subs(x, y), head.subs(x, y))

    val headfibre = (t: F) => head

    //    type ConstructorType = Func[ArgType, head.ConstructorType]

    def recDataTyp(w: Typ[H], x: Typ[C]) =
      tail(w) ->: tail.target(x) ->: head.recDataTyp(w, x)

    def codFromData(d: Func[F, Func[TT, HR]]): Typ[C] = {
      val x = d.dom.Var
      val y = d(x).dom.Var
      head.codFromData(d(x)(y))
    }

    def inducDataTyp(w: Typ[H], xs: Func[H, Typ[C]])(
        cons: Func[F, HC]): Typ[FuncLike[F, Func[DT, HI]]] = {
      val a        = tail(w).Var
      val headcons = cons(a)
      val fibre =
        lmbda(a)(tail.depTarget(xs)(a) ->: head.inducDataTyp(w, xs)(headcons))
      piDefn(a)(tail.depTarget(xs)(a) ->: head.inducDataTyp(w, xs)(headcons))
    }

    def headData(data: Func[F, Func[TT, HR]], arg: F, f: => Func[H, C]): HR = {
      val g      = tail.induced(f)
      val recres = g(arg)
      val result = data(arg)(recres)
      result
    }

    def headInducData(data: FuncLike[F, Func[DT, HI]],
                      arg: F,
                      f: => FuncLike[H, C]): HI = {
      data(arg)(tail.inducedDep(f)(arg))
    }

    // def apply(W: Typ[H]) =
    //   FuncTyp[F, HC](tail(W), head(W))

    val univLevel = math.max(head.univLevel, tail.univLevel)

  }

  /**
    * [[ConstructorPatternMap]] corresponding to [[ConstructorShape.CnstFuncConsShape]]
    */
  case class CnstFncPtnMap[T <: Term with Subs[T],
                           Cod <: Term with Subs[Cod],
                           HC <: Term with Subs[HC],
                           H <: Term with Subs[H],
                           HR <: Term with Subs[HR],
                           HI <: Term with Subs[HI]](
      tail: Typ[T],
      head: ConstructorPatternMap[Cod, HC, H, HR, HI])
      extends RecursiveConstructorPatternMap[Cod,
                                             T,
                                             HC,
                                             Func[T, HC],
                                             H,
                                             Func[T, HR],
                                             FuncLike[T, HI],
                                             HR,
                                             HI] { self =>

    def subs(x: Term, y: Term) =
      CnstFncPtnMap(tail.replace(x, y), head.subs(x, y))

    val headfibre = (t: T) => head

    def recDataTyp(w: Typ[H], x: Typ[Cod]) = tail ->: head.recDataTyp(w, x)

    def codFromData(d: Func[T, HR]): Typ[Cod] =
      head.codFromData(d(tail.Var))

    def inducDataTyp(w: Typ[H], xs: Func[H, Typ[Cod]])(
        cons: Func[T, HC]): Typ[FuncLike[T, HI]] = {
      val a = tail.Var
      if (a.typ != cons.dom) {
        pprint.log(a); pprint.log(a.typ); pprint.log(cons.dom); pprint.log(cons)
      }
      val headcons = cons(a)
      val fibre    = lmbda(a)(head.inducDataTyp(w, xs)(headcons))
      piDefn(a)(head.inducDataTyp(w, xs)(headcons))
    }

    //   type ConstructorType = Func[Term, head.ConstructorType]

    def headData(data: Func[T, HR], arg: T, f: => Func[H, Cod]): HR =
      data(arg)

    def headInducData(data: FuncLike[T, HI],
                      arg: T,
                      f: => FuncLike[H, Cod]): HI = data(arg)

    // def apply(W: Typ[H]) = FuncTyp[T, HC](tail, head(W))

    val univLevel = head.univLevel

  }

  /**
    * [[ConstructorPatternMap]] corresponding to [[ConstructorShape.CnstDepFuncConsShape]]
    */
  case class CnstDepFuncPtnMap[T <: Term with Subs[T],
                               V <: Term with Subs[V],
                               VV <: Term with Subs[VV],
                               C <: Term with Subs[C],
                               HC <: Term with Subs[HC],
                               H <: Term with Subs[H],
                               HR <: Term with Subs[HR],
                               HI <: Term with Subs[HI]](
      tail: Typ[T],
      headfibre: T => (ConstructorPatternMap[C, HC, H, V, VV]),
      headlevel: Int = 0)
      extends RecursiveConstructorPatternMap[C,
                                             T,
                                             HC,
                                             FuncLike[T, HC],
                                             H,
                                             FuncLike[T, V],
                                             FuncLike[T, VV],
                                             V,
                                             VV] { self =>

    //    type ArgType = Term

    //    type HeadType = U

    //  type Cod = C

    def subs(x: Term, y: Term) = {

      CnstDepFuncPtnMap(tail.replace(x, y),
                        (t: T) => headfibre(t.replace(y, x)).subs(x, y))
    }
    //    type ConstructorType = FuncLike[Term, U]

    type RecDataType = FuncLike[T, V]

    type InducDataType = FuncLike[T, VV]

    def recDataTyp(w: Typ[H], x: Typ[C]) = {
      val a     = tail.Var
      val fibre = lmbda(a)(headfibre(a).recDataTyp(w, x))
      piDefn(a)(headfibre(a).recDataTyp(w, x))
    }

    def codFromData(d: FuncLike[T, V]): Typ[C] = {
      val a = tail.Var
      headfibre(a).codFromData(d(a))
    }

    def inducDataTyp(w: Typ[H], xs: Func[H, Typ[C]])(
        cons: FuncLike[T, HC]): Typ[InducDataType] = {
      val a = tail.Var
      if (a.typ != cons.dom) {
        pprint.log(a); pprint.log(a.typ); pprint.log(cons.dom); pprint.log(cons)
      }
      val headcons = cons(a)
      val fibre    = lmbda(a)(headfibre(a).inducDataTyp(w, xs)(headcons))
      piDefn(a)(headfibre(a).inducDataTyp(w, xs)(headcons))
    }

    type HeadRecDataType = V

    type HeadInducDataType = VV

    def headData(data: RecDataType, arg: T, f: => Func[H, C]): V = {
      data(arg)
    }

    def headInducData(data: InducDataType,
                      arg: T,
                      f: => FuncLike[H, C]): HeadInducDataType = data(arg)

    val univLevel = headlevel

  }

}

import scala.language.existentials

/**
  * The introduction rule for an inductive type, as a function of the type;
  * typically (A -> B -> W)-> C -> W -> (D -> W) -> W as a function of W
  * May have Pi-types instead of function types.
  *
  * Usually constructed using the DSL in [[TLImplicits]]
  *
  * @tparam H scala type of the terms of the inductive type.
  * @tparam ConstructorType  scala type of a constructor (introduction rule) corresponding to this pattern.
  * @tparam S formal type to capture information at type level
  *
  */
sealed trait ConstructorShape[S <: HList,
                              H <: Term with Subs[H],
                              ConstructorType <: Term with Subs[
                                ConstructorType]] {

  val introArgs: Int

  /**
    * returns HoTT type of the introduction rule given the (inductive) type W (to be the head).
    */
  def apply(tp: Typ[H]): Typ[ConstructorType]

  /**
    * helper to give [[ConstructorPatternMap]] when scala type of codomain is specified.
    */
  def mapper[Cod <: Term with Subs[Cod]]
    : ConstructorPatternMapper[S,
                               Cod,
                               ConstructorType,
                               H,
                               RecDataType,
                               InducDataType] forSome {
      type RecDataType <: Term with Subs[RecDataType];
      type InducDataType <: Term with Subs[InducDataType]
    }

  /**
    * lift to [[ConstructorPatternMap]] using an implicit [[ConstructorPatternMapper]],
    * which could from the [[mapper]] method.
    */
  def lift[Cod <: Term with Subs[Cod],
           RecDataType <: Term with Subs[RecDataType],
           InducDataType <: Term with Subs[InducDataType]](
      implicit mp: ConstructorPatternMapper[S,
                                            Cod,
                                            ConstructorType,
                                            H,
                                            RecDataType,
                                            InducDataType]) =
    mp.mapper(this)

  /**
    * lift to [[ConstructorPatternMap]] using the result of the [[mapper]] method.
    */
  def mapped[Cod <: Term with Subs[Cod]] =
    lift(mapper[Cod])
  //    mapper[Cod, H].mapper(this)

  /**
    * returns term giving introduction rule given inductive type and name
    */
  def symbcons(name: AnySym, tp: Typ[H]) =
    apply(tp).variable(name)

  def subs(x: Term, y: Term): ConstructorShape[S, H, ConstructorType]

  import ConstructorShape._

  /**
    * returns shape `that -> this' where `that` is of the form `W`, `A -> W` etc;
    * invoking this is an error if we `that` is independent of `W`
    */
  def -->:[F <: Term with Subs[F]](that: IterFuncShape[H, F]) =
    FuncConsShape(that, this)

  /**
    * returns shape `that -> this' where `that` must be the shape for inductive type `W`
    */
  def -->:(that: IdShape.type) = {
    FuncConsShape(IdIterShape[H](), this)
  }

  /**
    * returns shape `tail ->: this` where tail must be independent of the inductive type `W` being defined.
    */
  def ->:[T <: Term with Subs[T]](tail: Typ[T]) = CnstFuncConsShape(tail, this)

  /**
    * returns dependent shape `tail ~>: this` where tail must be independent of the inductive type `W` being defined.
    */
  def ~>:[T <: Term with Subs[T]](tailVar: T) = {
    // val fibre = (t: T) => this.subs(tailVar, t)
    import SubstInstances._
    val fib = Subst.Lambda(tailVar, this)

    CnstDepFuncConsShape(tailVar.typ.asInstanceOf[Typ[T]], fib)
  }
}

object ConstructorShape {
  import ConstructorPatternMapper._

  /**
    * [[ConstructorShape]] corresponding to the introduction rule W;
    * all constructor patterns are constructed from this.
    */
  case class IdShape[H <: Term with Subs[H]]()
      extends ConstructorShape[HNil, H, H] {
    val introArgs = 0

    def apply(tp: Typ[H]) = tp

    def mapper[C <: Term with Subs[C]] =
      ConstructorPatternMapper.idTargMapper[C, H]

    def subs(x: Term, y: Term) = this
  }

  object IdShape {
    def byTyp[H <: Term with Subs[H]](typ: Typ[H]) = IdShape[H]()
  }

  /**
    * [[ConstructorShape]] corresponding to an introduction rule of the form
    * `(.. -> W) -> head`
    */
  case class FuncConsShape[HS <: HList,
                           H <: Term with Subs[H],
                           HC <: Term with Subs[HC],
                           F <: Term with Subs[F]](
      tail: IterFuncShape[H, F],
      head: ConstructorShape[HS, H, HC])
      extends ConstructorShape[FuncConsShape.type :: HS, H, Func[F, HC]] {

    val introArgs = 2 + head.introArgs

    def apply(tp: Typ[H]) = tail(tp) ->: head(tp)

    def mapper[C <: Term with Subs[C]]
      : ConstructorPatternMapper[FuncConsShape.type :: HS,
                                 C,
                                 Func[F, HC],
                                 H,
                                 RecDataType,
                                 InducDataType] forSome {
        type RecDataType <: Term with Subs[RecDataType];
        type InducDataType <: Term with Subs[InducDataType]
      } = funcPtnMapper(tail.mapper[C], head.mapper[C])

    def subs(x: Term, y: Term) =
      FuncConsShape(tail.subs(x, y), head.subs(x, y))
  }

  /**
    * [[ConstructorShape]] corresponding to an introduction rule of the form
    * `A -> head` with `A` not dependent on the inductive type `W` being constructed
    */
  case class CnstFuncConsShape[HShape <: HList,
                               H <: Term with Subs[H],
                               HC <: Term with Subs[HC],
                               T <: Term with Subs[T],
                               HS <: Term with Subs[HS]](
      tail: Typ[T],
      head: ConstructorShape[HShape, H, HC])
      extends ConstructorShape[CnstFuncConsShape.type :: HShape, H, Func[T, HC]] {
    val introArgs = 1 + head.introArgs

    def apply(tp: Typ[H]) = tail ->: head(tp)

    def mapper[Cod <: Term with Subs[Cod]] =
      cnstFncPtnMapper(head.mapper[Cod])

    def subs(x: Term, y: Term) =
      CnstFuncConsShape(tail.replace(x, y), head.subs(x, y))
  }

  /**
    * [[ConstructorShape]] corresponding to an introduction rule of the form
    * `A ~> head` with `A` not dependent on the inductive type `W` being constructed
    */
  case class CnstDepFuncConsShape[HShape <: HList,
                                  H <: Term with Subs[H],
                                  HC <: Term with Subs[HC],
                                  T <: Term with Subs[T],
                                  HS <: Term with Subs[HS]](
      tail: Typ[T],
      headfibre: T => ConstructorShape[HShape, H, HC])
      extends ConstructorShape[CnstDepFuncConsShape.type :: HShape,
                               H,
                               FuncLike[T, HC]] {
    lazy val introArgs: Int = 1 + headfibre(tail.Var).introArgs

    def apply(W: Typ[H]): Typ[FuncLike[T, HC]] = {
      val a = tail.Var
      piDefn[T, HC](a)(headfibre(a)(W))
    }

    def mapper[Cod <: Term with Subs[Cod]] =
      cnstDepFncPtnMapper(headfibre(tail.Var).mapper[Cod])

    def subs(x: Term, y: Term) =
      CnstDepFuncConsShape(tail.replace(x, y),
                           (t: T) => headfibre(t.replace(y, x)).subs(x, y))
  }
}

import ConstructorShape._

object ConstructorPatternMapper {
  import ConstructorPatternMap._

  implicit def idTargMapper[C <: Term with Subs[C], H <: Term with Subs[H]]
    : ConstructorPatternMapper[HNil, C, H, H, C, C] =
    new ConstructorPatternMapper[HNil, C, H, H, C, C] {
      def mapper = (_) => IdTargMap[C, H]()
    }

  implicit def funcPtnMapper[HShape <: HList,
                             C <: Term with Subs[C],
                             F <: Term with Subs[F],
                             HC <: Term with Subs[HC],
                             H <: Term with Subs[H],
                             HR <: Term with Subs[HR],
                             HI <: Term with Subs[HI],
                             TT <: Term with Subs[TT],
                             DT <: Term with Subs[DT]](
      implicit tail: IterFuncMapper[H, C, F, TT, DT],
      head: ConstructorPatternMapper[HShape, C, HC, H, HR, HI])
    : ConstructorPatternMapper[FuncConsShape.type :: HShape,
                               C,
                               Func[F, HC],
                               H,
                               Func[F, Func[TT, HR]],
                               FuncLike[F, Func[DT, HI]]] =
    new ConstructorPatternMapper[FuncConsShape.type :: HShape,
                                 C,
                                 Func[F, HC],
                                 H,
                                 Func[F, Func[TT, HR]],
                                 FuncLike[F, Func[DT, HI]]] {
      def mapper = {
        case FuncConsShape(t, h) =>
          FuncPtnMap(tail.mapper(t), head.mapper(h))
      }
    }

  implicit def cnstFncPtnMapper[HShape <: HList,
                                T <: Term with Subs[T],
                                Cod <: Term with Subs[Cod],
                                HC <: Term with Subs[HC],
                                H <: Term with Subs[H],
                                HR <: Term with Subs[HR],
                                HI <: Term with Subs[HI]](
      implicit head: ConstructorPatternMapper[HShape, Cod, HC, H, HR, HI])
    : ConstructorPatternMapper[CnstFuncConsShape.type :: HShape,
                               Cod,
                               Func[T, HC],
                               H,
                               Func[T, HR],
                               FuncLike[T, HI]] =
    new ConstructorPatternMapper[CnstFuncConsShape.type :: HShape,
                                 Cod,
                                 Func[T, HC],
                                 H,
                                 Func[T, HR],
                                 FuncLike[T, HI]] {
      def mapper = {
        case CnstFuncConsShape(t, h) =>
          CnstFncPtnMap(t, head.mapper(h))
      }
    }

  implicit def cnstDepFncPtnMapper[HShape <: HList,
                                   T <: Term with Subs[T],
                                   Cod <: Term with Subs[Cod],
                                   HC <: Term with Subs[HC],
                                   H <: Term with Subs[H],
                                   HR <: Term with Subs[HR],
                                   HI <: Term with Subs[HI]](
      implicit head: ConstructorPatternMapper[HShape, Cod, HC, H, HR, HI])
    : ConstructorPatternMapper[CnstDepFuncConsShape.type :: HShape,
                               Cod,
                               FuncLike[T, HC],
                               H,
                               FuncLike[T, HR],
                               FuncLike[T, HI]] =
    new ConstructorPatternMapper[CnstDepFuncConsShape.type :: HShape,
                                 Cod,
                                 FuncLike[T, HC],
                                 H,
                                 FuncLike[T, HR],
                                 FuncLike[T, HI]] {
      def mapper = {
        case CnstDepFuncConsShape(t, hf) =>
          CnstDepFuncPtnMap(t, (tt: T) => head.mapper(hf(tt)))
      }
    }
}

/**
  * bridge between the definition [[ConstructorShape]] of an introduction rule and [[ConstructorPatternMap]] which depends also
  * on the scala type of the codomain, allowing cases for recursive and  inductive definition;
  * ideally used implicitly.
  */
sealed trait ConstructorPatternMapper[
    Shape <: HList,
    Cod <: Term with Subs[Cod],
    ConstructorType <: Term with Subs[ConstructorType],
    H <: Term with Subs[H],
    RecDataType <: Term with Subs[RecDataType],
    InducDataType <: Term with Subs[InducDataType]] {

  /**
    * the bridge function
    */
  def mapper
    : ConstructorShape[Shape, H, ConstructorType] => ConstructorPatternMap[
      Cod,
      ConstructorType,
      H,
      RecDataType,
      InducDataType]
}

/**
  * an introduction rule [[shape]] together with the iductive type [[typ]] being defined.
  */
case class ConstructorTypTL[S <: HList,
                            H <: Term with Subs[H],
                            ConstructorType <: Term with Subs[ConstructorType]](
    shape: ConstructorShape[S, H, ConstructorType],
    typ: Typ[H]) {

  import ConstructorShape._

  /**
    * returns a variable to act as an introduction rule.
    */
  def :::(name: AnySym) = ConstructorTL(name, shape, typ)

  /**
    * returns constructor with shape `W -> this';
    * invoking this is an error if we `that` is not `W`
    */
  def -->>:(that: Typ[H]) = {
    assert(
      that == typ,
      s"the method -->: is for extending by the same type but $that is not $typ")
    val tail = IdIterShape[H]()
    val ptn  = FuncConsShape(tail, shape)
    ConstructorTypTL(ptn, typ)
  }

  /**
    * returns constructor with shape `that -> this' where `that` is of the form `W`, `A -> W` etc;
    * invoking this is an error if we `that` is independent of `W`
    */
  def -->>:[F <: Term with Subs[F]](that: IterFuncShape[H, F]) =
    ConstructorTypTL(that -->: shape, typ)

  /**
    * returns constructor with shape `that -> this' where `that` is a type A;
    * invoking this is an error if we `that` is not independent of `W`
    */
  def ->>:[T <: Term with Subs[T]](that: Typ[T]) = {
    assert(
      !(that.dependsOn(typ)),
      "the method ->: is for extension by constant types, maybe you mean _-->:_")
    ConstructorTypTL(that ->: shape, typ)
  }

  /**
    * returns constructor with shape `that ~> this' where `that` is a type A;
    * invoking this is an error if we `that` is not independent of `W`
    */
  def ~>>:(thatVar: H) =
    ConstructorTypTL(thatVar ~>: shape, typ)
}

case class NoIntro(w: Typ[Term], cnstTyp: Typ[Term])
    extends Exception(s"unmatched ConstructorTyp, $w, $cnstTyp")

object ConstructorTypTL {

  /**
    * wrapped existential form of [[ConstructorTypTL]], to be used at runtime, translation etc where refined types are  unknown.
    */
  sealed trait Exst {
    type S <: HList
    type ConstructorType <: Term with Subs[ConstructorType]

    val value: ConstructorTypTL[S, Term, ConstructorType]

    /**
      * returns constructor with shape `W -> this';
      * invoking this is an error if we `that` is not `W`
      */
    def -->>:(that: IterFuncShape.Exst) =
      Exst(that.shape -->>: value)

    /**
      * returns constructor with shape `that -> this' where `that` is a type A;
      * invoking this is an error if we `that` is not independent of `W`
      */
    def ->>:(that: Typ[Term]) =
      Exst(that ->>: value)

    /**
      * returns constructor with shape `that ~> this' where `that` is a type A;
      * invoking this is an error if we `that` is not independent of `W`
      */
    def ~>>:(thatVar: Term) =
      Exst(thatVar ~>>: value)

    /**
      * returns a variable to act as an introduction rule.
      */
    def :::(name: AnySym) = name ::: value
  }

  object Exst {

    /**
      * helper for constructing existentials
      */
    def apply[Shape <: HList, CT <: Term with Subs[CT]](
        cns: ConstructorTypTL[Shape, Term, CT]) =
      new Exst {
        type S               = Shape
        type ConstructorType = CT

        val value = cns
      }
  }

  /**
    * returns existential form of [[ConstructorTypTL]] given the inductive type `w` and
    * the type `cnstTyp` of the introduction rule.
    */
  def getExst(w: Typ[Term], cnstTyp: Typ[Term]): Exst = cnstTyp match {
    case `w` => Exst(ConstructorTypTL(IdShape[Term](), w))
    case ft: FuncTyp[u, v] if (ft.dom.indepOf(w)) =>
      ft.dom ->>: getExst(w, ft.codom)
    case pd: PiDefn[u, v] if (pd.domain.indepOf(w)) =>
      pd.variable ~>>: getExst(w, pd.value)
    case ft: FuncTyp[u, v] =>
      IterFuncShape.getExst(w, ft.dom) -->>: getExst(w, ft.codom)
    case _ => throw NoIntro(w, cnstTyp)
  }
}

/**
  * an introduction rule for an inductive type
  */
case class ConstructorTL[S <: HList,
                         H <: Term with Subs[H],
                         ConstructorType <: Term with Subs[ConstructorType]](
    name: AnySym,
    shape: ConstructorShape[S, H, ConstructorType],
    W: Typ[H])
