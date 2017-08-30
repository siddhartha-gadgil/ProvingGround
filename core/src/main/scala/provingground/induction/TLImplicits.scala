package provingground.induction

import provingground._, HoTT._

// import scala.language.existentials

import shapeless._

/**
  * implicits for constructing inductive types
  */
object TLImplicits extends InductionImplicits

trait InductionImplicits {
  import IterFuncShape._

  import ConstructorShape._

  implicit class ConstructorHead[H <: Term with Subs[H]](typ: Typ[H]) {
    def pair              = ConstructorTypTL(IdShape[H], typ)
    def :::(name: AnySym) = name ::: pair

    def ->>:[T <: Term with Subs[T]](that: Typ[T]) = that ->>: pair

    def -->>:(that: Typ[H]) = that -->>: pair

    def -->>:[F <: Term with Subs[F]](that: IterFuncShape[H, F]) =
      that -->>: pair

    def ~>>:[T <: Term with Subs[T]](thatVar: H) = thatVar ~>>: pair
  }

  implicit class TypAsSeqHead[H <: Term with Subs[H]](W: Typ[H]) {
    def seq = ConstructorSeqTL.Empty(W)

    def =:[S <: HList, ConstructorType <: Term with Subs[ConstructorType]](
        head: ConstructorTL[S, H, ConstructorType]) = head |: seq
  }

  implicit class IterFuncTypHead[O <: Term with Subs[O]](typ: Typ[O]) {

    def -|>:[TT <: Term with Subs[TT]](tail: Typ[TT]) =
      FuncShape(tail, IdIterShape[O])
  }

  implicit class IndexedFamily[F <: Term with Subs[F],
                               H <: Term with Subs[H],
                               Index <: HList](W: F)(
      implicit val g: TypFamilyPtnGetter[F, H, Index]) {

    def emptySeq =
      IndexedConstructorSeqDom.get(W)(g)

    def =::[HShape <: HList, HC <: Term with Subs[HC]](
        head: IndexedConstructor[HShape, H, F, HC, Index]) = {
      val seq = emptySeq
      head |: seq
    }

    def :>(typ: Typ[H]) = {
      val fmly        = g.get(W)
      val ind         = fmly.getIndex(W, typ).get
      implicit val gs = g.subst

      IndexedConstructorShape.IndexedIdShape(fmly, ind)
    }
  }

  implicit class IndexedPair[F <: Term with Subs[F],
                             H <: Term with Subs[H],
                             Index <: HList](wt: (F, Typ[H]))(
      implicit val g: TypFamilyPtnGetter[F, H, Index]) {
    def W = wt._1

    implicit val gs = g.subst

    def typ = wt._2

    def fmly = g.get(W)

    def iterHead =
      IndexedConstructorShape.get(W, typ)

    def ~>>:[T <: Term with Subs[T]](tailVar: T) =
      (iterHead).~>>:(tailVar)

    def :::(name: AnySym) = name ::: iterHead

    def ->>:[T <: Term with Subs[T]](tail: Typ[T]) =
      (iterHead).->>:(tail)

    import IndexedConstructorShape._

    // def -->>:(tail: Typ[H]) = {
    //   val fmly = g.get(W)
    //   val ind = fmly
    //     .getIndex(W, typ)
    //     .get
    //
    //     (iterHead).-->>:(IndexedConstructorShape.IndexedIdShape(fmly, ind))
    // }

    def -->>:(that: IndexedIdShape[H, F, Index]) = {
      IndexedFuncConsShape(IdIterShape[H], iterHead, that.index)
    }
  }
}
