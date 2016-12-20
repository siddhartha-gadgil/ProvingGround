package provingground

import HoTT._
import math._
//import scala.language.existentials

//import scala.util.Try

trait HeadTerm extends Term with Subs[HeadTerm]

object IterFuncPatternMap {
  sealed trait IterFuncPtnMap[S <: Term with Subs[S],
                              O <: Term with Subs[O],
                              C <: Term with Subs[C],
                              F <: Term with Subs[F],
                              TT <: Term with Subs[TT],
                              DT <: Term with Subs[DT]] {

    /**
      * the universe containing the type
      */
    val univLevel: Int

    type Cod = C

    /**
      * scala type (upper bound) for a member of the family, i.e., sections
      */
    type Family = F

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
    def apply(tp: Typ[O]): Typ[Family]

    /**
      * target scala type.
      */
    def target(x: Typ[Cod]): Typ[TT]

    /**
      * dependent target scala type.
      */
    def depTarget(xs: Func[O, Typ[Cod]]): Family => Typ[DT]

    def subs(x: Term, y: Term): IterFuncPtnMap[S, O, C, F, TT, DT]

    /**
      * function induced by f: W -> X of type (A -> W) -> (A -> X) etc
      *
      * @param f function from which to induce
      *
      * @param W the inductive type
      *
      * @param X codomain of the given function
      */
    def induced(f: Func[O, Cod]): Family => TT

    /**
      * dependent function induced by dependent f: W -> X(s) of type (A -> W) -> ((a : A) ~> Xs(a)) etc
      *
      * @param f dependent function from which to induce
      *
      * @param W the inductive type
      *
      * @param Xs family of codomains of the given dependent function
      */
    def inducedDep(f: FuncLike[O, Cod]): Family => DT
  }

  /**
    * The identity family
    */
  case class IdIterPtnMap[O <: Term with Subs[O], C <: Term with Subs[C]]()
      extends IterFuncPtnMap[HeadTerm, O, C, O, C, C] {

    def apply(W: Typ[O]) = W

    //    type Family =  O

    type FamilyType = Typ[O]

    type Total = O

    def value(x: Total): O = x

    type IterFunc = Func[O, C]

    type IterTypFunc = Func[O, Typ[C]]

    type IterDepFunc = FuncLike[O, C]

    def iterFuncTyp(w: FamilyType, x: Typ[Cod]): Typ[IterFunc] = w ->: x

    def iterDepFuncTyp(w: FamilyType, xs: IterTypFunc) = PiDefn(xs)

    type TargetType = C

    type DepTargetType = C

    def target(x: Typ[Cod]) = x

    def depTarget(xs: Func[O, Typ[Cod]]) = xs

    def subs(x: Term, y: Term) = this

    //    type Cod = C

    //    type MemberType = O

    val univLevel = 0

    /**
      * induced function is the given one.
      */
    def induced(f: Func[O, C]) = f

    /**
      * induced function is the given one.
      */
    def inducedDep(f: FuncLike[O, C]) = f
  }

  case class FuncIterPtnMap[HS <: Term with Subs[HS],
                            TT <: Term with Subs[TT],
                            V <: Term with Subs[V],
                            T <: Term with Subs[T],
                            D <: Term with Subs[D],
                            O <: Term with Subs[O],
                            C <: Term with Subs[C]](
      tail: Typ[TT],
      head: IterFuncPtnMap[HS, O, C, V, T, D]
  )
      extends IterFuncPtnMap[
          Func[TT, HS], O, C, Func[TT, V], Func[TT, T], FuncLike[TT, D]] {
    self =>
    def apply(W: Typ[O]) = FuncTyp[TT, V](tail, head(W))

    type DepTargetType = FuncLike[TT, D]

    type TargetType = Func[TT, T]

    def target(x: Typ[Cod]) = tail ->: head.target(x)

    def depTarget(xs: Func[O, Typ[Cod]]) = (fmly: Family) => {
      val a = tail.Var
      val b = fmly(a)
      val targfibre = lmbda(a)(headfibre(a).depTarget(xs)(b))
      piDefn(a)(headfibre(a).depTarget(xs)(b))
    }

    //    type Cod = head.Cod

    def subs(x: Term, y: Term) =
      FuncIterPtnMap(tail, head.subs(x, y))

    val headfibre = (arg: Term) => head

    val univLevel = max(head.univLevel, univlevel(tail.typ))

    /**
      * inductively defining the induced function.
      * maps (g : tail --> head(W)) to func : tail --> head(X) given (head(W) --> head(X))
      *
      */
    def induced(f: Func[O, Cod]): Family => TargetType = {
      val x = tail.Var
      val g = apply(f.dom).Var
      lmbda(g)(
          lmbda(x)(head.induced(f)(g(x)))
      )
    }

    /**
      * inductively defining the induced function.
      * maps (g : tail --> head(W)) to func : (t : tail) ~> head(Xs(t)) given (head(W) --> (t: tail) ~> head(Xs(t)))
      *
      */
    def inducedDep(f: FuncLike[O, Cod]): Family => DepTargetType = {
      val x = tail.Var
      val g = apply(f.dom).Var
      lambda(g)(
          lambda(x)(head.inducedDep(f)(g(x)))
      )
    }
  }

  /**
    * Extending by a constant type A a family of type patterns depending on (a : A).
    *
    */
  case class DepFuncIterPtnMap[HS <: Term with Subs[HS],
                               TT <: Term with Subs[TT],
                               V <: Term with Subs[V],
                               T <: Term with Subs[T],
                               D <: Term with Subs[D],
                               O <: Term with Subs[O],
                               C <: Term with Subs[C]](
      tail: Typ[TT],
      headfibre: TT => IterFuncPtnMap[HS, O, C, V, T, D],
      headlevel: Int = 0
  )
      extends IterFuncPtnMap[FuncLike[TT, HS],
                             O,
                             C,
                             FuncLike[TT, V],
                             FuncLike[TT, T],
                             FuncLike[TT, D]] {

    //    type Family =  FuncLike[Term, V]

//    type TargetType = FuncLike[TT, T]

    // type DepTargetType = FuncLike[Term, D]

    def apply(W: Typ[O]) = {
      val x = tail.Var
      val fiber = lmbda(x)(headfibre(x)(W))
      //   val fiber = typFamily(tail,  (t : Term) => headfibre(t)(W))
      piDefn[TT, V](x)(headfibre(x)(W))
    }

    //  type Cod = C

    def target(x: Typ[C]) = {
      val a = tail.Var
      val targfibre = lmbda(a)(headfibre(a).target(x))
      piDefn(a)(headfibre(a).target(x))
    }

    def depTarget(xs: Func[O, Typ[C]]) = (fmly: FuncLike[TT, V]) => {
      val a = tail.Var
      val b = fmly(a)
      val targfibre = lmbda(a)(headfibre(a).depTarget(xs)(b))
      piDefn(a)(headfibre(a).depTarget(xs)(b))
    }

    def subs(x: Term, y: Term) =
      DepFuncIterPtnMap(
          tail.replace(x, y), (tt: TT) => headfibre(tt).subs(x, y))

    //    val head = headfibre(tail.Var)

    //    type Family = FuncLike[Term, head.Family]

    def induced(f: Func[O, C]): FuncLike[TT, V] => FuncLike[TT, T] = {
      val x = tail.Var
      val g = apply(f.dom).Var
      lambda(g)(
          lambda(x)(headfibre(x).induced(f)(g(x)))
      )
    }

    def inducedDep(f: FuncLike[O, C]): FuncLike[TT, V] => FuncLike[TT, D] = {
      val x = tail.Var
      val g = apply(f.dom).Var
      lambda(g)(
          lambda(x)(headfibre(x).inducedDep(f)(g(x)))
      )
    }

    val univLevel = max(univlevel(tail.typ), headlevel)
  }

  import scala.language.existentials

  // sealed trait IterFuncMapper[
  //   S <: Term with Subs[S],
  //   O <: Term with Subs[O], C <: Term with Subs[C], F <: Term with Subs[F],
  //       TT <: Term with Subs[TT], DT <: Term with Subs[DT]] {
  //         def mapper(shape: IterFuncShape[S]) : IterFuncPtnMap[S, O, C, F,TT, DT]
  //       }

  sealed trait IterFuncMapper[S <: Term with Subs[S],
                              O <: Term with Subs[O],
                              C <: Term with Subs[C],
                              F <: Term with Subs[F],
                              TT <: Term with Subs[TT],
                              DT <: Term with Subs[DT]] {
    // need types to be  typelevel for the dependent case, so we pick a mapper and use it for all fibres.
    def mapper: IterFuncShape[S] => IterFuncPtnMap[S, O, C, F, TT, DT]
  }

  object IterFuncMapper {
    implicit def idIterMapper[O <: Term with Subs[O], C <: Term with Subs[C]] =
      new IterFuncMapper[HeadTerm, O, C, O, C, C] {
        def mapper =
          (shape: IterFuncShape[HeadTerm]) => IdIterPtnMap[O, C]
      }

    implicit def funcIterMapper[Tail <: Term with Subs[Tail],
                                HS <: Term with Subs[HS],
                                O <: Term with Subs[O],
                                C <: Term with Subs[C],
                                HF <: Term with Subs[HF],
                                HTT <: Term with Subs[HTT],
                                HDT <: Term with Subs[HDT]](
        implicit hm: IterFuncMapper[HS, O, C, HF, HTT, HDT]) =
      new IterFuncMapper[Func[Tail, HS],
                         O,
                         C,
                         Func[Tail, HF],
                         Func[Tail, HTT],
                         FuncLike[Tail, HDT]] {
        def mapper = {
          case FuncShape(t, h) => FuncIterPtnMap(t, hm.mapper(h))
        }
      }

    implicit def depFuncIterMapper[Tail <: Term with Subs[Tail],
                                   HS <: Term with Subs[HS],
                                   O <: Term with Subs[O],
                                   C <: Term with Subs[C],
                                   HF <: Term with Subs[HF],
                                   HTT <: Term with Subs[HTT],
                                   HDT <: Term with Subs[HDT]](
        implicit hm: IterFuncMapper[HS, O, C, HF, HTT, HDT]) =
      new IterFuncMapper[FuncLike[Tail, HS],
                         O,
                         C,
                         FuncLike[Tail, HF],
                         FuncLike[Tail, HTT],
                         FuncLike[Tail, HDT]] {
        def mapper = {
          case DepFuncShape(t, hf) =>
            DepFuncIterPtnMap(t, (tt: Tail) => hm.mapper(hf(tt)))
        }
      }
  }

  sealed trait IterFuncShape[S <: Term with Subs[S]] {
    // need types to be  typelevel for the dependent case, so we pick a mapper and use it for all fibres.
    // def mapper[
    //   O <: Term with Subs[O],
    //   C <: Term with Subs[C]]: IterFuncShape[S] => IterFuncPtnMap[S, O, C, F,TT, DT] forSome {
    //                             type F <: Term with Subs[F];
    //                             type TT <: Term with Subs[TT];
    //                             type DT <: Term with Subs[DT]
    //                           }

    def mapper[O <: Term with Subs[O], C <: Term with Subs[C]]
      : IterFuncMapper[S, O, C, F, TT, DT] forSome {
        type F <: Term with Subs[F];
        type TT <: Term with Subs[TT];
        type DT <: Term with Subs[DT]
      }

    def subs(x: Term, y: Term): IterFuncShape[S]

    def mapped[O <: Term with Subs[O], C <: Term with Subs[C]] =
      mapper[O, C].mapper(this)
  }

  case object IdIterShape extends IterFuncShape[HeadTerm] {
    def subs(x: Term, y: Term) = IdIterShape

    def mapper[O <: Term with Subs[O], C <: Term with Subs[C]] =
      implicitly[IterFuncMapper[HeadTerm, O, C, O, C, C]]
    //  (shape: IterFuncShape[HeadTerm]) => IdIterPtnMap[O, C]
  }

  case class FuncShape[TT <: Term with Subs[TT], HS <: Term with Subs[HS]](
      tail: Typ[TT], head: IterFuncShape[HS])
      extends IterFuncShape[Func[TT, HS]] {

    def subs(x: Term, y: Term) = FuncShape(tail.replace(x, y), head.subs(x, y))

    def mapper[O <: Term with Subs[O], C <: Term with Subs[C]] = {
      IterFuncMapper.funcIterMapper(head.mapper)
      // case FuncShape(t, h) => FuncIterPtnMap(t, head.mapper(h))
    }
  }

  case class DepFuncShape[TT <: Term with Subs[TT], HS <: Term with Subs[HS]](
      tail: Typ[TT], headfibre: TT => IterFuncShape[HS])
      extends IterFuncShape[FuncLike[TT, HS]] {
    def subs(x: Term, y: Term) =
      DepFuncShape(tail.replace(x, y), (t: TT) => headfibre(t).subs(x, y))

    def mapper[O <: Term with Subs[O], C <: Term with Subs[C]] = {
      IterFuncMapper.depFuncIterMapper(headfibre(tail.Var).mapper)
    }
  }
}
