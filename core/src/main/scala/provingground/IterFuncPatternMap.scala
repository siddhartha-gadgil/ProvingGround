package provingground

import HoTT._
import math._
//import scala.language.existentials

//import scala.util.Try

object IterFuncPatternMap {
  sealed trait IterFuncPtnMap[
      O <: Term with Subs[O], C <: Term with Subs[C], F <: Term with Subs[F],
      TT <: Term with Subs[TT], DT <: Term with Subs[DT]] {

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


    def subs(x: Term, y: Term): IterFuncPtnMap[O, C, F, TT, DT]

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
      extends IterFuncPtnMap[O, C, O, C, C] {

    def apply(W: Typ[O]) = W

    //    type Family =  O

    type FamilyType = Typ[O]

    type Total = O

    def value(x: Total): O = x

    type IterFunc = Func[O, C]

    type IterTypFunc = Func[O, Typ[C]]

    type IterDepFunc = FuncLike[O, C]

    def iterFuncTyp(w: FamilyType, x: Typ[Cod]): Typ[IterFunc] = w ->: x

    def iterDepFuncTyp(w: FamilyType, xs: IterTypFunc) = PiTyp(xs)

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

  // trait RecIterPtnMap[TT <: Term with Subs[TT],
  //                  V <: Term with Subs[V],
  //                  T <: Term with Subs[T],
  //                  D <: Term with Subs[D],
  //                  O <: Term with Subs[O],
  //                  C <: Term with Subs[C]]
  //     extends IterFuncPtnMap[O, C, FuncLike[TT, V]] {
  //
  //   type TargetType <: FuncLike[TT, T] with Subs[TargetType]
  //
  //   type DepTargetType = FuncLike[TT, D]
  //
  //   val tail: Typ[TT]
  //
  //   val headfibre: TT => IterFuncPtnMap[O, C, V] {
  //     type TargetType = T; type DepTargetType = D
  //   }
  //
  //   def depTarget(xs: Func[O, Typ[Cod]]) = (fmly: Family) => {
  //     val a = tail.Var
  //     val b = fmly(a)
  //     val targfibre = lmbda(a)(headfibre(a).depTarget(xs)(b))
  //     PiTyp(targfibre)
  //   }
  // }

  case class FuncIterPtnMap[TT <: Term with Subs[TT],
                         V <: Term with Subs[V],
                         T <: Term with Subs[T],
                         D <: Term with Subs[D],
                         O <: Term with Subs[O],
                         C <: Term with Subs[C]](
      tail: Typ[TT],
      head: IterFuncPtnMap[O, C, V, T, D]
  )
      extends IterFuncPtnMap[O, C, Func[TT, V], Func[TT, T], FuncLike[TT, D]] { self =>
    def apply(W: Typ[O]) = FuncTyp[TT, V](tail, head(W))

    type DepTargetType = FuncLike[TT, D]

    type TargetType = Func[TT, T]

    def target(x: Typ[Cod]) = tail ->: head.target(x)

    def depTarget(xs: Func[O, Typ[Cod]]) = (fmly: Family) => {
      val a = tail.Var
      val b = fmly(a)
      val targfibre = lmbda(a)(headfibre(a).depTarget(xs)(b))
      PiTyp(targfibre)
    }

    //    type Cod = head.Cod


    def subs(x: Term, y: Term) =
      FuncIterPtnMap(
          tail, head.subs(x, y))


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
  case class DepFuncIterPtnMap[TT <: Term with Subs[TT],
                            V <: Term with Subs[V],
                            T <: Term with Subs[T],
                            D <: Term with Subs[D],
                            O <: Term with Subs[O],
                            C <: Term with Subs[C]](
      tail: Typ[TT],
      headfibre: TT => IterFuncPtnMap[O, C, V, T, D] ,
      headlevel: Int = 0
  )
      extends IterFuncPtnMap[O, C, FuncLike[TT, V], FuncLike[TT, T], FuncLike[TT, D]] {

    //    type Family =  FuncLike[Term, V]

//    type TargetType = FuncLike[TT, T]

    // type DepTargetType = FuncLike[Term, D]

    def apply(W: Typ[O]) = {
      val x = tail.Var
      val fiber = lmbda(x)(headfibre(x)(W))
      //   val fiber = typFamily(tail,  (t : Term) => headfibre(t)(W))
      PiTyp[TT, V](fiber)
    }

    //  type Cod = C

    def target(x: Typ[C]) = {
      val a = tail.Var
      val targfibre = lmbda(a)(headfibre(a).target(x))
      PiTyp(targfibre)
    }

      def depTarget(xs: Func[O, Typ[C]]) = (fmly: FuncLike[TT, V]) => {
        val a = tail.Var
        val b = fmly(a)
        val targfibre = lmbda(a)(headfibre(a).depTarget(xs)(b))
        PiTyp(targfibre)
      }

    def subs(x: Term, y: Term) =
      DepFuncIterPtnMap(tail.replace(x, y), (tt: TT) => headfibre(tt).subs(x, y))


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
}
