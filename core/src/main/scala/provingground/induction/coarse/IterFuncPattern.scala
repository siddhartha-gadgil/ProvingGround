package provingground.induction.coarse

import provingground._, HoTT._
import math._

import scala.util.Try

object IterFuncPattern {
  sealed trait IterFuncPtn[
      O <: Term with Subs[O], C <: Term with Subs[C], F <: Term with Subs[F]] {

    /**
      * the universe containing the type
      */
    val univLevel: Int

    type Cod = C

    /**
      * scala type (upper bound) for a member of the family, i.e., sections
      */
    type Family = F

    /**
      * scala type of target for induced functions
      */
    type TargetType <: Term with Subs[TargetType]

    /**
      * Note that the target may be a type family, not a type,
      *  so this is the actual type of objects.
      */
    type DepTargetType <: Term with Subs[DepTargetType]

    /**
      * returns the type corresponding to the pattern, such as A -> W, given the (inductive) type W,
      *  this is used mainly for constructor patterns, with the W being fixed.
      */
    def apply(tp: Typ[O]): Typ[Family]

    /**
      * target scala type.
      */
    def target(x: Typ[Cod]): Typ[TargetType]

    /**
      * dependent target scala type.
      */
    def depTarget(xs: Func[O, Typ[Cod]]): Family => Typ[DepTargetType]

    def withCod[CC <: Term with Subs[CC]](w: Typ[O]): IterFuncPtn[O, CC, Family]

    def subs(x: Term, y: Term): IterFuncPtn[O, C, Family]

    /**
      * function induced by f: W -> X of type (A -> W) -> (A -> X) etc
      *
      * @param f function from which to induce
      *
      * @param W the inductive type
      *
      * @param X codomain of the given function
      */
    def induced(f: Func[O, Cod]): Family => TargetType

    /**
      * dependent function induced by dependent f: W -> X(s) of type (A -> W) -> ((a : A) ~> Xs(a)) etc
      *
      * @param f dependent function from which to induce
      *
      * @param W the inductive type
      *
      * @param Xs family of codomains of the given dependent function
      */
    def inducedDep(f: FuncLike[O, Cod]): Family => DepTargetType
  }

  object IterFuncPtn {
    def getOpt[O <: Term with Subs[O], F <: Term with Subs[F]](typ: Typ[O])(
        fmlyTyp: Typ[F]) =
      Try(get[O, Term, F](typ)(fmlyTyp)).toOption

    def get[O <: Term with Subs[O],
            C <: Term with Subs[C],
            F <: Term with Subs[F]](typ: Typ[O])(
        fmlyTyp: Typ[F]): IterFuncPtn[O, C, F] =
      fmlyTyp match {
        case `typ` => IdIterPtn[O, C]().asInstanceOf[IterFuncPtn[O, C, F]]
        case FuncTyp(dom: Typ[u], codom: Typ[v]) =>
          val head = get[O, C, v](typ)(codom)
          val tail = dom
          val headCast: IterFuncPtn[O, C, v] {
            type TargetType    = head.TargetType;
            type DepTargetType = head.DepTargetType;
          }               = head
          val funcIterPtn = FuncIterPtn(tail, headCast)
          funcIterPtn.asInstanceOf[IterFuncPtn[O, C, F]]
        case tp: GenFuncTyp[u, v] =>
          val fibre     = tp.fib
          val tail      = tp.domain
          val a         = tail.Var
          val headfibre = (x: u) => get[O, C, v](typ)(fibre(x))
          val newHead   = headfibre(tail.Var)
          type VV = newHead.Family

          type TTT = newHead.TargetType
          type DD  = newHead.DepTargetType
          val newHeadFibre = (t: u) =>
            (headfibre(t).asInstanceOf[IterFuncPtn[O, C, VV] {

              type TargetType = TTT; type DepTargetType = DD;
            }])
          DepFuncIterPtn(tail, newHeadFibre).asInstanceOf[IterFuncPtn[O, C, F]]
      }
  }

  case class IterFuncTyp[O <: Term with Subs[O],
                         C <: Term with Subs[C],
                         F <: Term with Subs[F]](pattern: IterFuncPtn[O, C, F],
                                                 typ: Typ[O]) {

    def ->:[TT <: Term with Subs[TT]](tail: Typ[TT]) =
      FuncIterPtn(tail, pattern)

    def ~>:[TT <: Term with Subs[TT]](tailVar: TT) = {
      val fibre = (t: Term) =>
        pattern
          .subs(tailVar, t)
          .asInstanceOf[IterFuncPtn[O, C, F] {
            type TargetType    = pattern.TargetType;
            type DepTargetType = pattern.DepTargetType
          }]
      DepFuncIterPtn(tailVar.typ, fibre)
    }
  }

  /**
    * The identity family
    */
  case class IdIterPtn[O <: Term with Subs[O], C <: Term with Subs[C]]()
      extends IterFuncPtn[O, C, O] {
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

    def withCod[CC <: Term with Subs[CC]](w: Typ[O]) = IdIterPtn[O, CC]()

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

  trait RecIterPtn[TT <: Term with Subs[TT],
                   V <: Term with Subs[V],
                   T <: Term with Subs[T],
                   D <: Term with Subs[D],
                   O <: Term with Subs[O],
                   C <: Term with Subs[C]]
      extends IterFuncPtn[O, C, FuncLike[TT, V]] {

    type TargetType <: FuncLike[TT, T] with Subs[TargetType]

    type DepTargetType = FuncLike[TT, D]

    val tail: Typ[TT]

    val headfibre: TT => IterFuncPtn[O, C, V] {
      type TargetType = T; type DepTargetType = D
    }

    def depTarget(xs: Func[O, Typ[Cod]]) = (fmly: Family) => {
      val a = tail.Var
      val b = fmly(a)
      // val targfibre = lmbda(a)(headfibre(a).depTarget(xs)(b))
      piDefn(a)(headfibre(a).depTarget(xs)(b))

    }
  }

  case class FuncIterPtn[TT <: Term with Subs[TT],
                         V <: Term with Subs[V],
                         T <: Term with Subs[T],
                         D <: Term with Subs[D],
                         O <: Term with Subs[O],
                         C <: Term with Subs[C]](tail: Typ[TT],
                                                 head: IterFuncPtn[O, C, V] {
                                                   type TargetType    = T;
                                                   type DepTargetType = D;
                                                 })
      extends IterFuncPtn[O, C, Func[TT, V]] { self =>
    def apply(W: Typ[O]) = FuncTyp[TT, V](tail, head(W))

    type DepTargetType = FuncLike[TT, D]

    type TargetType = Func[TT, T]

    def target(x: Typ[Cod]) = tail ->: head.target(x)

    def depTarget(xs: Func[O, Typ[Cod]]) = (fmly: Family) => {
      val a = tail.Var
      val b = fmly(a)
      // val targfibre = lmbda(a)(headfibre(a).depTarget(xs)(b))
      piDefn(a)(headfibre(a).depTarget(xs)(b))
    }

    //    type Cod = head.Cod

    def withCod[CC <: Term with Subs[CC]](w: Typ[O]) = {
      val newHead = head.withCod[CC](w)
      FuncIterPtn[TT,
                  newHead.Family,
                  newHead.TargetType,
                  newHead.DepTargetType,
                  O,
                  CC](tail, newHead)
    }

    def subs(x: Term, y: Term) = {
      val newHead = head.subs(x, y)
      FuncIterPtn[TT,
                  newHead.Family,
                  newHead.TargetType,
                  newHead.DepTargetType,
                  O,
                  C](tail, newHead)
    }

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
      lmbda(g)(lmbda(x)(head.induced(f)(g(x))))
    }

    /**
      * inductively defining the induced function.
      * maps (g : tail --> head(W)) to func : (t : tail) ~> head(Xs(t)) given (head(W) --> (t: tail) ~> head(Xs(t)))
      *
      */
    def inducedDep(f: FuncLike[O, Cod]): Family => DepTargetType = {
      val x = tail.Var
      val g = apply(f.dom).Var
      lambda(g)(lambda(x)(head.inducedDep(f)(g(x))))
    }
  }

  /**
    * Extending by a constant type A a family of type patterns depending on (a : A).
    *
    */
  case class DepFuncIterPtn[TT <: Term with Subs[TT],
                            V <: Term with Subs[V],
                            T <: Term with Subs[T],
                            D <: Term with Subs[D],
                            O <: Term with Subs[O],
                            C <: Term with Subs[C]](
      tail: Typ[TT],
      headfibre: TT => IterFuncPtn[O, C, V] {

        type TargetType = T; type DepTargetType = D;
      },
      headlevel: Int = 0)
      extends RecIterPtn[TT, V, T, D, O, C] {

    //    type Family =  FuncLike[Term, V]

    type TargetType = FuncLike[TT, T]

    // type DepTargetType = FuncLike[Term, D]

    def apply(W: Typ[O]) = {
      val x     = tail.Var
      val fiber = lmbda(x)(headfibre(x)(W))
      //   val fiber = typFamily(tail,  (t : Term) => headfibre(t)(W))
      piDefn(x)(headfibre(x)(W))
    }

    //  type Cod = C

    def target(x: Typ[Cod]) = {
      val a         = tail.Var
      val targfibre = lmbda(a)(headfibre(a).target(x))
      PiDefn(targfibre)
    }

    def withCod[CC <: Term with Subs[CC]](w: Typ[O]) = {
      val newHead = headfibre(tail.Var)
      type VV  = newHead.Family
      type TTT = newHead.TargetType
      type DD  = newHead.DepTargetType
      val newHeadFibre = (t: TT) =>
        (headfibre(t)
          .withCod[CC](w)
          .asInstanceOf[IterFuncPtn[O, CC, VV] {

            type TargetType = TTT; type DepTargetType = DD;
          }])
      DepFuncIterPtn[TT, VV, TTT, DD, O, CC](tail, newHeadFibre)
    }

    def subs(x: Term, y: Term) = {
      val newHead = headfibre(tail.Var)
      type VV  = newHead.Family
      type TTT = newHead.TargetType
      type DD  = newHead.DepTargetType
      val newHeadFibre = (t: TT) =>
        (headfibre(t)
          .subs(x, y)
          .asInstanceOf[IterFuncPtn[O, C, VV] {

            type TargetType = TTT; type DepTargetType = DD;
          }])
      DepFuncIterPtn[TT, VV, TTT, DD, O, C](tail, newHeadFibre)
    }

    //    val head = headfibre(tail.Var)

    //    type Family = FuncLike[Term, head.Family]

    def induced(f: Func[O, Cod]): Family => TargetType = {
      val x = tail.Var
      val g = apply(f.dom).Var
      lambda(g)(lambda(x)(headfibre(x).induced(f)(g(x))))
    }

    def inducedDep(f: FuncLike[O, Cod]): Family => DepTargetType = {
      val x = tail.Var
      val g = apply(f.dom).Var
      lambda(g)(lambda(x)(headfibre(x).inducedDep(f)(g(x))))
    }

    val univLevel = max(univlevel(tail.typ), headlevel)
  }
}
