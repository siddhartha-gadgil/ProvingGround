package provingground.induction

import provingground._, HoTT._

import shapeless._

object SubstInstances {
  implicit def indConsShape[S <: HList,
                            H <: Term with Subs[H],
                            Fb <: Term with Subs[Fb],
                            ConstructorType <: Term with Subs[ConstructorType],
                            Index <: HList: TermList]
    : Subst[IndexedConstructorShape[S, H, Fb, ConstructorType, Index]] =
    new Subst[IndexedConstructorShape[S, H, Fb, ConstructorType, Index]] {
      def subst(a: IndexedConstructorShape[S, H, Fb, ConstructorType, Index])(
          x: Term,
          y: Term) =
        a.subs(x, y)
    }

  implicit def consShape[S <: HList,
                         H <: Term with Subs[H],
                         ConstructorType <: Term with Subs[ConstructorType]]
    : Subst[ConstructorShape[S, H, ConstructorType]] =
    new Subst[ConstructorShape[S, H, ConstructorType]] {
      def subst(a: ConstructorShape[S, H, ConstructorType])(
          x: Term,
          y: Term
      ) = a.subs(x, y)
    }

  implicit def typFmaily[H <: Term with Subs[H],
                         F <: Term with Subs[F],
                         Index <: HList: TermList]
    : Subst[TypFamilyPtn[H, F, Index]] = new Subst[TypFamilyPtn[H, F, Index]] {
    def subst(a: TypFamilyPtn[H, F, Index])(
        x: Term,
        y: Term
    ) = a.subs(x, y)
  }

  implicit def iterFunc[O <: Term with Subs[O], F <: Term with Subs[F]]
    : Subst[IterFuncShape[O, F]] =
    new Subst[IterFuncShape[O, F]] {
      def subst(a: IterFuncShape[O, F])(x: Term, y: Term) = a.subs(x, y)
    }

  implicit def indexedIterFunc[H <: Term with Subs[H],
                               F <: Term with Subs[F],
                               Fb <: Term with Subs[Fb],
                               Index <: HList: TermList]
    : Subst[IndexedIterFuncShape[H, F, Fb, Index]] =
    new Subst[IndexedIterFuncShape[H, F, Fb, Index]] {
      def subst(a: IndexedIterFuncShape[H, F, Fb, Index])(
          x: Term,
          y: Term
      ) = a.subs(x, y)
    }
}
