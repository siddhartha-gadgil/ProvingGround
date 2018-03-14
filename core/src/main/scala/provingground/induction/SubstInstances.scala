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
}
