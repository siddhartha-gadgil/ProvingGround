package provingground

import HoTT._

object Implicits {
  val Types = IdFmlyPtn[Term, Term]()

  implicit class ConstructorHead[H <: Term with Subs[H]](typ: Typ[H]) {
    def pair = ConstructorTyp(IdW[H], typ)
    def :::(name: AnySym) = name ::: pair

    def ->>:[T <: Term with Subs[T]](that: Typ[T]) = that ->>: pair

    def -->>:(that: Typ[H]) = that -->>: pair

    def -->>:[FF <: Term with Subs[FF]](that: IterFuncPattern.IterFuncTyp[H, Term, FF]) =
      that -->>: pair

    def ~>>:[T <: Term with Subs[T]](thatVar: H) = thatVar ~>>: pair
  }

  implicit class SymbFmly[C <: Term with Subs[C], F <: Term with Subs[F]](
      ptn: FmlyPtn[Term, C, F]) {
    import FmlyPtn.fmly

    import IndexedConstructorPatterns.emptySeq

    def :|:(name: AnySym) = emptySeq(ptn, fmly(ptn)(name))

    def :::(name: AnySym) = fmly(ptn)(name)

    def :|:(fm: F) = emptySeq(ptn, fm)
  }

  implicit class TypAsSeqHead[H <: Term with Subs[H]](W: Typ[H]) {
    def seq = ConstructorSeq.Empty[Term, H](W)

    def =:(head: Constructor[Term, H]) = ConstructorSeq.Cons(head, seq)

    def =::(typ: Typ[H]) = typ ||: seq

  }

  implicit class IterFuncTypHead[O <: Term with Subs[O]](typ: Typ[O]) {
    import IterFuncPattern._
    def pair = IterFuncTyp(IdIterPtn[O, Term], typ)

    def -|>:[TT <: Term with Subs[TT]](tail: Typ[TT]) =
      IterFuncTyp(tail ->: pair, typ)

    def ~|>:[TT <: Term with Subs[TT]](tailVar: TT) =
      IterFuncTyp(tailVar ~>: pair, typ)
  }

  implicit class IndTypFmly[C <: Term with Subs[C],
      H <: Term with Subs[H], F <: Term with Subs[F]](typFmlyPtn: FmlyPtn[H, C, F]){
        def >>(w: F) = (new IndexedConstructorPatterns(typFmlyPtn)).Family(w)
      }

  implicit class UnifAppln(func: Term){
    def of(arg: Term) = Unify.appln(func, arg).get
  }
  
}
