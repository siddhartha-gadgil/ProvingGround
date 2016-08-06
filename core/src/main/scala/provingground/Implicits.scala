package provingground

import HoTT._

object Implicits {
  implicit class ConstructorHead[H <: Term with Subs[H]](typ: Typ[H]) {
    def pair = ConstructorTyp(IdW[H], typ)
    def :::(name: AnySym) = name ::: pair

    def ->>:[T <: Term with Subs[T]](that: Typ[T]) = that ->>: pair

    def -->>:(that: Typ[H]) = that -->>: pair

    def ~>>:[T <: Term with Subs[T]](thatVar: H) = thatVar ~>>: pair
  }
 
  
  implicit class SymbFmly[C <: Term with Subs[C], F <: Term with Subs[F]](
      ptn: FmlyPtn[Term, C, F]){
    import FmlyPtn.fmly
    
    import IndexedConstructorPatterns.emptySeq
    
    def :|:(name: AnySym) = emptySeq(ptn, fmly(ptn)(name))
    
    def :::(name: AnySym) = fmly(ptn)(name)
    
    def :|:(fm: F) = emptySeq(ptn, fm)
  }

  
  implicit class TypAsSeqHead[H <: Term with Subs[H]](W: Typ[H]){
    def seq =ConstructorSeq. Empty[Term, H](W)
    
    def =:(head: Constructor[Term, H]) = ConstructorSeq.Cons(head, seq)
  }

}