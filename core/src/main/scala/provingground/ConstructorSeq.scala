package provingground

import HoTT._

case class PartialConstructorSeq[C <: Term with Subs[C], H <: Term with Subs[H]](
    head: ConstructorTyp[C, H], tail: ConstructorSeq[Term, H]){
  def :::(name: AnySym) = name ::: head !: tail
  
  def ->>:[T <: Term with Subs[T]](that: Typ[T]) = PartialConstructorSeq(that ->>: head, tail)
    
  def -->>:(that: Typ[H]) = PartialConstructorSeq(that -->>: head,  tail)

  def ~>>:[T <: Term with Subs[T]](thatVar: H) = 
  {
    val newHead = thatVar ~>>: head
    PartialConstructorSeq(thatVar ~>>: head, tail)
  }
}

trait ConstructorSeq[C <: Term with Subs[C], H <: Term with Subs[H]] {
  def recDefn(X: Typ[C]): RecursiveDefinition[H, C]

  val W: Typ[H]

  type RecType <: Term with Subs[RecType]

  def recDataLambda(X: Typ[C]): Func[H, C] => RecType

  def rec(X: Typ[C]): RecType =
    recDataLambda(X: Typ[C])(recDefn(X: Typ[C]))

  def inducDefn(fibre: Func[H, Typ[C]]): InductiveDefinition[H, C]

  type InducType <: Term with Subs[InducType]

  def inducDataLambda(fibre: Func[H, Typ[C]]): FuncLike[H, C] => InducType

  def induc(fibre: Func[H, Typ[C]]) =
    inducDataLambda(fibre)(inducDefn(fibre))

  def !:(head: Constructor[C, H]) = ConstructorSeq.Cons(head, this)
  
  val intros: List[Term]
}

object ConstructorSeq {
  implicit class TypAsSeqHead[H <: Term with Subs[H]](W: Typ[H]){
    def seq = Empty[Term, H](W)
    
    def =:(head: Constructor[Term, H]) = ConstructorSeq.Cons(head, seq)
  }
  
  case class Empty[C <: Term with Subs[C], H <: Term with Subs[H]](W: Typ[H])
      extends ConstructorSeq[C, H] {
    def recDefn(X: Typ[C]) = RecursiveDefinition.Empty(W, X)

    type RecType = Func[H, C]

    def recDataLambda(X: Typ[C]) = (f) => f

    def inducDefn(fibre: Func[H, Typ[C]]) =
      InductiveDefinition.Empty(fibre)

    type InducType = FuncLike[H, C]

    def inducDataLambda(fibre: Func[H, Typ[C]]) = (f) => f
    
    val intros = List()
  }

  case class Cons[C <: Term with Subs[C], H <: Term with Subs[H]](
      cons: Constructor[C, H],
      tail: ConstructorSeq[C, H]
  )
      extends ConstructorSeq[C, H] {

    val W = tail.W

    def data(X: Typ[C]): cons.pattern.RecDataType =
      cons.pattern.recDataTyp(cons.W, X).symbObj(Constructor.RecSym(cons))

    val defn = (d: cons.pattern.RecDataType) =>
      (f: Func[H, C]) => cons.pattern.recDefCase(cons.cons, d, f)

    def recDefn(X: Typ[C]) =
      RecursiveDefinition.DataCons(data(X), defn, tail.recDefn(X))

    type RecType = Func[cons.pattern.RecDataType, tail.RecType]

    def recDataLambda(X: Typ[C]) =
      f => lmbda(data(X))(tail.recDataLambda(X)(f))

    type InducType = Func[cons.pattern.InducDataType, tail.InducType]

    def inducData(fibre: Func[H, Typ[C]]) =
      cons.pattern
        .inducDataTyp(W, fibre)(cons.cons)
        .symbObj(Constructor.InducSym(cons))

    val inducDefn = (d: cons.pattern.InducDataType) =>
      (f: FuncLike[H, C]) => cons.pattern.inducDefCase(cons.cons, d, f)

    def inducDefn(fibre: Func[H, Typ[C]]) =
      InductiveDefinition.DataCons(
          inducData(fibre), inducDefn, tail.inducDefn(fibre))

    def inducDataLambda(fibre: Func[H, Typ[C]]) =
      (f) => lmbda(inducData(fibre))(tail.inducDataLambda(fibre)(f))

    val intros = cons.cons :: tail.intros
  }

  def fold[C <: Term with Subs[C], H <: Term with Subs[H]](
      W: Typ[H]): List[Constructor[C, H]] => ConstructorSeq[C, H] = {
    case List() => ConstructorSeq.Empty(W)
    case head :: tail => ConstructorSeq.Cons(head, fold(W)(tail))
  }

  def recFn[C <: Term with Subs[C], H <: Term with Subs[H]](
      cs: List[Constructor[C, H]], W: Typ[H], X: Typ[C]) = fold(W)(cs).rec(X)

  def inducFn[C <: Term with Subs[C], H <: Term with Subs[H]](
      cs: List[Constructor[C, H]], W: Typ[H], Xs: Func[H, Typ[C]]) =
    fold(W)(cs).induc(Xs)
}
