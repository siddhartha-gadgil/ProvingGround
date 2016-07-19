package provingground

import HoTT._

trait ConstructorSeq[C <: Term with Subs[C], H <: Term with Subs[H]] {
  def recCaseDefn(X: Typ[C]): RecursiveCaseDefinition[H, C]

  val W: Typ[H]

  type RecType <: Term with Subs[RecType]

  def recDataLambda(X: Typ[C]): Func[H, C] => RecType

  def rec(X: Typ[C]): RecType =
    recDataLambda(X: Typ[C])(recCaseDefn(X: Typ[C]))

  def inducCaseDefn(fibre: Func[H, Typ[C]]): InductiveCaseDefinition[H, C]

  type InducType <: Term with Subs[InducType]

  def inducDataLambda(fibre: Func[H, Typ[C]]): FuncLike[H, C] => InducType

  def induc(fibre: Func[H, Typ[C]]) =
    inducDataLambda(fibre)(inducCaseDefn(fibre))

  def ::(head: Constructor[C, H]) = ConstructorSeq.Cons(head, this)
}

object ConstructorSeq {
  case class Empty[C <: Term with Subs[C], H <: Term with Subs[H]](W: Typ[H])
      extends ConstructorSeq[C, H] {
    def recCaseDefn(X: Typ[C]) = RecursiveCaseDefinition.Empty(W, X)

    type RecType = Func[H, C]

    def recDataLambda(X: Typ[C]) = (f) => f

    def inducCaseDefn(fibre: Func[H, Typ[C]]) =
      InductiveCaseDefinition.Empty(fibre)

    type InducType = FuncLike[H, C]

    def inducDataLambda(fibre: Func[H, Typ[C]]) = (f) => f
  }

  case class Cons[C <: Term with Subs[C], H <: Term with Subs[H]](
      cons: Constructor[C, H],
      tail: ConstructorSeq[C, H]
  )
      extends ConstructorSeq[C, H] {

    val W = tail.W

    def data(X: Typ[C]): cons.pattern.RecDataType =
      cons.pattern.recDom(cons.W, X).symbObj(Constructor.RecSym(cons))

    val defn = (d: cons.pattern.RecDataType) =>
      (f: Func[H, C]) => cons.pattern.recDef(cons.cons, d, f)

    def recCaseDefn(X: Typ[C]) =
      RecursiveCaseDefinition.DataCons(data(X), defn, tail.recCaseDefn(X))

    type RecType = Func[cons.pattern.RecDataType, tail.RecType]

    def recDataLambda(X: Typ[C]) =
      f => lmbda(data(X))(tail.recDataLambda(X)(f))

    type InducType = Func[cons.pattern.InducDataType, tail.InducType]

    def inducData(fibre: Func[H, Typ[C]]) =
      cons.pattern
        .inducDom(W, fibre)(cons.cons)
        .symbObj(Constructor.InducSym(cons))

    val inducDefn = (d: cons.pattern.InducDataType) =>
      (f: FuncLike[H, C]) => cons.pattern.inducDef(cons.cons, d, f)

    def inducCaseDefn(fibre: Func[H, Typ[C]]) =
      InductiveCaseDefinition.DataCons(
          inducData(fibre), inducDefn, tail.inducCaseDefn(fibre))

    def inducDataLambda(fibre: Func[H, Typ[C]]) =
      (f) => lmbda(inducData(fibre))(tail.inducDataLambda(fibre)(f))
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
