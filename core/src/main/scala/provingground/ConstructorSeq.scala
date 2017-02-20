package provingground

import HoTT._
import shapeless.HList
// import HList._

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

  def |:(head: Constructor[C, H]) = ConstructorSeq.Cons(head, this)

  val intros: List[Term]
}

object ConstructorSeq {
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
  ) extends ConstructorSeq[C, H] {

    val W = tail.W

    def data(X: Typ[C]): cons.pattern.RecDataType =
      cons.pattern.recDataTyp(cons.W, X).symbObj(Constructor.RecSym(cons))

    val defn = (d: cons.pattern.RecDataType) =>
      (f: Func[H, C]) => cons.pattern.recDefCase(cons.cons, d, f)

    def recDefn(X: Typ[C]) =
      RecursiveDefinition.DataCons(data(X), defn, tail.recDefn(X))

    type RecType = Func[cons.pattern.RecDataType, tail.RecType]

    def recDataLambda(X: Typ[C]) =
      f => LambdaFixed(data(X), tail.recDataLambda(X)(f))

    type InducType = Func[cons.pattern.InducDataType, tail.InducType]

    def inducData(fibre: Func[H, Typ[C]]) =
      cons.pattern
        .inducDataTyp(W, fibre)(cons.cons)
        .symbObj(Constructor.InducSym(cons))

    val inducDefn = (d: cons.pattern.InducDataType) =>
      (f: FuncLike[H, C]) => cons.pattern.inducDefCase(cons.cons, d, f)

    def inducDefn(fibre: Func[H, Typ[C]]) =
      InductiveDefinition.DataCons(inducData(fibre),
                                   inducDefn,
                                   tail.inducDefn(fibre))

    def inducDataLambda(fibre: Func[H, Typ[C]]) =
      (f) => LambdaFixed(inducData(fibre), tail.inducDataLambda(fibre)(f))

    val intros = cons.cons :: tail.intros
  }

  def fold[C <: Term with Subs[C], H <: Term with Subs[H]](
      W: Typ[H]): List[Constructor[C, H]] => ConstructorSeq[C, H] = {
    case List()       => ConstructorSeq.Empty(W)
    case head :: tail => ConstructorSeq.Cons(head, fold(W)(tail))
  }

  def recFn[C <: Term with Subs[C], H <: Term with Subs[H]](
      cs: List[Constructor[C, H]],
      W: Typ[H],
      X: Typ[C]) = fold(W)(cs).rec(X)

  def inducFn[C <: Term with Subs[C], H <: Term with Subs[H]](
      cs: List[Constructor[C, H]],
      W: Typ[H],
      Xs: Func[H, Typ[C]]) =
    fold(W)(cs).induc(Xs)
}

trait ConstructorSeqMap[C <: Term with Subs[C],
                        H <: Term with Subs[H],
                        RecType <: Term with Subs[RecType],
                        InducType <: Term with Subs[InducType],
                        Intros <: HList] {

  def recDefn(X: Typ[C]): RecursiveDefinition[H, C]

  val W: Typ[H]

  // type RecType <: Term with Subs[RecType]

  def recDataLambda(X: Typ[C]): Func[H, C] => RecType

  def rec(X: Typ[C]): RecType =
    recDataLambda(X: Typ[C])(recDefn(X: Typ[C]))

  def inducDefn(fibre: Func[H, Typ[C]]): InductiveDefinition[H, C]

  // type InducType <: Term with Subs[InducType]

  def inducDataLambda(fibre: Func[H, Typ[C]]): FuncLike[H, C] => InducType

  def induc(fibre: Func[H, Typ[C]]) =
    inducDataLambda(fibre)(inducDefn(fibre))
}

object ConstructorSeqMap {
  import shapeless._

  case class Empty[C <: Term with Subs[C], H <: Term with Subs[H]](W: Typ[H])
      extends ConstructorSeqMap[C, H, Func[H, C], FuncLike[H, C], HNil] {
    def recDefn(X: Typ[C]) = RecursiveDefinition.Empty(W, X)

    // type RecType = Func[H, C]

    def recDataLambda(X: Typ[C]) = (f) => f

    def inducDefn(fibre: Func[H, Typ[C]]) =
      InductiveDefinition.Empty(fibre)

    // type InducType = FuncLike[H, C]

    def inducDataLambda(fibre: Func[H, Typ[C]]) = (f) => f

    val intros = List()
  }

  case class RecSym[C <: Term with Subs[C]](cons: C) extends AnySym {
    def subs(x: Term, y: Term) = RecSym(cons.replace(x, y))
  }

  case class InducSym[C <: Term with Subs[C]](cons: C) extends AnySym {
    def subs(x: Term, y: Term) = InducSym(cons.replace(x, y))
  }

  object Cons {
    def sym[HS <: Term with Subs[HS],
            C <: Term with Subs[C],
            H <: Term with Subs[H],
            Cod <: Term with Subs[Cod],
            RD <: Term with Subs[RD],
            ID <: Term with Subs[ID],
            TR <: Term with Subs[TR],
            TI <: Term with Subs[TI],
            TIntros <: HList](
        name: AnySym,
        pattern: ConstructorPatternMap[HS, Cod, C, H, RD, ID],
        tail: ConstructorSeqMap[Cod, H, TR, TI, TIntros]
    ) = {
      val W    = tail.W
      val cons = pattern.symbcons(name, W)
      Cons(cons, pattern, tail)
    }
  }


  case class Cons[HS <: Term with Subs[HS],
                  C <: Term with Subs[C],
                  H <: Term with Subs[H],
                  Cod <: Term with Subs[Cod],
                  RD <: Term with Subs[RD],
                  ID <: Term with Subs[ID],
                  TR <: Term with Subs[TR],
                  TI <: Term with Subs[TI],
                  TIntros <: HList](
      cons: C,
      pattern: ConstructorPatternMap[HS, Cod, C, H, RD, ID],
      tail: ConstructorSeqMap[Cod, H, TR, TI, TIntros]
  ) extends ConstructorSeqMap[Cod,
                                H,
                                Func[RD, TR],
                                Func[ID, TI],
                                C :: TIntros] {

    val W = tail.W

    def data(X: Typ[Cod]): RD =
      pattern.recDataTyp(W, X).symbObj(RecSym(cons))

    val defn = (d: RD) => (f: Func[H, Cod]) => pattern.recDefCase(cons, d, f)

    def recDefn(X: Typ[Cod]) =
      RecursiveDefinition.DataCons(data(X), defn, tail.recDefn(X))

    // type RecType = Func[cons.pattern.RecDataType, tail.RecType]

    def recDataLambda(X: Typ[Cod]) =
      f => LambdaFixed(data(X), tail.recDataLambda(X)(f))

    // type InducType = Func[cons.pattern.InducDataType, tail.InducType]

    def inducData(fibre: Func[H, Typ[Cod]]) =
      pattern.inducDataTyp(W, fibre)(cons).symbObj(InducSym(cons))

    val inducDefn = (d: ID) =>
      (f: FuncLike[H, Cod]) => pattern.inducDefCase(cons, d, f)

    def inducDefn(fibre: Func[H, Typ[Cod]]) =
      InductiveDefinition.DataCons(inducData(fibre),
                                   inducDefn,
                                   tail.inducDefn(fibre))

    def inducDataLambda(fibre: Func[H, Typ[Cod]]) =
      (f) => LambdaFixed(inducData(fibre), tail.inducDataLambda(fibre)(f))
  }
}

import scala.language.existentials

trait ConstructorSeqDom[H <: Term with Subs[H]] {

  def mapped[C <: Term with Subs[C]](W: Typ[H])
    : ConstructorSeqMap[C, H, RecType, InducType, TIntros] forSome {
      type RecType <: Term with Subs[RecType];
      type InducType <: Term with Subs[InducType]; type TIntros <: HList
    }

  def rec[C <: Term with Subs[C]](W: Typ[H],
                                                          X: Typ[C]) =
    mapped[C](W).rec(X)

  def induc[C <: Term with Subs[C]](
      W: Typ[H],
      Xs: Func[H, Typ[C]]) =
    mapped[C](W).induc(Xs)

  def intros(typ: Typ[H]): List[Term]
}

object ConstructorSeqDom {
  case class Empty[H <: Term with Subs[H]]() extends ConstructorSeqDom[H] {
    def mapped[C <: Term with Subs[C]](W: Typ[H]) =
      ConstructorSeqMap.Empty[C, H](W)

    def intros(typ: Typ[H]) = List()
  }

  case class Cons[S <: Term with Subs[S], H <: Term with Subs[H], ConstructorType <: Term with Subs[ConstructorType]](name: AnySym,
                                          pattern: ConstructorShape[S, H, ConstructorType],
                                          tail: ConstructorSeqDom[H])
      extends ConstructorSeqDom[H] {
    def mapped[C <: Term with Subs[C]](W: Typ[H])
      : ConstructorSeqMap[C, H, RecType, InducType, TIntros] forSome {
        type RecType <: Term with Subs[RecType];
        type InducType <: Term with Subs[InducType]; type TIntros <: HList
      } = {
      val ptn = pattern.mapped[C]
      val tl  = tail.mapped[C](W)
      ConstructorSeqMap.Cons.sym(name, ptn, tl)
    }

    def intros(typ: Typ[H]) =
      pattern.symbcons(name, typ) :: tail.intros(typ)
  }
}

case class ConstructorSeqTL[H <: Term with Subs[H]](seqDom: ConstructorSeqDom[H],
                                                    typ: Typ[H]) {
  def |:[S <: Term with Subs[S], ConstructorType <: Term with Subs[ConstructorType]](head: ConstructorTL[S, H, ConstructorType]) =
    ConstructorSeqTL(ConstructorSeqDom.Cons(head.name, head.shape, seqDom),
                     typ)

  def rec[C <: Term with Subs[C]](X: Typ[C]) = seqDom.rec(typ, X)

  def induc[C <: Term with Subs[C]](Xs: Func[H, Typ[C]]) =
    seqDom.induc(typ, Xs)

  val intros = seqDom.intros(typ)
}

object ConstructorSeqTL {
  def Empty[H <: Term with Subs[H]](W: Typ[H]) =
    ConstructorSeqTL(ConstructorSeqDom.Empty[H], W)
}
