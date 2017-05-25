package provingground.induction

import provingground._, HoTT._
import shapeless.HList

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
    // def sym[C <: Term with Subs[C],
    //         H <: Term with Subs[H],
    //         Cod <: Term with Subs[Cod],
    //         RD <: Term with Subs[RD],
    //         ID <: Term with Subs[ID],
    //         TR <: Term with Subs[TR],
    //         TI <: Term with Subs[TI],
    //         TIntros <: HList](
    //     name: AnySym,
    //     pattern: ConstructorPatternMap[Cod, C, H, RD, ID],
    //     tail: ConstructorSeqMap[Cod, H, TR, TI, TIntros]
    // ) = {
    //   val W    = tail.W
    //   val cons = pattern.symbcons(name, W)
    //   Cons(cons, pattern, tail)
    // }
  }

  case class Cons[C <: Term with Subs[C],
                  H <: Term with Subs[H],
                  Cod <: Term with Subs[Cod],
                  RD <: Term with Subs[RD],
                  ID <: Term with Subs[ID],
                  TR <: Term with Subs[TR],
                  TI <: Term with Subs[TI],
                  TIntros <: HList](
      cons: C,
      pattern: ConstructorPatternMap[Cod, C, H, RD, ID],
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
      f => lmbda(data(X))(tail.recDataLambda(X)(f))

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
      (f) => lmbda(inducData(fibre))(tail.inducDataLambda(fibre)(f))
  }
}

import scala.language.existentials

trait ConstructorSeqMapper[SS <: HList,
                           C <: Term with Subs[C],
                           H <: Term with Subs[H],
                           RecType <: Term with Subs[RecType],
                           InducType <: Term with Subs[InducType],
                           Intros <: HList] {
  def mapped(seqdom: ConstructorSeqDom[SS, H, Intros])(
      W: Typ[H]): ConstructorSeqMap[C, H, RecType, InducType, Intros]
}

object ConstructorSeqMapper {
  import shapeless._

  implicit def empty[H <: Term with Subs[H], C <: Term with Subs[C]]
    : ConstructorSeqMapper[HNil, C, H, Func[H, C], FuncLike[H, C], HNil] =
    new ConstructorSeqMapper[HNil, C, H, Func[H, C], FuncLike[H, C], HNil] {
      def mapped(seqdom: ConstructorSeqDom[HNil, H, HNil])(W: Typ[H]) =
        ConstructorSeqMap.Empty[C, H](W)
    }

  implicit def cons[TSS <: HList,
                    HShape <: HList,
                    H <: Term with Subs[H],
                    Cod <: Term with Subs[Cod],
                    ConstructorType <: Term with Subs[ConstructorType],
                    TIntros <: HList,
                    RD <: Term with Subs[RD],
                    ID <: Term with Subs[ID],
                    TR <: Term with Subs[TR],
                    TI <: Term with Subs[TI]](
      implicit patternMapper: ConstructorPatternMapper[HShape,
                                                       Cod,
                                                       ConstructorType,
                                                       H,
                                                       RD,
                                                       ID],
      tailMapper: ConstructorSeqMapper[TSS, Cod, H, TR, TI, TIntros])
    : ConstructorSeqMapper[HShape :: TSS,
                           Cod,
                           H,
                           Func[RD, TR],
                           Func[ID, TI],
                           ConstructorType :: TIntros] =
    new ConstructorSeqMapper[HShape :: TSS,
                             Cod,
                             H,
                             Func[RD, TR],
                             Func[ID, TI],
                             ConstructorType :: TIntros] {
      def mapped(
          seqdom: ConstructorSeqDom[HShape :: TSS,
                                    H,
                                    ConstructorType :: TIntros])(W: Typ[H]) =
        seqdom match {
          case ConstructorSeqDom.Cons(name, pattern, tail) =>
            val patternMap = patternMapper.mapper(pattern)
            val tailMap    = tailMapper.mapped(tail)(W)
            ConstructorSeqMap.Cons(pattern.symbcons(name, W), patternMap, tailMap)
        }
    }
}

trait ConstructorSeqDom[SS <: HList, H <: Term with Subs[H], Intros <: HList] {

  def mapped[C <: Term with Subs[C]](W: Typ[H])
    : ConstructorSeqMap[C, H, RecType, InducType, TIntros] forSome {
      type RecType <: Term with Subs[RecType];
      type InducType <: Term with Subs[InducType]; type TIntros <: HList
    }

  def rec[C <: Term with Subs[C]](W: Typ[H], X: Typ[C]) =
    mapped[C](W).rec(X)

  def induc[C <: Term with Subs[C]](W: Typ[H], Xs: Func[H, Typ[C]]) =
    mapped[C](W).induc(Xs)

  def intros(typ: Typ[H]): Intros

  def subs(x: Term, y: Term): ConstructorSeqDom[SS, H, Intros]
}

object ConstructorSeqDom {
  import shapeless._

  case class Empty[H <: Term with Subs[H]]()
      extends ConstructorSeqDom[HNil, H, HNil] {
    def mapped[C <: Term with Subs[C]](W: Typ[H]) =
      ConstructorSeqMap.Empty[C, H](W)

    def intros(typ: Typ[H]) = HNil

    def subs(x: Term, y: Term) = this
  }

  case class Cons[TSS <: HList,
                  HShape <: HList,
                  H <: Term with Subs[H],
                  ConstructorType <: Term with Subs[ConstructorType],
                  TIntros <: HList](
      name: AnySym,
      pattern: ConstructorShape[HShape, H, ConstructorType],
      tail: ConstructorSeqDom[TSS, H, TIntros])
      extends ConstructorSeqDom[HShape :: TSS, H, ConstructorType :: TIntros] {
    def mapped[C <: Term with Subs[C]](W: Typ[H])
      : ConstructorSeqMap[C, H, RecType, InducType, TIntros] forSome {
        type RecType <: Term with Subs[RecType];
        type InducType <: Term with Subs[InducType]; type TIntros <: HList
      } = {
      val ptn = pattern.mapped[C]
      val tl  = tail.mapped[C](W)
      ConstructorSeqMap.Cons(pattern.symbcons(name, W), ptn, tl)
    }

    def intros(typ: Typ[H]) =
      pattern.symbcons(name, typ) :: tail.intros(typ)

    def subs(x: Term, y: Term) =
      Cons(name, pattern.subs(x, y), tail.subs(x, y))
  }
}

case class IndTyp[SS <: HList, Intros <: HList](
    name: String,
    seqDom: ConstructorSeqDom[SS, Term, Intros])
    extends Typ[Term] { self =>
  lazy val struct = ConstructorSeqTL(seqDom, self)

  val baseTyp = ConstructorSeqTL(seqDom, name :: Type)

  type Obj = Term

  val typ = Type

  def newobj = ??? // should not be using this as a variable

  def subs(x: Term, y: Term) = IndTyp(name, seqDom.subs(x, y))

  def variable(name: AnySym) = SymbObj(name, self)
}

case class ConstructorSeqTL[SS <: HList,
                            H <: Term with Subs[H],
                            Intros <: HList](
    seqDom: ConstructorSeqDom[SS, H, Intros],
    typ: Typ[H]) {
  def |:[S <: HList, ConstructorType <: Term with Subs[ConstructorType]](
      head: ConstructorTL[S, H, ConstructorType]) =
    ConstructorSeqTL(ConstructorSeqDom.Cons(head.name, head.shape, seqDom),
                     typ)

  def recE[C <: Term with Subs[C]](X: Typ[C]) = seqDom.rec(typ, X)

  def rec[C <: Term with Subs[C],
          RecType <: Term with Subs[RecType],
          InducType <: Term with Subs[InducType]](X: Typ[C])(
      implicit mapper: ConstructorSeqMapper[SS,
                                            C,
                                            H,
                                            RecType,
                                            InducType,
                                            Intros]
  ) = mapper.mapped(seqDom)(typ).rec(X)

  def inducE[C <: Term with Subs[C]](Xs: Func[H, Typ[C]]) =
    seqDom.induc(typ, Xs)

  def induc[C <: Term with Subs[C],
            RecType <: Term with Subs[RecType],
            InducType <: Term with Subs[InducType]](Xs: Func[H, Typ[C]])(
      implicit mapper: ConstructorSeqMapper[SS,
                                            C,
                                            H,
                                            RecType,
                                            InducType,
                                            Intros]
  ) = mapper.mapped(seqDom)(typ).induc(Xs)

  val intros = seqDom.intros(typ)
}

object ConstructorSeqTL {
  def Empty[H <: Term with Subs[H]](W: Typ[H]) =
    ConstructorSeqTL(ConstructorSeqDom.Empty[H], W)

  trait Exst {
    type SS <: HList
    type Intros <: HList

    val value: ConstructorSeqTL[SS, Term, Intros]

    def |:[S <: HList, ConstructorType <: Term with Subs[ConstructorType]](
        head: ConstructorTL[S, Term, ConstructorType]) =
      Exst(head |: value)
  }

  object Exst {
    def apply[SSS <: HList, IIntros <: HList](
        cs: ConstructorSeqTL[SSS, Term, IIntros]) =
      new Exst {
        type SS     = SSS
        type Intros = IIntros

        val value = cs
      }
  }

  def getExst(w: Typ[Term], intros: List[Term]): Exst = intros match {
    case List() => Exst(Empty(w))
    case x :: ys =>
      val name = x.asInstanceOf[Symbolic].name.toString
      val head = name ::: ConstructorTypTL.getExst(w, x.typ)
      head |: getExst(w, ys)
  }
}
