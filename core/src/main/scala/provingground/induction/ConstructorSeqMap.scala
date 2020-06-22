package provingground.induction

import provingground._, HoTT._
import shapeless.HList

/**
  * Inductive type definition as in [[ConstructorSeqTL]] together with the scala type of a codomain;
  * this determines the scala type of `rec_W,X` and `induc_W, Xs` functions.
  * methods return recursive and inductive definitions for a  given codomain; these  are lambda forms of definitions made
  * for each introduction rule in [[ConstructorPatternMap]] and combined for different introduction rules
  *  in [[RecursiveDefinition]] and [[InductiveDefinition]]
  *
  * @tparam C scala type of terms in the codomain for defining recursive/inductive function
  * @tparam H scala type of terms in the inductive type with this shape.
  * @tparam Intros introduction rules for inductive type of this shape
  * @tparam RecType scala type of a function `rec_W, X`
  * @tparam InducType scala type of a function `ind_W, X`
  */
sealed trait ConstructorSeqMap[C <: Term with Subs[C],
                               H <: Term with Subs[H],
                               RecType <: Term with Subs[RecType],
                               InducType <: Term with Subs[InducType],
                               Intros <: HList] {

  /**
    * recursive definition function `rec_W, X` in raw form
    */
  def recDefn(X: Typ[C]): RecursiveDefinition[H, C]

  /**
    * the inductive type being defined
    */
  val W: Typ[H]

  /**
    * converts a recursive definition built by [[RecursiveDefinition]] to a lambda
    */
  def recDataLambda(X: Typ[C]): Func[H, C] => RecType

  /**
    * the recursion function `rec_W,X`
    */
  def rec(X: Typ[C]): RecType =
    recDataLambda(X: Typ[C])(recDefn(X: Typ[C]))

  /**
    * inductive definition function `ind_W, X` in raw form
    */
  def inducDefn(fibre: Func[H, Typ[C]]): InductiveDefinition[H, C]

  /**
    * converts an inductive definition built by [[InductiveDefinition]] to a lambda
    */
  def inducDataLambda(fibre: Func[H, Typ[C]]): FuncLike[H, C] => InducType

  /**
    * inductive definition function `ind_W, Xs`
    */
  def induc(fibre: Func[H, Typ[C]]) =
    inducDataLambda(fibre)(inducDefn(fibre))

  def subs(x: Term,
           y: Term): ConstructorSeqMap[C, H, RecType, InducType, Intros]
}

object ConstructorSeqMap {
  import shapeless._

  /**
    * [[ConstructorSeqMap]] for an empty sequence of introduction rules
    */
  case class Empty[C <: Term with Subs[C], H <: Term with Subs[H]](W: Typ[H])
      extends ConstructorSeqMap[C, H, Func[H, C], FuncLike[H, C], HNil] {

    def recDefn(X: Typ[C]) = RecursiveDefinition.Empty(W, X)

    def recDataLambda(X: Typ[C]) = (f) => f

    def inducDefn(fibre: Func[H, Typ[C]]) =
      InductiveDefinition.Empty(fibre)

    def inducDataLambda(fibre: Func[H, Typ[C]]) = (f) => f

    def subs(x: Term, y: Term) = Empty(W.replace(x, y))

  }

  /**
    * formal symbol for definition data for a recursion function
    */
  case class RecDataSym[C <: Term with Subs[C]](cons: C) extends AnySym {
    def subs(x: Term, y: Term) = RecDataSym(cons.replace(x, y))
  }

  /**
    * formal symbol for definition data for an induction symbol
    */
  case class InducDataSym[C <: Term with Subs[C]](cons: C) extends AnySym {
    def subs(x: Term, y: Term) = InducDataSym(cons.replace(x, y))
  }

  case class ConsIndException[C <: Term with Subs[C],
                              H <: Term with Subs[H],
                              Cod <: Term with Subs[Cod],
                              RD <: Term with Subs[RD],
                              ID <: Term with Subs[ID],
                              TR <: Term with Subs[TR],
                              TI <: Term with Subs[TI],
                              TIntros <: HList](
      cons: Cons[C, H, Cod, RD, ID, TR, TI, TIntros],
      fibre: Func[H, Typ[Cod]],
      error: Throwable)
      extends Exception(error.getMessage)

  /**
    * prepending an introduction rule to [[ConstructorSeqMap]]
    *
    * @param cons the introduction rule
    * @param pattern the mapped version of the introduction rule for the given codomain
    * @param tail the tail [[ConstructorSeqMap]]
    */
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
      tail: ConstructorSeqMap[Cod, H, TR, TI, TIntros])
      extends ConstructorSeqMap[Cod,
                                H,
                                Func[RD, TR],
                                Func[ID, TI],
                                C :: TIntros] {

    def subs(x: Term, y: Term) =
      Cons(cons.replace(x, y), pattern.subs(x, y), tail.subs(x, y))

    val W = tail.W

    def data(X: Typ[Cod]): RD =
      pattern.recDataTyp(W, X).symbObj(RecDataSym(cons))

    val defn = (d: RD) => (f: Func[H, Cod]) => pattern.recDefCase(cons, d, f)

    import RecursiveDefinition.DataCons

    def dataCons(X: Typ[Cod]): DataCons[H, Cod, RD] =
      DataCons(
        data(X),
        defn,
        cons,
        tail.recDefn(X),
        (x: Term) =>
          (y: Term) =>
            (cod: Typ[Cod]) =>
              if (W.replace(x, y) == W && cod
                    .replace(x, y) == cod && subs(x, y) == this) None
              else Some(subs(x, y).dataCons(cod.replace(x, y)))
      )

    def recDefn(X: Typ[Cod]) = dataCons(X)
    // RecursiveDefinition.DataCons(
    //   data(X), defn, tail.recDefn(X))

    def recDataLambda(X: Typ[Cod]) =
      f => lmbda(data(X))(tail.recDataLambda(X)(f))

    def inducData(fibre: Func[H, Typ[Cod]]) =
      scala.util
        .Try(pattern.inducDataTyp(W, fibre)(cons).symbObj(InducDataSym(cons)))
        .fold(
          err => throw ConsIndException(this, fibre, err),
          identity
        )

    val inducDefn = (d: ID) =>
      (f: FuncLike[H, Cod]) => pattern.inducDefCase(cons, d, f)

    def indDataCons(
        fibre: Func[H, Typ[Cod]]): InductiveDefinition.DataCons[H, Cod, ID] =
      InductiveDefinition.DataCons(
        inducData(fibre),
        cons,
        inducDefn,
        tail.inducDefn(fibre),
        (x) =>
          (y) =>
            (fib) =>
              if (W.replace(x, y) == W && fib
                    .replace(x, y) == fib && subs(x, y) == this) {
                None
              } else Some(subs(x, y).indDataCons(fib.replace(x, y)))
      )

    def inducDefn(fibre: Func[H, Typ[Cod]]) = indDataCons(fibre)

    def inducDataLambda(fibre: Func[H, Typ[Cod]]) =
      (f) => lmbda(inducData(fibre))(tail.inducDataLambda(fibre)(f))
  }
}

import scala.language.existentials

/**
  * given scala type of the codomain and a specific inductive type, lifts a [[ConstructorSeqDom]] to a [[ConstructorSeqMap]]
  */
sealed trait ConstructorSeqMapper[SS <: HList,
                                  C <: Term with Subs[C],
                                  H <: Term with Subs[H],
                                  RecType <: Term with Subs[RecType],
                                  InducType <: Term with Subs[InducType],
                                  Intros <: HList] {

  /**
    * given scala type of the codomain and a specific inductive type, lifts a [[ConstructorSeqDom]] to a [[ConstructorSeqMap]]
    */
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
            ConstructorSeqMap.Cons(pattern.symbcons(name, W),
                                   patternMap,
                                   tailMap)
        }
    }
}

/**
  * the ``shape`` of the definition of an inductive type; when a type is specified we get an object of type
  * [[ConstructorSeqTL]], which is the full definition
  *
  *
  * @tparam SS ``shape sequence`` - a formal type for lifting information about introduction rules to type level.
  * @tparam H the scala type of terms in an inductive type of this shape
  * @tparam Intros the scala type of the introduction rules
  *
  *
  */
sealed trait ConstructorSeqDom[
    SS <: HList, H <: Term with Subs[H], Intros <: HList] {
  val introArgsVec: Vector[Int]

  val numIntros: Int

  /**
    * given a codomain, returns a `mapped` version, i.e. one with all the types needed for recursion and induction.
    */
  def mapped[C <: Term with Subs[C]](
      W: Typ[H]): ConstructorSeqMap[C, H, RecType, InducType, TIntros] forSome {
    type RecType <: Term with Subs[RecType];
    type InducType <: Term with Subs[InducType]; type TIntros <: HList
  }

  /**
    * existential typed `rec_W, X`, used by the method `recE` of [[ConstructorSeqTL]]
    */
  def rec[C <: Term with Subs[C]](W: Typ[H], X: Typ[C]) =
    mapped[C](W).rec(X)

  /**
    * existential typed `ind_W, X`, used by the method `indE` of [[ConstructorSeqTL]]
    */
  def induc[C <: Term with Subs[C]](W: Typ[H], Xs: Func[H, Typ[C]]) =
    mapped[C](W).induc(Xs)

  /**
    * returns introduction rules, given an inductive type.
    */
  def intros(typ: Typ[H]): Intros

  def subs(x: Term, y: Term): ConstructorSeqDom[SS, H, Intros]
}

object ConstructorSeqDom {
  import shapeless._

  implicit def consSeqDomSubst[SS <: HList,
                               H <: Term with Subs[H],
                               Intros <: HList]
    : Subst[ConstructorSeqDom[SS, H, Intros]] {} =
    new Subst[ConstructorSeqDom[SS, H, Intros]] {
      def subst(a: ConstructorSeqDom[SS, H, Intros])(x: Term, y: Term) =
        a.subs(x, y)
    }

  /**
    * Empty sequence of introduction rules
    */
  case class Empty[H <: Term with Subs[H]]()
      extends ConstructorSeqDom[HNil, H, HNil] {
    val introArgsVec: Vector[Int] = Vector()

    val numIntros: Int = 0

    def mapped[C <: Term with Subs[C]](W: Typ[H]) =
      ConstructorSeqMap.Empty[C, H](W)

    def intros(typ: Typ[H]) = HNil

    def subs(x: Term, y: Term) = this
  }

  object Empty {
    def byTyp[H <: Term with Subs[H]](typ: Typ[H]) = Empty[H]
  }

  /**
    * prepending an introduction rule, given `name` and `shape`.
    */
  case class Cons[TSS <: HList,
                  HShape <: HList,
                  H <: Term with Subs[H],
                  ConstructorType <: Term with Subs[ConstructorType],
                  TIntros <: HList](
      name: AnySym,
      pattern: ConstructorShape[HShape, H, ConstructorType],
      tail: ConstructorSeqDom[TSS, H, TIntros])
      extends ConstructorSeqDom[HShape :: TSS, H, ConstructorType :: TIntros] {
    val introArgsVec: Vector[Int] = pattern.introArgs +: tail.introArgsVec

    val numIntros: Int = 1 + tail.numIntros

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
      Cons(name.subs(x, y), pattern.subs(x, y), tail.subs(x, y))
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

  def newobj =
    throw new IllegalArgumentException(
      s"trying to use the constant $this as a variable (or a component of one)")

  def subs(x: Term, y: Term) = IndTyp(name, seqDom.subs(x, y))

  def variable(name: AnySym) = SymbObj(name, self)
}

/**
  * Essentially the definition of an inductive type; has all parameters of the definition:
  *
  * @param seqDom the sequence of introduction rules.
  * @param typ the inductive type being defined.
  * @tparam SS ``shape sequence`` - a formal type for lifting information about introduction rules to type level.
  * @tparam H the scala type of terms in the inductive type `typ`
  * @tparam Intros the scala type of the introduction rules
  *
  * We can define functions recursively using the [[rec]] method and inductively using the [[induc]] method.
  */
case class ConstructorSeqTL[SS <: HList,
                            H <: Term with Subs[H],
                            Intros <: HList](
    seqDom: ConstructorSeqDom[SS, H, Intros],
    typ: Typ[H]) {
  def subs(x: Term, y: Term) =
    ConstructorSeqTL(seqDom.subs(x, y), typ.replace(x, y))

  /**
    * Prepend to the sequence of introduction rules.
    */
  def |:[S <: HList, ConstructorType <: Term with Subs[ConstructorType]](
      head: ConstructorTL[S, H, ConstructorType]) =
    ConstructorSeqTL(ConstructorSeqDom.Cons(head.name, head.shape, seqDom), typ)

  /**
    * Existential typed version of [[rec]] for use at runtime if neccesary.
    */
  def recE[C <: Term with Subs[C]](X: Typ[C]) = seqDom.rec(typ, X)

  /**
    * returns the term `rec_W, X` for recursively defining function to `X` from the inductive type `W = typ`;
    * an implicit `mapper` is used, which also allows calculation of the type `RecType`.
    *
    * @param X the codomain to which we wish to define recursive functions - the only one we need to specify;
    * @tparam RecType type of the `rec` function returned;
    * @tparam InducType not used here, but part of the definition of the `mapper`.
    */
  def rec[C <: Term with Subs[C],
          RecType <: Term with Subs[RecType],
          InducType <: Term with Subs[InducType]](X: Typ[C])(
      implicit mapper: ConstructorSeqMapper[SS,
                                            C,
                                            H,
                                            RecType,
                                            InducType,
                                            Intros]) =
    mapper.mapped(seqDom)(typ).rec(X)

  /**
    * Existential typed version of [[induc]] to be used at runtime if necessary.
    */
  def inducE[C <: Term with Subs[C]](Xs: Func[H, Typ[C]]) =
    seqDom.induc(typ, Xs)

  /**
    * returns the term `ind_W, X` for inductively defining function to the type family `Xs` on W
    * from the inductive type `W = typ`;
    * an implicit `mapper` is used, which also allows calculation of the type `RecType`.
    *
    * @param X the codomain to which we wish to define recursive functions - the only one we need to specify;
    * @tparam RecType not used here, but part of the definition of the `mapper`.
    * @tparam InducType the type of the `induc` function returned.
    */
  def induc[C <: Term with Subs[C],
            RecType <: Term with Subs[RecType],
            InducType <: Term with Subs[InducType]](Xs: Func[H, Typ[C]])(
      implicit mapper: ConstructorSeqMapper[SS,
                                            C,
                                            H,
                                            RecType,
                                            InducType,
                                            Intros]) =
    mapper.mapped(seqDom)(typ).induc(Xs)

  /**
    * The introduction rules.
    */
  val intros = seqDom.intros(typ)
}

object ConstructorSeqTL {
  implicit def consSeqTLSubs[SS <: HList,
                             H <: Term with Subs[H],
                             Intros <: HList]
    : Subst[ConstructorSeqTL[SS, H, Intros]] {} =
    new Subst[ConstructorSeqTL[SS, H, Intros]] {
      def subst(a: ConstructorSeqTL[SS, H, Intros])(x: Term, y: Term) = {
        ConstructorSeqTL(a.seqDom.subs(x, y), a.typ.replace(x, y))
      }
    }

  def Empty[H <: Term with Subs[H]](W: Typ[H]) =
    ConstructorSeqTL(ConstructorSeqDom.Empty[H], W)

  /**
    * Wrapped existential version of [[ConstructorSeqTL]]
    */
  sealed trait Exst {
    type SS <: HList
    type Intros <: HList

    /**
      * the wrapped value
      */
    val value: ConstructorSeqTL[SS, Term, Intros]

    /**
      * prepend introduction rule
      */
    def |:[S <: HList, ConstructorType <: Term with Subs[ConstructorType]](
        head: ConstructorTL[S, Term, ConstructorType]) =
      Exst(head |: value)
  }

  object Exst {

    /**
      * helper for construction
      */
    def apply[SSS <: HList, IIntros <: HList](
        cs: ConstructorSeqTL[SSS, Term, IIntros]) =
      new Exst {
        type SS     = SSS
        type Intros = IIntros

        val value = cs
      }
  }

  /**
    * returns the wrapped existential form of [[ConstructorSeqTL]]
    */
  def getExst(w: Typ[Term], intros: Vector[Term]): Exst = intros match {
    case Vector() => Exst(Empty(w))
    case x +: ys =>
      val name = x.asInstanceOf[Symbolic].name
      val head = name ::: ConstructorTypTL.getExst(w, x.typ)
      head |: getExst(w, ys)
  }
}
