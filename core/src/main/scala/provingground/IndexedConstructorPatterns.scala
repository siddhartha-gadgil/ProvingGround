package provingground
import HoTT._
//import Families._

//import scala.language.implicitConversions
//import scala.util._
//import scala.language.existentials
import ConstructorPattern._

//import IterFuncPattern.{IterFuncPtn => FmlyPtn, _}

import FamilyPattern.FmlyPtn

import IterFuncPattern._

/**
 * @author gadgil
 */
class IndexedConstructorPatterns[
C <: Term with Subs[C], H <: Term with Subs[H]](
  val typFmlyPtn: FmlyPtn[H, C] /*{
    type FamilyType = F; type ArgType = Ind; type IterFunc = I; type IterTypFunc = IT; type IterDepFunc = DI }*/
) { outer =>

      type F = typFmlyPtn.FamilyType
      type Ind = typFmlyPtn.ArgType
      type I = typFmlyPtn.IterFunc
      type IT = typFmlyPtn.IterTypFunc
      type DI = typFmlyPtn.IterDepFunc


  type Cod = C
  import typFmlyPtn._
  sealed trait ConstructorPattern[Cnstr <: Term with Subs[Cnstr]] { self =>


    /**
     * argument for the final image
     */
    val index: Ind

    /**
     * type of a constructor for this pattern.
     */
    type ConstructorType = Cnstr

    def apply(tps: F): Typ[ConstructorType]

    /**
     * (scala) type of data for recursion corresponding to the single constructor
     */
    type RecDataType <: Term with Subs[RecDataType]

    type InducDataType <: Term with Subs[InducDataType]

    /**
     * domain containing the recursion data for the constructor, i.e., the HoTT type of recursion data.
     */
    def recDom(w: F, x: Typ[Cod]): Typ[RecDataType]

    def inducDom(w: F, xs: Func[H, Typ[Cod]])(cons: ConstructorType): Typ[InducDataType]
    /**
     * given a term, matches to see if this is the image of a given (quasi)-constructor.
     * returns simplification (wrapped in Some) if the term matches.
     * @param cons constructor, actually quasi-constructor, with which to match.
     * @param data definition data for the image of the constructor.
     * @param f the function being defined, to be applied recursively.
     */
    def recDef(cons: ConstructorType, data: RecDataType, f: => I): Total => Option[Cod]

    def inducDef(cons: ConstructorType, data: InducDataType, f: => DI): Total => Option[Cod]

    def inClass[CC <: Term with Subs[CC]](w: Typ[H])(that: IndexedConstructorPatterns[CC,  H]): that.ConstructorPattern[Cnstr]

    def codClass[CC <: Term with Subs[CC]](w: Typ[H]) = {
      val _codfmly = typFmlyPtn.withCod[CC](w)
      val codfmly = _codfmly.asInstanceOf[FmlyPtn[H, CC] {
        type FamilyType = _codfmly.FamilyType; type ArgType = Ind; type IterFunc = _codfmly.IterFunc;
        type IterTypFunc = _codfmly.IterTypFunc; type IterDepFunc = _codfmly.IterDepFunc
      }]
      new IndexedConstructorPatterns(codfmly)
    }

    def withCod[CC <: Term with Subs[CC]](w: Typ[H]) = {
      val thatClass = codClass[CC](w)
      inClass(w)(thatClass)
    }

  }

  case class iW(index: Ind) extends ConstructorPattern[H] {

    type RecDataType = Cod

    type InducDataType = Cod

    def apply(tps: F) = typFmlyPtn.contractType(tps)(index)

    def recDom(w: F, x: Typ[Cod]): Typ[RecDataType] = x

    def inducDom(w: F, xs: Func[H, Typ[Cod]])(cons: H) = xs(cons)

    def recDef(cons: ConstructorType, data: RecDataType, f: => I): Total => Option[Cod] = {
      case (t: Term) if t == cons => Some(data)
      case _ => None
    }

    def inducDef(cons: ConstructorType, data: InducDataType, f: => DI): Total => Option[Cod] = {
      case (t: Term) if t == cons => Some(data)
      case _ => None
    }

    def inClass[
    CC <: Term with Subs[CC]](w: Typ[H])(that: IndexedConstructorPatterns[CC, H]): that.ConstructorPattern[H] =
      that.iW(index.asInstanceOf[that.Ind])

  }

  /**
   * Functional extension of a type pattern
   */
  sealed trait RecursiveConstructorPattern[ArgT <: Term with Subs[ArgT], HeadT <: Term with Subs[HeadT], CT <: FuncLike[ArgT, HeadT] with Subs[CT]] extends ConstructorPattern[CT] { self =>
    /**
     * scala type of argument to constructor A -> ... (or A ~> ...)
     */
    type ArgType = ArgT

    // type Cod = Term

    /**
     * scala type of the head T for constructor A -> T
     * for Pi-Types, the head may have varying HoTT type but must have fixed scala type.
     */
    type HeadType = HeadT

    //  type ConstructorType =CT

    /**
     * (scala) type of recursive data for head.
     */
    type HeadRecDataType <: Term

    type HeadInducDataType <: Term

    /**
     * The head pattern, constant T for A -> T and T(a) for A ~> T(a)
     */
    val headfibre: ArgType => ConstructorPattern[HeadType] {
      type RecDataType = HeadRecDataType;
      type InducDataType = HeadInducDataType
    }

    /**
     * returns data for recursion to be passed on to the head given an argument (when matching with the construtor).
     */
    def headData(data: RecDataType, arg: ArgType, f: => I): HeadRecDataType

    def recDef(cons: ConstructorType, data: RecDataType, f: => I): Total => Option[Cod] = {
      t =>
        for (arg <- getArg(cons)(t); term <- headfibre(arg).recDef(cons(arg), headData(data, arg, f), f)(t)) yield term
    }

    def headInducData(data: InducDataType, arg: ArgType, f: => DI): HeadInducDataType

    def inducDef(cons: ConstructorType, data: InducDataType, f: => DI): Total => Option[Cod] = {
      t =>
        for (arg <- getArg(cons)(t); term <- headfibre(arg).inducDef(cons(arg), headInducData(data, arg, f), f)(t)) yield term
    }
  }

  case class FuncPtn[TF <: Term with Subs[TF], HC <: Term with Subs[HC]](
    tail: IterFuncPtn[H, C, TF],
    tailIndex: Ind,
    head: ConstructorPattern[HC]
  ) extends RecursiveConstructorPattern[TF, HC, Func[TF, HC]] {

    type Cod = C

    type HeadRecDataType = head.RecDataType

    type HeadInducDataType = head.InducDataType

    val index = head.index

    val _head: ConstructorPattern[HeadType] { type RecDataType = HeadRecDataType; type InducDataType = HeadInducDataType } = head

    val headfibre = (t: ArgType) => _head

    type RecDataType = Func[tail.Family, Func[tail.TargetType, head.RecDataType]]

    type InducDataType = FuncLike[tail.Family, Func[tail.DepTargetType, head.InducDataType]]

    def apply(tps: F) = {
      val w = typFmlyPtn.contractType(tps)(tailIndex)
      FuncTyp[ArgType, head.ConstructorType](tail(w), head(tps))
    }

    def recDom(tps: F, x: Typ[Cod]): Typ[RecDataType] = {
      val w = typFmlyPtn.contractType(tps)(tailIndex)
      tail(w) ->: tail.target(x) ->: head.recDom(tps, x)
    }

    def inducDom(tps: F, xs: Func[H, Typ[C]])(cons: ConstructorType): Typ[InducDataType] = {
      val w = typFmlyPtn.contractType(tps)(tailIndex)
      val a = tail(w).Var
      val headcons = cons(a)
      val fibre = lmbda(a)(tail.depTarget(xs)(a) ->: head.inducDom(tps, xs)(headcons))
      PiTyp(fibre)
    }

    def inClass[CC <: Term with Subs[CC]](w: Typ[H])(that: IndexedConstructorPatterns[CC,  H]): that.ConstructorPattern[ConstructorType] =
      that.FuncPtn(tail.withCod[CC](w), tailIndex.asInstanceOf[that.Ind], head.inClass(w)(that))

    def headData(data: RecDataType, arg: ArgType, f: => I): HeadRecDataType = {
      val g = typFmlyPtn.fill(f)(tailIndex)
      data(arg)(tail.induced(g)(arg))
    }

    def headInducData(data: InducDataType, arg: ArgType, f: => DI): HeadInducDataType = {
      val g = typFmlyPtn.depFill(f)(tailIndex)
      data(arg)(tail.inducedDep(g)(arg))
    }

  }

  case class CnstFncPtn[TT <: Term with Subs[TT], HC <: Term with Subs[HC]](
    tail: Typ[TT],
    tailIndex: Ind,
    head: ConstructorPattern[HC]
  ) extends RecursiveConstructorPattern[TT, HC, Func[TT, HC]] {

    type HeadRecDataType = head.RecDataType

    type HeadInducDataType = head.InducDataType

    val index = head.index

    val _head: ConstructorPattern[HC] {
      type ConstructorType = HeadType; type RecDataType = HeadRecDataType;
      type InducDataType = HeadInducDataType
    } = head

    val headfibre = (t: ArgType) => _head

    type RecDataType = Func[TT, head.RecDataType]

    type InducDataType = FuncLike[TT, head.InducDataType]

    def apply(tps: F) = {
      FuncTyp[ArgType, head.ConstructorType](tail, head(tps))
    }

    def recDom(tps: F, x: Typ[Cod]): Typ[RecDataType] = {
      //      val w = typFmlyPtn.contractType(tps)(tailIndex)
      tail ->: head.recDom(tps, x)
    }

    def inducDom(tps: F, xs: Func[H, Typ[Cod]])(cons: ConstructorType): Typ[InducDataType] = {
      val a = tail.Var
      val headcons = cons(a)
      val fibre = lmbda(a)(head.inducDom(tps, xs)(headcons))
      PiTyp(fibre)
    }

    def inClass[
    CC <: Term with Subs[CC]](w: Typ[H])(that: IndexedConstructorPatterns[CC, H]): that.ConstructorPattern[ConstructorType] =
      that.CnstFncPtn(tail, tailIndex.asInstanceOf[that.Ind], head.inClass(w)(that))

    def headData(data: RecDataType, arg: ArgType, f: => I): HeadRecDataType = {
      val g = typFmlyPtn.fill(f)(tailIndex)
      data(arg)
    }

    def headInducData(data: InducDataType, arg: ArgType, f: => DI): HeadInducDataType = {
      val g = typFmlyPtn.depFill(f)(tailIndex)
      data(arg)
    }

  }

  case class DepFuncPtn[U <: Term with Subs[U], V <: Term with Subs[V], VV <: Term with Subs[VV], W <: Term with Subs[W], TF <: Term with Subs[TF]](
    tail: IterFuncPtn[H, C, TF],
    tailIndex: Ind,
    index: Ind,
    headfibre: TF => (ConstructorPattern[U] { type RecDataType = V; type InducDataType = VV })
  ) extends RecursiveConstructorPattern[TF, U, FuncLike[TF, U]] {

    type Cod = C

    type HeadRecDataType = V

    type HeadInducDataType = VV

    type RecDataType = FuncLike[tail.Family, Func[tail.TargetType, V]]

    type InducDataType = FuncLike[tail.Family, Func[tail.DepTargetType, VV]]

    def apply(tps: F) = {
      val w = typFmlyPtn.contractType(tps)(tailIndex)
      val a = tail(w).Var
      val fiber = lmbda(a)(headfibre(a)(tps))
      PiTyp[ArgType, U](fiber)
    }

    def recDom(tps: F, x: Typ[Cod]): Typ[RecDataType] = {
      val w = typFmlyPtn.contractType(tps)(tailIndex)
      val a = tail(w).Var
      val fibre = lmbda(a)(tail.target(x) ->: headfibre(a).recDom(tps, x))
      PiTyp(fibre)
    }

    def inducDom(tps: F, xs: Func[H, Typ[Cod]])(cons: ConstructorType): Typ[InducDataType] = {
      val w = typFmlyPtn.contractType(tps)(tailIndex)
      val a = tail(w).Var
      val headcons = cons(a)
      val fibre = lmbda(a)(tail.depTarget(xs)(a) ->: headfibre(a).inducDom(tps, xs)(headcons))
      PiTyp(fibre)
    }

    def inClass[CC <: Term with Subs[CC]](w: Typ[H])(that: IndexedConstructorPatterns[CC,  H]): that.ConstructorPattern[ConstructorType] = {
      val eg = headfibre(tail(w).Var).inClass(w)(that)
      val thatheadfibre = (x: TF) => headfibre(x).inClass(w)(that).asInstanceOf[that.ConstructorPattern[U] { type RecDataType = eg.RecDataType; type InducDataType = eg.InducDataType }]
      that.DepFuncPtn(tail.withCod[CC](w), tailIndex.asInstanceOf[that.Ind], index.asInstanceOf[that.Ind], thatheadfibre)
    }

    def headData(data: RecDataType, arg: ArgType, f: => I): HeadRecDataType = {
      val g = typFmlyPtn.fill(f)(tailIndex)
      data(arg)(tail.induced(g)(arg))
    }

    def headInducData(data: InducDataType, arg: ArgType, f: => DI): HeadInducDataType = {
      val g = typFmlyPtn.depFill(f)(tailIndex)
      data(arg)(tail.inducedDep(g)(arg))
    }

  }

  case class CnstDepFuncPtn[TT <: Term with Subs[TT], U <: Term with Subs[U], V <: Term with Subs[V], VV <: Term with Subs[VV], W <: Term with Subs[W]](
    tail: Typ[TT],
    index: Ind,
    headfibre: TT => (ConstructorPattern[U] { type RecDataType = V; type InducDataType = VV })
  ) extends RecursiveConstructorPattern[TT, U, FuncLike[TT, U]] {

    type HeadRecDataType = V

    type HeadInducDataType = VV

    type RecDataType = FuncLike[TT, V]

    type InducDataType = FuncLike[TT, VV]

    def apply(tps: F) = {
      val a = tail.Var
      val fiber = lmbda(a)(headfibre(a)(tps))
      PiTyp[ArgType, U](fiber)
    }

    def recDom(tps: F, x: Typ[Cod]): Typ[RecDataType] = {
      val a = tail.Var
      val fibre = lmbda(a)(headfibre(a).recDom(tps, x))
      PiTyp(fibre)
    }

    def inducDom(tps: F, xs: Func[H, Typ[C]])(cons: ConstructorType): Typ[InducDataType] = {
      val a = tail.Var
      val headcons = cons(a)
      val fibre = lmbda(a)(headfibre(a).inducDom(tps, xs)(headcons))
      PiTyp(fibre)
    }

    def inClass[
    CC <: Term with Subs[CC]](w: Typ[H])(that: IndexedConstructorPatterns[CC, H]): that.ConstructorPattern[ConstructorType] = {
      val eg = headfibre(tail.Var).inClass(w)(that)
      val thatheadfibre = (x: TT) => headfibre(x).inClass(w)(that).asInstanceOf[that.ConstructorPattern[U] { type RecDataType = eg.RecDataType; type InducDataType = eg.InducDataType }]
      that.CnstDepFuncPtn(tail, index.asInstanceOf[that.Ind], thatheadfibre)
    }

    def headData(data: RecDataType, arg: ArgType, f: => I): HeadRecDataType = {
      data(arg)
    }

    def headInducData(data: InducDataType, arg: ArgType, f: => DI): HeadInducDataType = {
      data(arg)
    }

  }

  /**
   * Constructor for an inductive type, with given scala type and poly-pattern of this type.
   *
   * abstraction of ConstructorDefn mainly to allow different type parameters.
   */
  trait Constructor[Cnstr <: Term with Subs[Cnstr]] { self =>
    /**
     * scala type, especially (nested) functions
     */
    type ConstructorType = Cnstr

    //  type Cod <: Term with Subs[Cod]
    /**
     * constructor-pattern for the constructor
     */
    val pattern: ConstructorPattern[Cnstr]

    //    val typ: Typ[Term]

    /**
     * the constructor (function or constant) itself.
     */
    val cons: pattern.ConstructorType

    /**
     * the type for which this is a constructor
     */
    val W: F
  }

  object Constructor{
    case class RecSym[Cnstr <: Term with Subs[Cnstr]](cons: Constructor[Cnstr]) extends AnySym

    case class InducSym[Cnstr <: Term with Subs[Cnstr]](cons: Constructor[Cnstr]) extends AnySym

  }

  /**
   * a constructor given by its parameters.
   *
   * @param pattern poly-pattern for the constructor.
   *
   * @param cons constructor function.
   *
   * @tparam U scala type of polypattern.
   */
  case class ConstructorDefn[U <: Term with Subs[U]](
    pattern: ConstructorPattern[U],
    cons: U, W: F
  ) extends Constructor[U]

  sealed trait ConstructorSeq {
    def recCaseDefn(X: Typ[Cod]): RecursiveCaseDefinition[Total, Cod]

    val W: F

    type RecType <: Term with Subs[RecType]

    def recDataLambda(X: Typ[C]): I => RecType

    def rec(X: Typ[C]): RecType = recDataLambda(X: Typ[C])(curry(recCaseDefn(X: Typ[C])))

    type InducType <: Term with Subs[InducType]

    def inducCaseDefn(fibre: Func[H, Typ[Cod]]): InductiveCaseDefinition[Total, Cod]

    def inducDataLambda(fibre: Func[H, Typ[Cod]]) : DI => InducType

  }

  def totalFibre(fibre: Func[H, Typ[Cod]], W: F) : Func[Total, Typ[Cod]] = {
    val x = domTotal(W).Var
    lmbda(x)(fibre(value(x)))
  }

  object ConstructorSeq {
    case class Empty(W: F) extends ConstructorSeq {
      def recCaseDefn(X: Typ[C]) = RecursiveCaseDefinition.Empty(domTotal(W), X)

      type RecType = I

      def recDataLambda(X: Typ[C]) = (f) => f

      type InducType = DI

      def inducCaseDefn(fibre: Func[H, Typ[Cod]]) = InductiveCaseDefinition.Empty(totalFibre(fibre, W))

      def inducDataLambda(fibre: Func[H, Typ[C]]) = (f) => f
    }

  }

  case class Cons[Cnstr <: Term with Subs[Cnstr]](
    cons: Constructor[Cnstr], tail: ConstructorSeq
  ) extends ConstructorSeq {

    val W = tail.W

    def data(X: Typ[C]): cons.pattern.RecDataType = cons.pattern.recDom(cons.W, X).symbObj(Constructor.RecSym(cons))

    val defn = (d: cons.pattern.RecDataType) => (f: Func[Total, C]) => cons.pattern.recDef(cons.cons, d, curry(f))

    def recCaseDefn(X: Typ[C]) = RecursiveCaseDefinition.DataCons(data(X), defn, tail.recCaseDefn(X))

    type RecType = Func[cons.pattern.RecDataType, tail.RecType]

    def recDataLambda(X: Typ[C]) = f => lmbda(data(X))(tail.recDataLambda(X)(f))



    def inducData(fibre: Func[H, Typ[Cod]]) = cons.pattern.inducDom(W, fibre)(cons.cons).symbObj(Constructor.InducSym(cons))


    type InducType = Func[cons.pattern.InducDataType, tail.InducType]

    val inducDefn =
      (d: cons.pattern.InducDataType) =>
        (f: FuncLike[Total, C]) =>
          cons.pattern.inducDef(cons.cons, d, depCurry(f))


    def inducCaseDefn(fibre: Func[H, Typ[C]]) = {
      InductiveCaseDefinition.DataCons(inducData(fibre), inducDefn, tail.inducCaseDefn(fibre))
      }

    def inducDataLambda(fibre: Func[H, Typ[C]]) = (f: DI) => lmbda(inducData(fibre))(tail.inducDataLambda(fibre)(f))


}

}
