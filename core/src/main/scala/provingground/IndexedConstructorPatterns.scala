package provingground
import HoTT._
//import Families._

//import scala.language.implicitConversions
//import scala.util._
import scala.language.existentials

//import IterFuncPattern.{IterFuncPtn => FmlyPtn, _}

import IterFuncPattern._

//import scala.util.Try

/**
  * @author gadgil
  */
class IndexedConstructorPatterns[C <: Term with Subs[C],
    H <: Term with Subs[H], F <: Term with Subs[F]](
    val typFmlyPtn: FmlyPtn[H, C, F]) { outer =>

  def totalArg(typ: Term, fmly: F, accum: Term = Star): Term =
    typ match {
      case `fmly` => accum
      case FormalAppln(func: Term, arg: Term) =>
        totalArg(func, fmly, mkPair(arg, accum))
    }

  def getTotalArg(typ: Typ[Term], fmly: F) =
    totalArg(typ, fmly).asInstanceOf[typFmlyPtn.ArgType]

  import typFmlyPtn.{incl, Total, FamilyType, curry, depCurry, domTotal, value, ArgType, IterFunc, IterTypFunc, IterDepFunc, fill, depFill}
  //    type F = typFmlyPtn.FamilyType
  type Ind = ArgType
  type I = IterFunc
  type IT = IterTypFunc
  type DI = IterDepFunc

  type Cod = C

  sealed trait iConstructorPattern[Cnstr <: Term with Subs[Cnstr]] { self =>

    /**
      * argument for the final image
      */
    val index: Ind

    /**
      * type of a constructor for this pattern.
      */
    type iConstructorType = Cnstr

    def apply(tps: F): Typ[iConstructorType]

    /**
      * (scala) type of data for recursion corresponding to the single constructor
      */
    type RecDataType <: Term with Subs[RecDataType]

    type InducDataType <: Term with Subs[InducDataType]

    /**
      * domain containing the recursion data for the constructor, i.e., the HoTT type of recursion data.
      */
    def recDataTyp(w: F, x: Typ[Cod]): Typ[RecDataType]

    def inducDataTyp(w: F, xs: Func[Total, Typ[Cod]])(
        // xs should have domain Total
        cons: iConstructorType): Typ[InducDataType]

    /**
      * given a term, matches to see if this is the image of a given (quasi)-constructor.
      * returns simplification (wrapped in Some) if the term matches.
      * @param cons constructor, actually quasi-constructor, with which to match.
      * @param data definition data for the image of the constructor.
      * @param f the function being defined, to be applied recursively.
      */
    def recDefCase(cons: iConstructorType,
                   data: RecDataType,
                   f: => I): Total => Option[Cod]

    def inducDefCase(cons: iConstructorType,
                     data: InducDataType,
                     f: => DI): Total => Option[Cod]

    def inClass[CC <: Term with Subs[CC]](
        w: Typ[H])(that: IndexedConstructorPatterns[CC, H, F])
      : that.iConstructorPattern[Cnstr]

    def subs(x: Term, y: Term): iConstructorPattern[Cnstr]

    def codClass[CC <: Term with Subs[CC]](w: Typ[H]) = {
      val _codfmly = typFmlyPtn.withCod[CC](w)
      val codfmly = _codfmly.asInstanceOf[FmlyPtn[H, CC, F] {
            type ArgType = Ind; type IterFunc = _codfmly.IterFunc;
            type IterTypFunc = _codfmly.IterTypFunc;
            type IterDepFunc = _codfmly.IterDepFunc
          }]
      new IndexedConstructorPatterns(codfmly)
    }

    def withCod[CC <: Term with Subs[CC]](w: Typ[H]) = {
      val thatClass = codClass[CC](w)
      inClass(w)(thatClass)
    }

    def -->:(thatInd: (IterFuncPtn[H, Cod, F], Ind)) =
      FuncPtn(thatInd._1, thatInd._2, this)

    def funcFrom[T <: Term with Subs[T]](tail: Typ[T], fmly: FamilyType) =
      CnstFncPtn(tail, this)

    def piOf[T <: Term with Subs[T]](tailVar: T, fmly: FamilyType) = {
      val fibre = (t: Term) =>
        this
          .subs(tailVar, t)
          .asInstanceOf[iConstructorPattern[Cnstr] {
                type RecDataType = self.RecDataType;
                type InducDataType = self.InducDataType
              }]
      CnstDepFuncPtn(tailVar.typ, fibre)
    }

    /**
      * constructor for this pattern given inductive type and name.
      */
    def constructor(tp: => F, name: AnySym) = {
      val cons = apply(tp).symbObj(name)
      iConstructorDefn(this, cons, tp)
    }
  }

  /**
    * iConstructor pattern with type, for convenient building.
    */
  case class iConstructorTyp[Cnstr <: Term with Subs[Cnstr]](
      pattern: iConstructorPattern[Cnstr],
//      typ: Typ[H],
      fmly: FamilyType
  ) {
    def :::(name: AnySym) = pattern.constructor(fmly, name)

//    def arg = getTotalArg(typ, fmly)

    def -->>:[FF <: Term with Subs[FF]](that: IterFuncTyp[H, C, FF]) = {
      val arg = getTotalArg(that.typ, fmly)
      iConstructorTyp(FuncPtn(that.pattern, arg, pattern), fmly)
    }

    def -->>:(that: Typ[H]) = {
      val arg = getTotalArg(that, fmly)
      val tail = IdIterPtn[H, C]()
      val ptn = FuncPtn(tail, arg, pattern)
      iConstructorTyp(ptn, fmly)
    }

    def ->>:[T <: Term with Subs[T]](that: Typ[T]) = {
      assert(
          !(that.dependsOn(fmly)),
          "the method ->: is for extension by constant types, maybe you mean _-->:_")
      iConstructorTyp(pattern.funcFrom(that, fmly), fmly)
    }

    def ~>>:[T <: Term with Subs[T]](thatVar: T) =
      iConstructorTyp(pattern piOf (thatVar, fmly), fmly) //FIXME type should change as variable created.
  }

  object iConstructorTyp {
    implicit class iConstructorHead(typAndFmly: (Typ[H], FamilyType)) {
      val typ = typAndFmly._1
      val fmly = typAndFmly._2

      def arg = getTotalArg(typ, fmly)

      def pair = iConstructorTyp(iW(arg), fmly)
      def :::(name: AnySym): iConstructor = name ::: pair

      def ->>:[T <: Term with Subs[T]](that: Typ[T]) = that ->>: pair

      def -->>:(that: Typ[H]) = that -->>: pair

      def ~>>:[T <: Term with Subs[T]](thatVar: T) = thatVar ~>>: pair
    }
  }

  case class iW(index: Ind) extends iConstructorPattern[H] {

    type RecDataType = Cod

    type InducDataType = Cod

    def apply(tps: F) = typFmlyPtn.contractType(tps)(index)

    def recDataTyp(w: F, x: Typ[Cod]): Typ[RecDataType] = x

    def inducDataTyp(w: F, xs: Func[Total, Typ[Cod]])(cons: H) =
      xs(incl(cons, index, w)) //FIXME
    // xs should have domain Total, we should take the total value of cons

    def recDefCase(cons: iConstructorType,
                   data: RecDataType,
                   f: => I): Total => Option[Cod] = (t) => {
      if (typFmlyPtn.value(t) == cons) Some(data)
      else None
    }

    def inducDefCase(cons: iConstructorType,
                     data: InducDataType,
                     f: => DI): Total => Option[Cod] = (t) => {
      if (typFmlyPtn.value(t) == cons) Some(data)
      else None
    }

    def inClass[CC <: Term with Subs[CC]](
        w: Typ[H])(that: IndexedConstructorPatterns[CC, H, F])
      : that.iConstructorPattern[H] =
      that.iW(index.asInstanceOf[that.Ind])

    def subs(x: Term, y: Term) = iW(index.replace(x, y))
  }

  /**
    * Functional extension of a type pattern
    */
  sealed trait RecursiveiConstructorPattern[
      ArgT <: Term with Subs[ArgT],
      HeadT <: Term with Subs[HeadT],
      CT <: FuncLike[ArgT, HeadT] with Subs[CT]]
      extends iConstructorPattern[CT] { self =>

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
    val headfibre: ArgType => iConstructorPattern[HeadType] {
      type RecDataType = HeadRecDataType;
      type InducDataType = HeadInducDataType
    }

    /**
      * returns data for recursion to be passed on to the head given an argument (when matching with the construtor).
      */
    def headData(data: RecDataType, arg: ArgType, f: => I): HeadRecDataType

    def recDefCase(cons: iConstructorType,
                   data: RecDataType,
                   f: => I): Total => Option[Cod] = tt => {
      val t = typFmlyPtn.value(tt)
      for (arg <- getArg(cons)(t);
           term <- headfibre(arg).recDefCase(
                      cons(arg), headData(data, arg, f), f)(tt)) yield term
    }

    def headInducData(
        data: InducDataType, arg: ArgType, f: => DI): HeadInducDataType

    def inducDefCase(cons: iConstructorType,
                     data: InducDataType,
                     f: => DI): Total => Option[Cod] = tt => {
      val t = typFmlyPtn.value(tt)
      for (arg <- getArg(cons)(t);
           term <- headfibre(arg).inducDefCase(
                      cons(arg), headInducData(data, arg, f), f)(tt)) yield
        term
    }
  }

  case class FuncPtn[TF <: Term with Subs[TF], HC <: Term with Subs[HC]](
      tail: IterFuncPtn[H, C, TF],
      tailIndex: Ind,
      head: iConstructorPattern[HC]
  )
      extends RecursiveiConstructorPattern[TF, HC, Func[TF, HC]] {

    type Cod = C

    type HeadRecDataType = head.RecDataType

    type HeadInducDataType = head.InducDataType

    val index = head.index

    val _head: iConstructorPattern[HeadType] {
      type RecDataType = HeadRecDataType;
      type InducDataType = HeadInducDataType
    } = head

    val headfibre = (t: ArgType) => _head

    type RecDataType =
      Func[tail.Family, Func[tail.TargetType, head.RecDataType]]

    type InducDataType =
      FuncLike[tail.Family, Func[tail.DepTargetType, head.InducDataType]]

    def apply(tps: F) = {
      val w = typFmlyPtn.contractType(tps)(tailIndex)
      FuncTyp[ArgType, head.iConstructorType](tail(w), head(tps))
    }

    def recDataTyp(tps: F, x: Typ[Cod]): Typ[RecDataType] = {
      val w = typFmlyPtn.contractType(tps)(tailIndex)
      tail(w) ->: tail.target(x) ->: head.recDataTyp(tps, x)
    }

    def inducDataTyp(tps: F, xs: Func[Total, Typ[C]])(
        cons: iConstructorType): Typ[InducDataType] = {
      val w = typFmlyPtn.contractType(tps)(tailIndex)
      val a = tail(w).Var
      val headcons = cons(a)
      val x = w.Var
      val xss = x :-> xs(incl(x, tailIndex, tps))
      val fibre = lmbda(a)(
          tail.depTarget(xss)(a) ->: head.inducDataTyp(tps, xs)(headcons))
      PiTyp(fibre)
    }

    def inClass[CC <: Term with Subs[CC]](
        w: Typ[H])(that: IndexedConstructorPatterns[CC, H, F])
      : that.iConstructorPattern[iConstructorType] =
      that.FuncPtn(tail.withCod[CC](w),
                   tailIndex.asInstanceOf[that.Ind],
                   head.inClass(w)(that))

    def subs(x: Term, y: Term) =
      FuncPtn(tail.subs(x, y), tailIndex.subs(x, y), head.subs(x, y))

    def headData(data: RecDataType, arg: ArgType, f: => I): HeadRecDataType = {
      val g = fill(f)(tailIndex)
      data(arg)(tail.induced(g)(arg))
    }

    def headInducData(
        data: InducDataType, arg: ArgType, f: => DI): HeadInducDataType = {
      val g = depFill(f)(tailIndex)
      data(arg)(tail.inducedDep(g)(arg))
    }
  }

  case class CnstFncPtn[TT <: Term with Subs[TT], HC <: Term with Subs[HC]](
      tail: Typ[TT],
      //    tailIndex: Ind,
      head: iConstructorPattern[HC]
  )
      extends RecursiveiConstructorPattern[TT, HC, Func[TT, HC]] {

    type HeadRecDataType = head.RecDataType

    type HeadInducDataType = head.InducDataType

    val index = head.index

    val _head: iConstructorPattern[HC] {
      type iConstructorType = HeadType; type RecDataType = HeadRecDataType;
      type InducDataType = HeadInducDataType
    } = head

    val headfibre = (t: ArgType) => _head

    type RecDataType = Func[TT, head.RecDataType]

    type InducDataType = FuncLike[TT, head.InducDataType]

    def apply(tps: F) = {
      FuncTyp[ArgType, head.iConstructorType](tail, head(tps))
    }

    def recDataTyp(tps: F, x: Typ[Cod]): Typ[RecDataType] = {
      //      val w = typFmlyPtn.contractType(tps)(tailIndex)
      tail ->: head.recDataTyp(tps, x)
    }

    def inducDataTyp(tps: F, xs: Func[Total, Typ[Cod]])(
        cons: iConstructorType): Typ[InducDataType] = {
      val a = tail.Var
      val headcons = cons(a)
      val fibre = lmbda(a)(head.inducDataTyp(tps, xs)(headcons))
      PiTyp(fibre)
    }

    def inClass[CC <: Term with Subs[CC]](
        w: Typ[H])(that: IndexedConstructorPatterns[CC, H, F])
      : that.iConstructorPattern[iConstructorType] =
      that.CnstFncPtn(tail, head.inClass(w)(that))

    def subs(x: Term, y: Term) =
      CnstFncPtn(tail.subs(x, y), head.subs(x, y))

    def headData(data: RecDataType, arg: ArgType, f: => I): HeadRecDataType = {
      val g = fill(f)(head.index)
      data(arg)
    }

    def headInducData(
        data: InducDataType, arg: ArgType, f: => DI): HeadInducDataType = {
      val g = depFill(f)(head.index)
      data(arg)
    }
  }

  case class DepFuncPtn[U <: Term with Subs[U],
                        V <: Term with Subs[V],
                        VV <: Term with Subs[VV],
                        W <: Term with Subs[W],
                        TF <: Term with Subs[TF]](
      tail: IterFuncPtn[H, C, TF],
      tailIndex: Ind,
      index: Ind,
      headfibre: TF => (iConstructorPattern[U] {
        type RecDataType = V; type InducDataType = VV
      })
  )
      extends RecursiveiConstructorPattern[TF, U, FuncLike[TF, U]] { self =>

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

    def recDataTyp(tps: F, x: Typ[Cod]): Typ[RecDataType] = {
      val w = typFmlyPtn.contractType(tps)(tailIndex)
      val a = tail(w).Var
      val fibre = lmbda(a)(tail.target(x) ->: headfibre(a).recDataTyp(tps, x))
      PiTyp(fibre)
    }

    def inducDataTyp(tps: F, xs: Func[Total, Typ[Cod]])(
        cons: iConstructorType): Typ[InducDataType] = {
      val w = typFmlyPtn.contractType(tps)(tailIndex)
      val a = tail(w).Var
      val headcons = cons(a)
      val x = w.Var
      val xss = x :-> xs(incl(x, tailIndex, tps))
      val fibre = lmbda(a)(tail.depTarget(xss)(a) ->: headfibre(a)
            .inducDataTyp(tps, xs)(headcons))
      PiTyp(fibre)
    }

    def inClass[CC <: Term with Subs[CC]](
        w: Typ[H])(that: IndexedConstructorPatterns[CC, H, F])
      : that.iConstructorPattern[iConstructorType] = {
      val eg = headfibre(tail(w).Var).inClass(w)(that)
      val thatheadfibre = (x: TF) =>
        headfibre(x)
          .inClass(w)(that)
          .asInstanceOf[that.iConstructorPattern[U] {
                type RecDataType = eg.RecDataType;
                type InducDataType = eg.InducDataType
              }]
      that.DepFuncPtn(tail.withCod[CC](w),
                      tailIndex.asInstanceOf[that.Ind],
                      index.asInstanceOf[that.Ind],
                      thatheadfibre)
    }

    def subs(x: Term, y: Term) = {
      val thatheadfibre = (z: TF) =>
        headfibre(z)
          .subs(x, y)
          .asInstanceOf[iConstructorPattern[U] {
                type RecDataType = self.RecDataType;
                type InducDataType = self.InducDataType
              }]
      DepFuncPtn(tail.subs(x, y),
                 tailIndex.subs(x, y),
                 index.subs(x, y),
                 thatheadfibre)
    }

    def headData(data: RecDataType, arg: ArgType, f: => I): HeadRecDataType = {
      val g = fill(f)(tailIndex)
      data(arg)(tail.induced(g)(arg))
    }

    def headInducData(
        data: InducDataType, arg: ArgType, f: => DI): HeadInducDataType = {
      val g = depFill(f)(tailIndex)
      data(arg)(tail.inducedDep(g)(arg))
    }
  }

  case class CnstDepFuncPtn[TT <: Term with Subs[TT],
                            U <: Term with Subs[U],
                            V <: Term with Subs[V],
                            VV <: Term with Subs[VV],
                            W <: Term with Subs[W]](
      tail: Typ[TT],
      //    index: Ind,
      headfibre: TT => (iConstructorPattern[U] {
        type RecDataType = V; type InducDataType = VV
      })
  )
      extends RecursiveiConstructorPattern[TT, U, FuncLike[TT, U]] { self =>

    lazy val index = headfibre(tail.Var).index

    type HeadRecDataType = V

    type HeadInducDataType = VV

    type RecDataType = FuncLike[TT, V]

    type InducDataType = FuncLike[TT, VV]

    def apply(tps: F) = {
      val a = tail.Var
      val fiber = lmbda(a)(headfibre(a)(tps))
      PiTyp[ArgType, U](fiber)
    }

    def recDataTyp(tps: F, x: Typ[Cod]): Typ[RecDataType] = {
      val a = tail.Var
      val fibre = lmbda(a)(headfibre(a).recDataTyp(tps, x))
      PiTyp(fibre)
    }

    def inducDataTyp(tps: F, xs: Func[Total, Typ[C]])(
        cons: iConstructorType): Typ[InducDataType] = {
      val a = tail.Var
      val headcons = cons(a)
      val fibre = lmbda(a)(headfibre(a).inducDataTyp(tps, xs)(headcons))
      PiTyp(fibre)
    }

    def inClass[CC <: Term with Subs[CC]](
        w: Typ[H])(that: IndexedConstructorPatterns[CC, H, F])
      : that.iConstructorPattern[iConstructorType] = {
      val eg = headfibre(tail.Var).inClass(w)(that)
      val thatheadfibre = (x: TT) =>
        headfibre(x)
          .inClass(w)(that)
          .asInstanceOf[that.iConstructorPattern[U] {
                type RecDataType = eg.RecDataType;
                type InducDataType = eg.InducDataType
              }]
      that.CnstDepFuncPtn(tail, thatheadfibre)
    }

    def subs(x: Term, y: Term) = {
      val thatheadfibre = (z: TT) =>
        headfibre(z)
          .subs(x, y)
          .asInstanceOf[iConstructorPattern[U] {
                type RecDataType = self.RecDataType;
                type InducDataType = self.InducDataType
              }]
      CnstDepFuncPtn(tail.subs(x, y), thatheadfibre)
    }

    def headData(data: RecDataType, arg: ArgType, f: => I): HeadRecDataType = {
      data(arg)
    }

    def headInducData(
        data: InducDataType, arg: ArgType, f: => DI): HeadInducDataType = {
      data(arg)
    }
  }

  /**
    * Constructor for an inductive type, with given scala type and poly-pattern of this type.
    *
    * abstraction of ConstructorDefn mainly to allow different type parameters.
    */
  trait iConstructor { self =>

    /**
      * scala type, especially (nested) functions
      */
    type Cnstr <: Term with Subs[Cnstr]

    //  type Cod <: Term with Subs[Cod]
    /**
      * constructor-pattern for the constructor
      */
    val pattern: iConstructorPattern[Cnstr]

    //    val typ: Typ[Term]

    /**
      * the constructor (function or constant) itself.
      */
    val cons: Cnstr

    /**
      * the type for which this is a constructor
      */
    val W: F
  }

  object iConstructor {
    case class RecSym(cons: iConstructor) extends AtomicSym

    case class InducSym(cons: iConstructor) extends AtomicSym
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
  case class iConstructorDefn[U <: Term with Subs[U]](
      pattern: iConstructorPattern[U],
      cons: U,
      W: F
  )
      extends iConstructor {
    type Cnstr = U
  }

  case class PartialiConstructorSeq[FF <: Term with Subs[FF]](
      head: iConstructorTyp[FF], tail: iConstructorSeq) {
    def :::(name: AnySym) = {
      val pc = (name ::: head)
      pc |: tail
    }

    def ->>:[T <: Term with Subs[T]](that: Typ[T]) =
      PartialiConstructorSeq(that ->>: head, tail)

    def -->>:(that: Typ[H]) = PartialiConstructorSeq(that -->>: head, tail)

    def ~>>:[T <: Term with Subs[T]](thatVar: H) = {
      val newHead = thatVar ~>>: head
      PartialiConstructorSeq(thatVar ~>>: head, tail)
    }
  }

  sealed trait iConstructorSeq {
    def recDefn(X: Typ[Cod]): RecursiveDefinition[Total, Cod]

    val W: F

    type RecType <: Term with Subs[RecType]

    def recDataLambda(X: Typ[C]): I => RecType

    def rec(X: Typ[C]): RecType =
      recDataLambda(X: Typ[C])(curry(recDefn(X: Typ[C])))

    type InducType <: Term with Subs[InducType]

    def inducDefn(
        fibre: Func[Total, Typ[Cod]]): InductiveDefinition[Total, Cod]

    def inducDataLambda(fibre: Func[Total, Typ[Cod]]): DI => InducType

    def induc(fibre: Func[Total, Typ[Cod]]): InducType =
      inducDataLambda(fibre)(depCurry(inducDefn(fibre)))

    def inducCast(x: Term) =
      induc(
          typFmlyPtn.uncurryTyp(x.asInstanceOf[typFmlyPtn.IterTypFunc])
      )

    def |:(head: iConstructor) = iConstructorSeq.Cons(head, this)

    def ||:(typ: Typ[H]) =
      PartialiConstructorSeq(iConstructorTyp(iW(getTotalArg(typ, W)), W), this)

    val intros: List[Term]
  }

//  def totalFibre(fibre: Func[Total, Typ[Cod]], W: F): Func[Total, Typ[Cod]] = {
//    val x = domTotal(W).Var
//    lmbda(x)(fibre(x))
//  }

  case class Family(W: F) {
    def empty = iConstructorSeq.Empty(W)

    def =:(head: iConstructor) = iConstructorSeq.Cons(head, empty)

    def head(typ: Typ[H]) = iConstructorTyp.iConstructorHead((typ, W))

    def :|:(typ: Typ[H]) = head(typ)
  }

  object iConstructorSeq {
    case class Empty(W: F) extends iConstructorSeq {
      def recDefn(X: Typ[C]) =
        RecursiveDefinition.Empty(domTotal(W), X)

      def =:(head: iConstructor) = iConstructorSeq.Cons(head, this)

      type RecType = I

      def recDataLambda(X: Typ[C]) = (f) => f

      type InducType = DI

      def inducDefn(fibre: Func[Total, Typ[Cod]]) =
        InductiveDefinition.Empty(fibre)

      def inducDataLambda(fibre: Func[Total, Typ[C]]) = (f) => f

      val intros: List[Term] = List()
    }

    case class Cons(
        cons: iConstructor,
        tail: iConstructorSeq
    )
        extends iConstructorSeq {

      val W = tail.W

      def data(X: Typ[C]): cons.pattern.RecDataType =
        cons.pattern.recDataTyp(cons.W, X).symbObj(iConstructor.RecSym(cons))

      val defn = (d: cons.pattern.RecDataType) =>
        (f: Func[Total, C]) => cons.pattern.recDefCase(cons.cons, d, curry(f))

      def recDefn(X: Typ[C]) =
        RecursiveDefinition.DataCons(data(X), defn, tail.recDefn(X))

      type RecType = Func[cons.pattern.RecDataType, tail.RecType]

      def recDataLambda(X: Typ[C]) =
        f => lmbda(data(X))(tail.recDataLambda(X)(f))

      def inducData(fibre: Func[Total, Typ[Cod]]) =
        cons.pattern
          .inducDataTyp(W, fibre)(cons.cons)
          .symbObj(iConstructor.InducSym(cons))

      type InducType = Func[cons.pattern.InducDataType, tail.InducType]

      val inducDefn = (d: cons.pattern.InducDataType) =>
        (f: FuncLike[Total, C]) =>
          cons.pattern.inducDefCase(cons.cons, d, depCurry(f))

      def inducDefn(fibre: Func[Total, Typ[C]]) = {
        InductiveDefinition.DataCons(
            inducData(fibre), inducDefn, tail.inducDefn(fibre))
      }

      def inducDataLambda(fibre: Func[Total, Typ[C]]) =
        (f: DI) => lmbda(inducData(fibre))(tail.inducDataLambda(fibre)(f))

      val intros: List[Term] = cons.cons :: tail.intros
    }
  }
}

object IndexedConstructorPatterns {
  def emptySeq[
      C <: Term with Subs[C], H <: Term with Subs[H], F <: Term with Subs[F]](
      typFmlyPtn: FmlyPtn[H, C, F], fmly: F) = {
    val cls = new IndexedConstructorPatterns(typFmlyPtn)
    cls.iConstructorSeq.Empty(fmly): cls.iConstructorSeq
  }
}
