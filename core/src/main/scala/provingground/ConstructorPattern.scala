package provingground
import HoTT._
import Families._
import math._
//import ScalaUniverses._
import scala.util._
import scala.language.existentials

import RecFunction._

/**
 * constructors, their patterns, recursion
 * @author gadgil
 */

/**
 * A composite pattern for inductive types.
 * Typically (A -> B -> W)-> C -> W -> (D -> W) -> W as a function of W
 * May have Pi-types instead of function types
 * Assumed to have fixed type for codomain  X.
 */
sealed trait ConstructorPattern[Cod <: Term with Subs[Cod], CnstrctrType <: Term with Subs[CnstrctrType]] { self =>

  type ConstructorType = CnstrctrType
  /**
   * changes codomain and propagates changes to other types.
   */
  def withCod[CC <: Term with Subs[CC]]: ConstructorPattern[CC, ConstructorType]

  /**
   * function pattern.
   */
  def -->:[V <: Term with Subs[V], T <: Term with Subs[T], D <: Term with Subs[D], F <: Term with Subs[F]](
    that: FmlyPtn[Term, Cod, F]
  ) = FuncPtn(that, this)

  //  def -->:[UU >: U <: Term ](that : Typ[Term])(implicit self : Typ[Term]) : ConstructorPattern[FuncLike[Term, UU]] = {
  //    if (that == self) FuncPtn[UU](IdFmlyPtn[Term], this) else CnstFncPtn[UU](that, this)
  //  }

  //    def :::[A](name : A)(implicit mytyp: Typ[Term]) : Constructor = constructor(mytyp, name)

  /**
   * returns HoTT type corresponding to the pattern given the (inductive) type W (to be the head).
   */
  def apply(tp: Typ[Term]): Typ[ConstructorType]

  /**
   * (upper bound for) scala type of the constructor-pattern, especially to show constructor is a function.
   */
  //   type ConstructorType <: Term with Subs[ConstructorType]

  /**
   * (scala) type of data for recursion corresponding to the single constructor
   */
  type RecDataType <: Term with Subs[RecDataType]

  type InducDataType <: Term with Subs[InducDataType]

  /**
   * domain containing the recursion data for the constructor, i.e., the HoTT type of recursion data.
   */
  def recDom(w: Typ[Term], x: Typ[Cod]): Typ[RecDataType]

  def inducDom(w: Typ[Term], xs: Func[Term, Typ[Cod]])(cons: ConstructorType) : Typ[InducDataType]

  /**
   * given a term, matches to see if this is the image of a given (quasi)-constructor.
   * returns simplification (wrapped in Some) if the term matches.
   * @param cons constructor, actually quasi-constructor, with which to match.
   * @param data definition data for the image of the constructor.
   * @param f the function being defined, to be applied recursively - not the function being modified.
   * @param g the function being modified.
   */
  def recDef(cons: ConstructorType, data: RecDataType, f: => Func[Term, Cod]): Term => Option[Cod]

  def inducDef(cons: ConstructorType, data: InducDataType, f: => FuncLike[Term, Cod]): Term => Option[Cod]



  def recModify(cons: ConstructorType)(data: RecDataType)(
    f: => Func[Term, Cod]
  )(g: Func[Term, Cod]): Func[Term, Cod] = new Func[Term, Cod] {
    lazy val dom = f.dom

    lazy val codom = f.codom

    lazy val typ = dom ->: codom

    def newobj = this

    def act(a: Term) = (recDef(cons, data, f)(a)).getOrElse(g(a))

    def subs(x: Term, y: Term) = this

    override def toString = f.toString
  }

  def inducModify(cons: ConstructorType)(data: InducDataType)(
    f: => FuncLike[Term, Cod]
  )(g: FuncLike[Term, Cod]): FuncLike[Term, Cod] = new FuncLike[Term, Cod] {
    lazy val dom = f.dom

    lazy val a = "a" :: dom

    lazy val depcodom = f.depcodom

    lazy val fibre = lmbda(a)(depcodom(a))

    lazy val typ = PiTyp(fibre)

    def newobj = this

    def act(a: Term) = (inducDef(cons, data, f)(a)).getOrElse(g(a))

    def subs(x: Term, y: Term) = this

    override def toString = f.toString
  }





  /**
   * invokes [[recDom]] after changing codomain type.
   */
  def rec[CC <: Term with Subs[CC]] = {
    val newPtn = withCod[CC]
    val fn: (ConstructorType, newPtn.RecDataType, Func[Term, CC]) => Term => Option[CC] = {
      (cons, data, f) => (t) => newPtn.recDef(cons, data, f)(t)
    }
    fn
  }

  /**
   * constructor for this pattern given inductive type and name.
   */
  def constructor(tp: => Typ[Term], name: AnySym): Constructor[Cod] = {
    val cons = apply(tp).symbObj(name)
    ConstructorDefn[ConstructorType, Cod](this, cons, tp)
  }

  def cons(tp: => Typ[Term], name: AnySym) = constructor(tp, name).cons

  /**
   * constructor for this pattern given inductive type, with a name symbol generated.
   */
  def newconstructor(tp: Typ[Term]): Constructor[Cod] = {
    val cons = apply(tp).obj
    ConstructorDefn[ConstructorType, Cod](this, cons, tp)
  }

  def cons(tp: => Typ[Term]) = newconstructor(tp).cons

  val univLevel: Int

}

object ConstructorPattern {
  val W = IdW

  /**
   * returns Some(x) if the term is f(x) with f given, otherwise None
   * @param func function f to match for f(x)
   */
  def getArg[D <: Term with Subs[D], U <: Term with Subs[U]](func: FuncLike[D, U]): Term => Option[D] = {
    case sym: Symbolic => sym.name match {
      case fx: ApplnSym[u, w] =>
        if (fx.func == func && fx.arg.typ == func.dom) Try(Some(fx.arg.asInstanceOf[D])).getOrElse(None)
        else getArg(func)(fx.func)
      case _ => None
    }
    case _ => None
  }
}

import ConstructorPattern._

/**
 * The constructor pattern W - the only valid head for constructor-patterns.
 */
case object IdW extends ConstructorPattern[Term, Term] {
  def apply(W: Typ[Term]) = W

  val univLevel = 0

  //    type ConstructorType = Term

  type RecDataType = Term

  type InducDataType = Term

  def recDom(w: Typ[Term], x: Typ[Term]) = x

  def inducDom(w: Typ[Term], xs: Func[Term, Typ[Term]])(cons: ConstructorType) : Typ[InducDataType] = xs(cons)

  //    type Cod = Term

  def withCod[CC <: Term with Subs[CC]] = IdTarg[CC]

  def recDef(cons: ConstructorType, data: RecDataType, f: => Func[Term, Term]): Term => Option[Term] = {
    case (t: Term) if t == cons => Some(data)
    case _ => None
  }

  def inducDef(cons: ConstructorType, data: InducDataType, f: => FuncLike[Term, Term]): Term => Option[Term] = {
    case (t: Term) if t == cons => Some(data)
    case _ => None
  }


  case class IdTarg[C <: Term with Subs[C]]() extends ConstructorPattern[C, Term] {
    def apply(W: Typ[Term]) = W

    val univLevel = 0

    //    type ConstructorType = Term

    type RecDataType = C

    type InducDataType = C

    //    type Cod = C

    def recDom(w: Typ[Term], x: Typ[C]) = x

    def inducDom(w: Typ[Term], xs: Func[Term, Typ[C]])(cons: ConstructorType) : Typ[InducDataType] = xs(cons)

    def withCod[CC <: Term with Subs[CC]] = IdTarg[CC]

    def recDef(cons: ConstructorType, data: RecDataType, f: => Func[Term, C]): Term => Option[C] = {
      case (t: Term) if t == cons => Some(data)
      case _ => None
    }

    def inducDef(cons: ConstructorType, data: InducDataType, f: => FuncLike[Term, C]): Term => Option[C] = {
      case (t: Term) if t == cons => Some(data)
      case _ => None
    }


  }

}

/**
 * Functional extension of a type pattern
 */
sealed trait RecursiveConstructorPattern[Cod <: Term with Subs[Cod], ArgT <: Term with Subs[ArgT], HeadT <: Term with Subs[HeadT], CT <: FuncLike[ArgT, HeadT] with Subs[CT]] extends ConstructorPattern[Cod, CT] { self =>
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

  //   type ConstructorType <: FuncLike[ArgType, HeadType] with Subs[ConstructorType]

  /**
   * (scala) type of recursive data for head.
   */
  type HeadRecDataType <: Term

  type HeadInducDataType <: Term

  /**
   * The head pattern, constant T for A -> T and T(a) for A ~> T(a)
   */
  val headfibre: ArgType => ConstructorPattern[Cod, HeadType] {
    type RecDataType = HeadRecDataType;
    type InducDataType = HeadInducDataType
  }

  /**
   * returns data for recursion to be passed on to the head given an argument (when matching with the constructor).
   */
  def headData(data: RecDataType, arg: ArgType, f: => Func[Term, Cod]): HeadRecDataType

  def recDef(cons: ConstructorType, data: RecDataType, f: => Func[Term, Cod]): Term => Option[Cod] = {
    t =>
      for (arg <- getArg(cons)(t); term <- headfibre(arg).recDef(cons(arg), headData(data, arg, f), f)(t)) yield term
  }

  def headInducData(data: InducDataType, arg: ArgType, f: => FuncLike[Term, Cod]): HeadInducDataType

  def inducDef(cons: ConstructorType, data: InducDataType, f: => FuncLike[Term, Cod]): Term => Option[Cod] = {
    t =>
      for (arg <- getArg(cons)(t); term <- headfibre(arg).inducDef(cons(arg), headInducData(data, arg, f), f)(t)) yield term
  }


}

/**
 * Extending a constructor-pattern by a type pattern.
 */
case class FuncPtn[C <: Term with Subs[C], F <: Term with Subs[F], HC <: Term with Subs[HC]](
  tail: FmlyPtn[Term, C, F], head: ConstructorPattern[C, HC]
) extends RecursiveConstructorPattern[C, F, HC, Func[F, HC]] { self =>
  //    type ArgType = F

  //    type HeadType = head.ConstructorType

  //    type Cod = C

  def withCod[CC <: Term with Subs[CC]] = {
    FuncPtn[CC, F, HC](tail.withCod[CC], head.withCod[CC])
  }

  val _head: ConstructorPattern[C, HeadType] { type RecDataType = HeadRecDataType; type InducDataType = HeadInducDataType } = head

  val headfibre = (t: ArgType) => _head

  //    type ConstructorType = Func[ArgType, head.ConstructorType]

  type HeadRecDataType = head.RecDataType

  type HeadInducDataType = head.InducDataType

  type RecDataType = Func[tail.Family, Func[tail.TargetType, head.RecDataType]]

  type InducDataType = FuncLike[tail.Family, Func[tail.DepTargetType, head.InducDataType]]

  def recDom(w: Typ[Term], x: Typ[C]) = tail(w) ->: tail.target(x) ->: head.recDom(w, x)

  def inducDom(w: Typ[Term], xs: Func[Term, Typ[C]])(cons: ConstructorType) : Typ[InducDataType] = {
    val a = tail(w).Var
    val headcons = cons(a)
    val fibre = lmbda(a)(tail.depTarget(xs)(a) ->: head.inducDom(w, xs)(headcons))
    PiTyp(fibre)
  }

  def headData(data: RecDataType, arg: ArgType, f: => Func[Term, C]): HeadRecDataType = {
    data(arg)(tail.induced(f)(arg))
  }

  def headInducData(data: InducDataType, arg: ArgType, f: => FuncLike[Term, C]): HeadInducDataType = {
    data(arg)(tail.inducedDep(f)(arg))
  }

  def apply(W: Typ[Term]) = FuncTyp[ArgType, head.ConstructorType](tail(W), head(W))

  val univLevel = max(head.univLevel, tail.univLevel)
}

/**
 * Extending a poly-pattern by a constant type, i.e., not depending on W.
 */
case class CnstFncPtn[Cod <: Term with Subs[Cod], HC <: Term with Subs[HC]](
  tail: Typ[Term],
  head: ConstructorPattern[Cod, HC]
) extends RecursiveConstructorPattern[Cod, Term, HC, Func[Term, HC]] { self =>
  //   type ArgType = Term

  //   type HeadType = head.ConstructorType

  //  type Cod = head.Cod

  def withCod[CC <: Term with Subs[CC]] = {
    CnstFncPtn[CC, HC](tail, head.withCod[CC])
  }

  val _head: ConstructorPattern[Cod, HC] {
    type RecDataType = HeadRecDataType;
    type InducDataType = HeadInducDataType;
  } = head

  val headfibre = (t: ArgType) => _head

  type RecDataType = Func[Term, head.RecDataType]

  type InducDataType = FuncLike[Term, head.InducDataType]

  def recDom(w: Typ[Term], x: Typ[Cod]) = tail ->: head.recDom(w, x)

  def inducDom(w: Typ[Term], xs: Func[Term, Typ[Cod]])(cons: ConstructorType) : Typ[InducDataType] = {
    val a = tail.Var
    val headcons = cons(a)
    val fibre = lmbda(a)(head.inducDom(w, xs)(headcons))
    PiTyp(fibre)
  }

  type HeadRecDataType = head.RecDataType

  type HeadInducDataType = head.InducDataType

  //   type ConstructorType = Func[Term, head.ConstructorType]

  def headData(data: RecDataType, arg: ArgType, f: => Func[Term, Cod]): HeadRecDataType = data(arg)

  def headInducData(data: InducDataType, arg: ArgType, f: => FuncLike[Term, Cod]): HeadInducDataType = data(arg)

  def apply(W: Typ[Term]) = FuncTyp[Term, head.ConstructorType](tail, head(W))

  val univLevel = head.univLevel
}

/**
 * Extending a type pattern by a constant type to get (tail --> head).
 */
/**
 * Dependent extension of a poly-pattern by a type pattern.
 * XXX this may never be applicable
 */
case class DepFuncPtn[U <: Term with Subs[U], V <: Term with Subs[V], VV <: Term with Subs[VV], W <: Term with Subs[W], C <: Term with Subs[C], F <: Term with Subs[F]](
  tail: FmlyPtn[Term, C, F],
  headfibre: Term => (ConstructorPattern[C, U] { type RecDataType = V; type InducDataType = VV }),
  headlevel: Int = 0
) /*(implicit su: ScalaUniv[U])*/ extends RecursiveConstructorPattern[C, F, U, FuncLike[F, U]] { self =>
  //   type ArgType = F

  //   type HeadType = U

  //   type ConstructorType = FuncLike[ArgType, U]

  //  type Cod = C

  def withCod[CC <: Term with Subs[CC]] = {
    val eg = headfibre(Star).withCod[CC]
    val fibre = (t: Term) => headfibre(t).withCod[CC].asInstanceOf[ConstructorPattern[CC, U] { type RecDataType = eg.RecDataType; type InducDataType = eg.InducDataType }]
    DepFuncPtn(tail.withCod[CC], fibre)
  }

  type RecDataType = FuncLike[tail.Family, Func[tail.TargetType, V]]

  type InducDataType = FuncLike[tail.Family, Func[tail.DepTargetType, VV]]

  def recDom(w: Typ[Term], x: Typ[C]) = {
    val a = tail(w).Var
    val fibre = lmbda(a)(tail.target(x) ->: headfibre(a).recDom(w, x))
    PiTyp(fibre)
  }

  def inducDom(w: Typ[Term], xs: Func[Term, Typ[C]])(cons: ConstructorType) : Typ[InducDataType] = {
    val a = tail(w).Var
    val headcons = cons(a)
    val fibre = lmbda(a)(tail.depTarget(xs)(a) ->: headfibre(a).inducDom(w, xs)(headcons))
    PiTyp(fibre)
  }


  type HeadRecDataType = V

  type HeadInducDataType = VV

  def headData(data: RecDataType, arg: ArgType, f: => Func[Term, C]): HeadRecDataType = {
    val W = f.dom
    val X = f.codom
    val d = tail.induced(f)(arg)
    data(arg)(d)
  }

  def headInducData(data: InducDataType, arg: ArgType, f: => FuncLike[Term, C]): HeadInducDataType = {
    val W = f.dom
    val Xs = f.depcodom
    val d = tail.inducedDep(f)(arg)
    data(arg)(d)
  }


  def apply(W: Typ[Term]): Typ[FuncLike[ArgType, U]] = {
    //      val head = headfibre(W.symbObj(Star))
    //      val fiber = typFamily[Term, U](tail(W),  (t : Term) => headfibre(t)(W))
    val a = tail(W).Var
    val fiber = lmbda(a)(headfibre(a)(W))
    PiTyp[ArgType, U](fiber)
  }

  //    type ConstructorType = Term

  val univLevel = max(tail.univLevel, headlevel)
}

/**
 * Dependent extension by a constant type  of a constructor-pattern depending on elements of that type.
 */
case class CnstDepFuncPtn[U <: Term with Subs[U], V <: Term with Subs[V], VV <: Term with Subs[VV], C <: Term with Subs[C]](
  tail: Typ[Term],
  headfibre: Term => (ConstructorPattern[C, U] { type RecDataType = V; type InducDataType = VV }), headlevel: Int = 0
) extends RecursiveConstructorPattern[C, Term, U, FuncLike[Term, U]] { self =>

  //    type ArgType = Term

  //    type HeadType = U

  //  type Cod = C

  def withCod[CC <: Term with Subs[CC]] = {
    val eg = headfibre(Star).withCod[CC]
    val fibre = (t: Term) => headfibre(t).withCod[CC].asInstanceOf[ConstructorPattern[CC, U] { type RecDataType = eg.RecDataType; type InducDataType = eg.InducDataType }]

    CnstDepFuncPtn(tail, fibre)
  }

  //    type ConstructorType = FuncLike[Term, U]

  type RecDataType = FuncLike[Term, V]

  type InducDataType = FuncLike[Term, VV]

  def recDom(w: Typ[Term], x: Typ[C]) = {
    val a = tail.Var
    val fibre = lmbda(a)(headfibre(a).recDom(w, x))
    PiTyp(fibre)
  }

  def inducDom(w: Typ[Term], xs: Func[Term, Typ[C]])(cons: ConstructorType) : Typ[InducDataType] = {
    val a = tail.Var
    val headcons = cons(a)
    val fibre = lmbda(a)(headfibre(a).inducDom(w, xs)(headcons))
    PiTyp(fibre)
  }


  type HeadRecDataType = V

  type HeadInducDataType = VV

  def headData(data: RecDataType, arg: ArgType, f: => Func[Term, C]): HeadRecDataType = {
    data(arg)
  }

  def headInducData(data: InducDataType, arg: ArgType, f: => FuncLike[Term, C]): HeadInducDataType = data(arg)

  def apply(W: Typ[Term]): Typ[FuncLike[Term, U]] = {
    //     val fiber = typFamily[Term, U](tail,  (t : Term) => headfibre(t)(W))
    val a = W.Var
    val fiber = lmbda(a)(headfibre(a)(W))
    PiTyp[Term, U](fiber)
  }

  //    type ConstructorType = Term

  val univLevel = headlevel
}

/**
 * Constructor for an inductive type, with given scala type and poly-pattern of this type.
 *
 * abstraction of ConstructorDefn mainly to allow different type parameters.
 */
trait Constructor[Cod <: Term with Subs[Cod]] { self =>
  /**
   * scala type, especially (nested) functions
   */
  type ConstructorType <: Term with Subs[ConstructorType]

  //    type Cod <: Term with Subs[Cod]
  /**
   * constructor-pattern for the constructor
   */
  val pattern: ConstructorPattern[Cod, ConstructorType]

  //    val typ: Typ[Term]

  /**
   * the constructor (function or constant) itself.
   */
  val cons: pattern.ConstructorType

  /**
   * the type for which this is a constructor
   */
  val W: Typ[Term]
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
case class ConstructorDefn[U <: Term with Subs[U], C <: Term with Subs[C]](
  pattern: ConstructorPattern[C, U],
  cons: U, W: Typ[Term]
) extends Constructor[C] {
  type ConstructorType = U

  //    type Cod = C
}

