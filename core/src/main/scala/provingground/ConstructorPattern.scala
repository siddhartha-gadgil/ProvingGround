package provingground
import HoTT._
//import IterFuncPtn._
import math.max
//import ScalaUniverses._
import scala.util.Try
import scala.language.existentials

//import IterFuncPattern.{IterFuncPtn => IterFuncPtn, _}

import IterFuncPattern._

//import RecFunction._

/**
  * constructors, their patterns, recursion
  * @author gadgil
  */
/**
  * A composite pattern for inductive types.
  * Typically (A -> B -> W)-> C -> W -> (D -> W) -> W as a function of W
  * May have Pi-types instead of function types
  * Assumed to have fixed type for codomain  X.
  *
  * @tparam H scala type of the terms of the inductive type.
  * @tparam CnstrctrType  scala type of a constructor corresponding to this pattern.
  * @tparam Cod scala type of objects in codomain for recursion and induction functions.
  *
  * The type of the codomain is needed as there are inner types for data for recursion and induction functions.
  */
sealed trait ConstructorPattern[Cod <: Term with Subs[Cod],
                                CnstrctrType <: Term with Subs[CnstrctrType],
                                H <: Term with Subs[H]] { self =>

  type DomainType = H

  type ConstructorType = CnstrctrType

  /**
    * changes codomain and propagates changes to other types.
    */
  def withCod[CC <: Term with Subs[CC]](
      w: Typ[H]): ConstructorPattern[CC, ConstructorType, H]

  def subs(x: Term, y: Term): ConstructorPattern[Cod, CnstrctrType, H]

  /**
    * returns HoTT type corresponding to the pattern given the (inductive) type W (to be the head).
    */
  def apply(tp: Typ[H]): Typ[ConstructorType]

  /**
    * (scala) type of data for recursion corresponding to the single constructor
    */
  type RecDataType <: Term with Subs[RecDataType]

  /**
    * (scala) type of data for recursion corresponding to the single constructor
    */
  type InducDataType <: Term with Subs[InducDataType]

  /**
    * domain containing the recursion data for the constructor, i.e., the HoTT type of recursion data.
    */
  def recDom(w: Typ[H], x: Typ[Cod]): Typ[RecDataType]

  /**
    * domain containing the induction data for the constructor, i.e., the HoTT type of the induction data.
    */
  def inducDom(w: Typ[H], xs: Func[H, Typ[Cod]])(
      cons: ConstructorType): Typ[InducDataType]

  /**
    * given a term, matches to see if this is the image of a given (quasi)-constructor, with `this` constructor pattern.
    * optionally returns simplification (if the term matches), determined by the recursion data.
    * @param cons constructor, actually quasi-constructor, with which to match.
    * @param data definition data for the image of the constructor.
    * @param f the function being defined recursively, to be used recursively in definition.
    */
  def recDef(cons: ConstructorType,
             data: RecDataType,
             f: => Func[H, Cod]): H => Option[Cod]

  /**
    * given a term, matches to see if this is the image of a given (quasi)-constructor, with `this` constructor pattern.
    * optionally returns simplification (if the term matches).
    * @param cons constructor, actually quasi-constructor, with which to match.
    * @param data definition data for the image of the constructor.
    * @param f the function being defined inductively, to be used recursively in definition.
    */
  def inducDef(cons: ConstructorType,
               data: InducDataType,
               f: => FuncLike[H, Cod]): H => Option[Cod]

  // Concrete methods implementing recursion

  /**
    * Mixin in this constructor in a recutrsive definition with many cases.
    * @param g result of previous cases
    * @param f the final function being defined, to be called recursively.
    * should not use this in new design, but use optional recDef directly.
    */
  @deprecated(
      "Use recDef instead, with formal application outside for concrete method",
      "April 22, 2016")
  def recModify(cons: ConstructorType)(data: RecDataType)(
      f: => Func[H, Cod]
  )(g: => Func[H, Cod]): Func[H, Cod] = new Func[H, Cod] {
    lazy val dom = f.dom

    lazy val codom = f.codom

    lazy val typ = dom ->: codom

    def newobj = this

    def act(a: H) = (recDef(cons, data, f)(a)).getOrElse(g(a))

    def subs(x: Term, y: Term) = this

    override def toString = f.toString
  }

  @deprecated(
      "Use inducDef instead, with formal application outside for concrete method",
      "April 26, 2016")
  def inducModify(cons: ConstructorType)(data: InducDataType)(
      f: => FuncLike[H, Cod]
  )(g: FuncLike[H, Cod]): FuncLike[H, Cod] = new FuncLike[H, Cod] {
    lazy val dom = f.dom

    lazy val a = "a" :: dom

    lazy val depcodom = f.depcodom

    lazy val fibre = lmbda(a)(depcodom(a))

    lazy val typ = PiTyp(fibre)

    def newobj = this

    def act(a: H) = (inducDef(cons, data, f)(a)).getOrElse(g(a))

    def subs(x: Term, y: Term) = this

    override def toString = f.toString
  }

  /**
    * invokes [[recDef]] after changing codomain type.
    */
  def rec[CC <: Term with Subs[CC]](w: Typ[H]) = {
    val newPtn = withCod[CC](w)
    val fn: (ConstructorType, newPtn.RecDataType,
             Func[H, CC]) => H => Option[CC] = { (cons, data, f) => (t) =>
      newPtn.recDef(cons, data, f)(t)
    }
    fn
  }

  /**
    * function pattern.
    */
  def -->:[F <: Term with Subs[F]](that: IterFuncPtn[H, Cod, F]) =
    FuncPtn(that, this)

  def ->:[T <: Term with Subs[T]](tail: Typ[T]) = CnstFncPtn(tail, this)

  def ~>:[T <: Term with Subs[T]](tailVar: T) = {
    val fibre = (t: Term) =>
      this
        .subs(tailVar, t)
        .asInstanceOf[ConstructorPattern[
                Cod, CnstrctrType, provingground.HoTT.Term] {
              type RecDataType = self.RecDataType;
              type InducDataType = self.InducDataType
            }]
    CnstDepFuncPtn(tailVar.typ, fibre)
  }

  /**
    * constructor for this pattern given inductive type and name.
    */
  def constructor(tp: => Typ[H], name: AnySym) = {
    val cons = apply(tp).symbObj(name)
    ConstructorDefn[ConstructorType, Cod, H](this, cons, tp)
  }

  def cons(tp: => Typ[H], name: AnySym): CnstrctrType =
    constructor(tp, name).cons

  /**
    * constructor for this pattern given inductive type, with a name symbol generated.
    */
  def newconstructor(tp: Typ[H]): Constructor[Cod, H] = {
    val cons = apply(tp).obj
    ConstructorDefn[ConstructorType, Cod, H](this, cons, tp)
  }

  def cons(tp: => Typ[H]) = newconstructor(tp).cons

  val univLevel: Int
}

object ConstructorPattern {
  val Head = IdW[Term]

  /**
    * returns Some(x) if the term is f(x) with f given, otherwise None
    * @param func function f to match for f(x)
    */
  def getArg[D <: Term with Subs[D], U <: Term with Subs[U]](
      func: FuncLike[D, U]): Term => Option[D] = {
    case sym: Symbolic =>
      sym.name match {
        case fx: ApplnSym[u, w] =>
          if (fx.func == func && fx.arg.typ == func.dom)
            Try(Some(fx.arg.asInstanceOf[D])).getOrElse(None)
          else getArg(func)(fx.func)
        case _ => None
      }
    case _ => None
  }

  def get[H <: Term with Subs[H], Cnstr <: Term with Subs[Cnstr]](
      constype: Cnstr, w: Typ[H]): ConstructorPattern[Term, Cnstr, H] =
    constype match {
      case `w` =>
        IdW[H].asInstanceOf[ConstructorPattern[Term, Cnstr, H]]
      case FuncTyp(dom: Typ[u], codom: Typ[v]) =>
        val head = get(codom, w)
        if (dom.dependsOn(w)) {
          val tail = IterFuncPtn.get[H, Term, u](w)(dom)
          val fp = FuncPtn(tail, head)
          fp.asInstanceOf[ConstructorPattern[Term, Cnstr, H]]
        } else
          CnstFncPtn(codom, head)
            .asInstanceOf[ConstructorPattern[Term, Cnstr, H]]
      case PiTyp(fib: Func[u, Typ[v]]) =>
        val fibre = fib.asInstanceOf[Func[u, Typ[v]]]
        val egfib = get(fibre(fibre.dom.obj), w)
        val headfibre = (t: Term) =>
          get(fibre(t.asInstanceOf[u]), w)
            .asInstanceOf[ConstructorPattern[HoTT.Term, v, H] {
                type RecDataType = egfib.RecDataType;
                type InducDataType = egfib.InducDataType
              }]
        if (fibre.dom.dependsOn(w)) {
          val tail = IterFuncPtn.get[H, Term, u](w)(fibre.dom)
          val tp =
            DepFuncPtn[v, egfib.RecDataType, egfib.InducDataType, Term, u, H](
                tail, headfibre)
          tp.asInstanceOf[ConstructorPattern[Term, Cnstr, H]]
        } else {
          val cp =
            CnstDepFuncPtn[v, egfib.RecDataType, egfib.InducDataType, Term, H](
                fibre.dom.asInstanceOf[Typ[H]], headfibre)
          cp.asInstanceOf[ConstructorPattern[Term, Cnstr, H]]
        }
    }
}

/**
  * Constructor pattern with type, for convenient building.
  */
case class ConstructorTyp[F <: Term with Subs[F], H <: Term with Subs[H]](
    pattern: ConstructorPattern[Term, F, H], typ: Typ[H]) {
  def :::(name: AnySym): Constructor[Term, H] = pattern.constructor(typ, name)

  def -->:(that: Typ[H]) = {
    assert(
        that == typ,
        s"the method -->: is for extenidng by the same type but $that is not $typ")
    val tail = IdFmlyPtn[H, Term]
    val ptn = FuncPtn(tail, pattern)
    ConstructorTyp(ptn, typ)
  }

  def -->:[FF <: Term with Subs[FF]](that: IterFuncTyp[H, Term, FF]) =
    ConstructorTyp(that.pattern -->: pattern, typ)

  def ->:[T <: Term with Subs[T]](that: Typ[T]) = {
    assert(
        !(that.dependsOn(typ)),
        "the method ->: is for extension by constant types, maybe you mean _-->:_")
    ConstructorTyp(that ->: pattern, typ)
  }

  def ~>:[T <: Term with Subs[T]](thatVar: T) =
    ConstructorTyp(thatVar ~>: pattern, typ)
}

object ConstructorTyp {
  implicit class ConstructorHead[H <: Term with Subs[H]](typ: Typ[H]) {
    def pair = ConstructorTyp(IdW[H], typ)
    def :::(name: AnySym) = name ::: pair

    def ->>:[T <: Term with Subs[T]](that: Typ[T]) = that ->: pair

    def -->>:(that: Typ[H]) = that -->: pair

    def ~>>[T <: Term with Subs[T]](thatVar: T) = thatVar ~>: pair
  }
}

import ConstructorPattern._

/**
  * The constructor pattern W - the only valid head for constructor-patterns.
  */
case class IdW[H <: Term with Subs[H]]()
    extends ConstructorPattern[Term, H, H] {
  def apply(W: Typ[H]) = W

  val univLevel = 0

  //    type ConstructorType = Term

  type RecDataType = Term

  type InducDataType = Term

  def recDom(w: Typ[H], x: Typ[Term]) = x

  def inducDom(w: Typ[H], xs: Func[H, Typ[Term]])(
      cons: ConstructorType): Typ[InducDataType] = xs(cons)

  //    type Cod = Term

  def withCod[CC <: Term with Subs[CC]](w: Typ[H]) = IdTarg[CC, H]

  def subs(x: Term, y: Term) = this

  def recDef(cons: ConstructorType,
             data: RecDataType,
             f: => Func[H, Term]): H => Option[Term] = {
    case (t: Term) if t == cons => Some(data)
    case _ => None
  }

  def inducDef(cons: ConstructorType,
               data: InducDataType,
               f: => FuncLike[H, Term]): Term => Option[Term] = {
    case (t: Term) if t == cons => Some(data)
    case _ => None
  }
}
case class IdTarg[C <: Term with Subs[C], H <: Term with Subs[H]]()
    extends ConstructorPattern[C, H, H] {
  def apply(W: Typ[H]) = W

  val univLevel = 0

  //    type ConstructorType = Term

  type RecDataType = C

  type InducDataType = C

  //    type Cod = C

  def recDom(w: Typ[H], x: Typ[C]) = x

  def inducDom(w: Typ[H], xs: Func[H, Typ[C]])(
      cons: ConstructorType): Typ[InducDataType] = xs(cons)

  def withCod[CC <: Term with Subs[CC]](w: Typ[H]) = IdTarg[CC, H]

  def subs(x: Term, y: Term) = this

  def recDef(cons: ConstructorType,
             data: RecDataType,
             f: => Func[H, C]): Term => Option[C] = {
    case (t: Term) if t == cons => Some(data)
    case _ => None
  }

  def inducDef(cons: ConstructorType,
               data: InducDataType,
               f: => FuncLike[H, C]): Term => Option[C] = {
    case (t: Term) if t == cons => Some(data)
    case _ => None
  }
}

/**
  * Functional extension of a type pattern
  */
sealed trait RecursiveConstructorPattern[
    Cod <: Term with Subs[Cod],
    ArgT <: Term with Subs[ArgT],
    HeadT <: Term with Subs[HeadT],
    CT <: FuncLike[ArgT, HeadT] with Subs[CT],
    H <: Term with Subs[H]]
    extends ConstructorPattern[Cod, CT, H] { self =>

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
  val headfibre: ArgType => ConstructorPattern[Cod, HeadType, H] {
    type RecDataType = HeadRecDataType;
    type InducDataType = HeadInducDataType
  }

  /**
    * returns data for recursion to be passed on to the head given an argument (when matching with the constructor).
    */
  def headData(
      data: RecDataType, arg: ArgType, f: => Func[H, Cod]): HeadRecDataType

  def recDef(cons: ConstructorType,
             data: RecDataType,
             f: => Func[H, Cod]): H => Option[Cod] = { t =>
    for (arg <- getArg(cons)(t);
         term <- headfibre(arg).recDef(cons(arg), headData(data, arg, f), f)(
                    t)) yield term
  }

  def headInducData(data: InducDataType,
                    arg: ArgType,
                    f: => FuncLike[H, Cod]): HeadInducDataType

  def inducDef(cons: ConstructorType,
               data: InducDataType,
               f: => FuncLike[H, Cod]): H => Option[Cod] = { t =>
    for (arg <- getArg(cons)(t);
         term <- headfibre(arg).inducDef(
                    cons(arg), headInducData(data, arg, f), f)(t)) yield term
  }
}

/**
  * Extending a constructor-pattern by a type pattern.
  */
case class FuncPtn[C <: Term with Subs[C],
                   F <: Term with Subs[F],
                   HC <: Term with Subs[HC],
                   H <: Term with Subs[H]](
    tail: IterFuncPtn[H, C, F],
    head: ConstructorPattern[C, HC, H]
)
    extends RecursiveConstructorPattern[C, F, HC, Func[F, HC], H] { self =>
  //    type ArgType = F

  //    type HeadType = head.ConstructorType

  //    type Cod = C

  def withCod[CC <: Term with Subs[CC]](w: Typ[H]) = {
    FuncPtn[CC, F, HC, H](tail.withCod[CC](w), head.withCod[CC](w))
  }

  def subs(x: Term, y: Term) =
    FuncPtn(tail.subs(x, y), head.subs(x, y))

  val _head: ConstructorPattern[C, HeadType, H] {
    type RecDataType = HeadRecDataType; type InducDataType = HeadInducDataType
  } = head

  val headfibre = (t: ArgType) => _head

  //    type ConstructorType = Func[ArgType, head.ConstructorType]

  type HeadRecDataType = head.RecDataType

  type HeadInducDataType = head.InducDataType

  type RecDataType = Func[tail.Family, Func[tail.TargetType, head.RecDataType]]

  type InducDataType =
    FuncLike[tail.Family, Func[tail.DepTargetType, head.InducDataType]]

  def recDom(w: Typ[H], x: Typ[C]) =
    tail(w) ->: tail.target(x) ->: head.recDom(w, x)

  def inducDom(w: Typ[H], xs: Func[H, Typ[C]])(
      cons: ConstructorType): Typ[InducDataType] = {
    val a = tail(w).Var
    val headcons = cons(a)
    val fibre =
      lmbda(a)(tail.depTarget(xs)(a) ->: head.inducDom(w, xs)(headcons))
    PiTyp(fibre)
  }

  def headData(
      data: RecDataType, arg: ArgType, f: => Func[H, C]): HeadRecDataType = {
    data(arg)(tail.induced(f)(arg))
  }

  def headInducData(data: InducDataType,
                    arg: ArgType,
                    f: => FuncLike[H, C]): HeadInducDataType = {
    data(arg)(tail.inducedDep(f)(arg))
  }

  def apply(W: Typ[H]) =
    FuncTyp[ArgType, head.ConstructorType](tail(W), head(W))

  val univLevel = max(head.univLevel, tail.univLevel)
}

/**
  * Extending a poly-pattern by a constant type, i.e., not depending on W.
  */
case class CnstFncPtn[T <: Term with Subs[T],
                      Cod <: Term with Subs[Cod],
                      HC <: Term with Subs[HC],
                      H <: Term with Subs[H]](
    tail: Typ[T],
    head: ConstructorPattern[Cod, HC, H]
)
    extends RecursiveConstructorPattern[Cod, T, HC, Func[T, HC], H] { self =>
  //   type ArgType = Term

  //   type HeadType = head.ConstructorType

  //  type Cod = head.Cod

  def withCod[CC <: Term with Subs[CC]](w: Typ[H]) = {
    CnstFncPtn[T, CC, HC, H](tail, head.withCod[CC](w))
  }

  def subs(x: Term, y: Term) =
    CnstFncPtn(tail.subs(x, y), head.subs(x, y))

  val _head: ConstructorPattern[Cod, HC, H] {
    type RecDataType = HeadRecDataType;
    type InducDataType = HeadInducDataType;
  } = head

  val headfibre = (t: ArgType) => _head

  type RecDataType = Func[T, head.RecDataType]

  type InducDataType = FuncLike[T, head.InducDataType]

  def recDom(w: Typ[H], x: Typ[Cod]) = tail ->: head.recDom(w, x)

  def inducDom(w: Typ[H], xs: Func[H, Typ[Cod]])(
      cons: ConstructorType): Typ[InducDataType] = {
    val a = tail.Var
    val headcons = cons(a)
    val fibre = lmbda(a)(head.inducDom(w, xs)(headcons))
    PiTyp(fibre)
  }

  type HeadRecDataType = head.RecDataType

  type HeadInducDataType = head.InducDataType

  //   type ConstructorType = Func[Term, head.ConstructorType]

  def headData(
      data: RecDataType, arg: ArgType, f: => Func[H, Cod]): HeadRecDataType =
    data(arg)

  def headInducData(data: InducDataType,
                    arg: ArgType,
                    f: => FuncLike[H, Cod]): HeadInducDataType = data(arg)

  def apply(W: Typ[H]) = FuncTyp[T, head.ConstructorType](tail, head(W))

  val univLevel = head.univLevel
}

/**
  * Extending a type pattern by a constant type to get (tail --> head).
  */
/**
  * Dependent extension of a poly-pattern by a type pattern.
  * XXX this may never be applicable
  */
case class DepFuncPtn[U <: Term with Subs[U],
                      V <: Term with Subs[V],
                      VV <: Term with Subs[VV],
                      C <: Term with Subs[C],
                      F <: Term with Subs[F],
                      H <: Term with Subs[H]](
    tail: IterFuncPtn[H, C, F],
    headfibre: F => (ConstructorPattern[C, U, H] {
      type RecDataType = V; type InducDataType = VV
    }),
    headlevel: Int = 0
) /*(implicit su: ScalaUniv[U])*/
    extends RecursiveConstructorPattern[C, F, U, FuncLike[F, U], H] { self =>

  def withCod[CC <: Term with Subs[CC]](w: Typ[H]) = {
    val eg = headfibre(tail(w).Var).withCod[CC](w)
    val fibre = (t: F) =>
      headfibre(t)
        .withCod[CC](w)
        .asInstanceOf[ConstructorPattern[CC, U, H] {
              type RecDataType = eg.RecDataType;
              type InducDataType = eg.InducDataType
            }]
    DepFuncPtn(tail.withCod[CC](w), fibre)
  }

  def subs(x: Term, y: Term) = {
    val fibre = (t: F) =>
      headfibre(t)
        .subs(x, y)
        .asInstanceOf[ConstructorPattern[C, U, H] {
              type RecDataType = self.RecDataType;
              type InducDataType = self.InducDataType
            }]
    DepFuncPtn(tail.subs(x, y), fibre)
  }

  type RecDataType = FuncLike[tail.Family, Func[tail.TargetType, V]]

  type InducDataType = FuncLike[tail.Family, Func[tail.DepTargetType, VV]]

  def recDom(w: Typ[H], x: Typ[C]) = {
    val a = tail(w).Var
    val fibre = lmbda(a)(tail.target(x) ->: (headfibre(a).recDom(w, x)))
    PiTyp(fibre)
  }

  def inducDom(w: Typ[H], xs: Func[H, Typ[C]])(
      cons: ConstructorType): Typ[InducDataType] = {
    val a = tail(w).Var
    val headcons = cons(a)
    val fibre = lmbda(a)(
        tail.depTarget(xs)(a) ->: headfibre(a).inducDom(w, xs)(headcons))
    PiTyp(fibre)
  }

  type HeadRecDataType = V

  type HeadInducDataType = VV

  def headData(
      data: RecDataType, arg: ArgType, f: => Func[H, C]): HeadRecDataType = {
    val W = f.dom
    val X = f.codom
    val d = tail.induced(f)(arg)
    data(arg)(d)
  }

  def headInducData(data: InducDataType,
                    arg: ArgType,
                    f: => FuncLike[H, C]): HeadInducDataType = {
    val W = f.dom
    val Xs = f.depcodom
    val d = tail.inducedDep(f)(arg)
    data(arg)(d)
  }

  def apply(W: Typ[H]): Typ[FuncLike[ArgType, U]] = {
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
case class CnstDepFuncPtn[U <: Term with Subs[U],
                          V <: Term with Subs[V],
                          VV <: Term with Subs[VV],
                          C <: Term with Subs[C],
                          H <: Term with Subs[H]](
    tail: Typ[H],
    headfibre: H => (ConstructorPattern[C, U, H] {
      type RecDataType = V; type InducDataType = VV
    }),
    headlevel: Int = 0
)
    extends RecursiveConstructorPattern[C, H, U, FuncLike[H, U], H] { self =>

  //    type ArgType = Term

  //    type HeadType = U

  //  type Cod = C

  def withCod[CC <: Term with Subs[CC]](w: Typ[H]) = {
    val eg = headfibre(w.Var).withCod[CC](w)
    val fibre = (t: H) =>
      headfibre(t)
        .withCod[CC](w)
        .asInstanceOf[ConstructorPattern[CC, U, H] {
              type RecDataType = eg.RecDataType;
              type InducDataType = eg.InducDataType
            }]

    CnstDepFuncPtn(tail, fibre)
  }

  def subs(x: Term, y: Term) = {
    val eg = headfibre(tail.Var).subs(x, y)
    val fibre = (t: H) =>
      headfibre(t)
        .subs(x, y)
        .asInstanceOf[ConstructorPattern[C, U, H] {
              type RecDataType = eg.RecDataType;
              type InducDataType = eg.InducDataType
            }]

    CnstDepFuncPtn(tail.subs(x, y), fibre)
  }
  //    type ConstructorType = FuncLike[Term, U]

  type RecDataType = FuncLike[H, V]

  type InducDataType = FuncLike[H, VV]

  def recDom(w: Typ[H], x: Typ[C]) = {
    val a = tail.Var
    val fibre = lmbda(a)(headfibre(a).recDom(w, x))
    PiTyp(fibre)
  }

  def inducDom(w: Typ[H], xs: Func[H, Typ[C]])(
      cons: ConstructorType): Typ[InducDataType] = {
    val a = tail.Var
    val headcons = cons(a)
    val fibre = lmbda(a)(headfibre(a).inducDom(w, xs)(headcons))
    PiTyp(fibre)
  }

  type HeadRecDataType = V

  type HeadInducDataType = VV

  def headData(
      data: RecDataType, arg: ArgType, f: => Func[H, C]): HeadRecDataType = {
    data(arg)
  }

  def headInducData(data: InducDataType,
                    arg: ArgType,
                    f: => FuncLike[H, C]): HeadInducDataType = data(arg)

  def apply(W: Typ[H]): Typ[FuncLike[H, U]] = {
    //     val fiber = typFamily[Term, U](tail,  (t : Term) => headfibre(t)(W))
    val a = W.Var
    val fiber = lmbda(a)(headfibre(a)(W))
    PiTyp[H, U](fiber)
  }

  //    type ConstructorType = Term

  val univLevel = headlevel
}

/**
  * Constructor for an inductive type, with given scala type and poly-pattern of this type.
  *
  * abstraction of ConstructorDefn mainly to allow different type parameters.
  */
trait Constructor[Cod <: Term with Subs[Cod], H <: Term with Subs[H]] { self =>

  /**
    * scala type, especially (nested) functions
    */
  type ConstructorType <: Term with Subs[ConstructorType]

  //    type Cod <: Term with Subs[Cod]
  /**
    * constructor-pattern for the constructor
    */
  val pattern: ConstructorPattern[Cod, ConstructorType, H]

  //    val typ: Typ[Term]

  /**
    * the constructor (function or constant) itself.
    */
  val cons: pattern.ConstructorType

  /**
    * the type for which this is a constructor
    */
  val W: Typ[H]

  def withCod[CC <: Term with Subs[CC]]: Constructor[CC, H]
}

object Constructor {
  case class RecSym[C <: Term with Subs[C], H <: Term with Subs[H]](
      cons: Constructor[C, H])
      extends AnySym

  case class InducSym[C <: Term with Subs[C], H <: Term with Subs[H]](
      cons: Constructor[C, H])
      extends AnySym

  def fromName[
      U <: Term with Subs[U], C <: Term with Subs[C], H <: Term with Subs[H]](
      pattern: ConstructorPattern[C, U, H],
      name: String
  ) =
    (w: Typ[H]) => {
      val cons: U = pattern.cons(w, name)
      ConstructorDefn[U, C, H](pattern, cons, w)
    }

  def fromFormal(
      consType: Term, w: Typ[Term]): Typ[Term] => Constructor[Term, Term] =
    (typ: Typ[Term]) => {
      val name = consType.asInstanceOf[Symbolic].name
      consType match {
        case `w` =>
          IdW[Term].constructor(typ, name)
        case FuncTyp(dom: Typ[u], codom: Typ[v]) =>
          val head = fromFormal(codom, w)(typ)
          if (dom.dependsOn(w)) {
            val tail = IterFuncPtn.get[Term, Term, u](w)(dom)
            val fp = FuncPtn(tail, head.pattern)
            fp.constructor(typ, name)
          } else CnstFncPtn(codom, head.pattern).constructor(typ, name)
        case PiTyp(fib: Func[u, Typ[v]]) =>
          val fibre = fib.asInstanceOf[Func[u, Typ[v]]]
          val egfib = get(fibre(fibre.dom.obj), w)
          val headfibrePtn = (t: Term) =>
            fromFormal(fibre(t.asInstanceOf[u]), w)(typ).pattern
              .asInstanceOf[ConstructorPattern[HoTT.Term, v, Term] {
                  type RecDataType = egfib.RecDataType;
                  type InducDataType = egfib.InducDataType
                }]
          if (fibre.dom.dependsOn(w)) {
            val tail = IterFuncPtn.get[Term, Term, u](w)(fibre.dom)
            val tp = DepFuncPtn[
                v, egfib.RecDataType, egfib.InducDataType, Term, u, Term](
                tail, headfibrePtn)
            tp.constructor(typ, name)
          } else {
            val cp = CnstDepFuncPtn[
                v, egfib.RecDataType, egfib.InducDataType, Term, Term](
                fibre.dom.asInstanceOf[Typ[Term]],
                headfibrePtn
            )
            cp.constructor(typ, name)
          }
      }
    }
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
case class ConstructorDefn[
    U <: Term with Subs[U], C <: Term with Subs[C], H <: Term with Subs[H]](
    pattern: ConstructorPattern[C, U, H],
    cons: U,
    W: Typ[H]
)
    extends Constructor[C, H] {
  type ConstructorType = U

  def withCod[CC <: Term with Subs[CC]] =
    ConstructorDefn(pattern.withCod[CC](W), cons, W)
  //    type Cod = C
}
