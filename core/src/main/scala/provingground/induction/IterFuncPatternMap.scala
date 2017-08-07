package provingground.induction

import provingground._, HoTT._
import math._
//import scala.language.existentials

//import scala.util.Try

// trait HeadTerm extends Term with Subs[HeadTerm]

/**
 * a family of the form `P: A -> B -> W` etc, or dependent versions of this as a function of W,
 * together with the scala type of a codomain; has methods for defining induced functions and dependent functions.
 *
 * @tparam O scala type of terms of the type `W`
 * @tparam C scala type of codomain
 * @tparam F scala type of the family eg `P: A -> W`
 * @tparam TT scala type of family type eg `A -> B -> X`, ie, codomain of induced function
 * @tparam DT scala type of family type eg `A -> B -> X`, ie, codomain of induced function
 */
sealed trait IterFuncPtnMap[O <: Term with Subs[O], C <: Term with Subs[C], F <: Term with Subs[F], TT <: Term with Subs[TT], DT <: Term with Subs[DT]] {

  /**
   * the universe containing the type
   */
  val univLevel: Int

  type Cod = C

  /**
   * scala type (upper bound) for a member of the family, i.e., sections
   */
  type Family = F

  // /**
  //   * scala type of target for induced functions
  //   */
  // type TargetType <: Term with Subs[TargetType]
  //
  // /**
  //   * Note that the target may be a type family, not a type,
  //   *  so this is the actual type of objects.
  //   */
  // type DepTargetType <: Term with Subs[DepTargetType]

  /**
   * returns the type corresponding to the pattern, such as A -> W, given the (inductive) type W,
   *  this is used mainly for constructor patterns, with the W being fixed.
   */
  def apply(tp: Typ[O]): Typ[Family]

  /**
   * target scala type.
   */
  def target(x: Typ[Cod]): Typ[TT]

  /**
   * dependent target scala type.
   */
  def depTarget(xs: Func[O, Typ[Cod]]): Family => Typ[DT]

  def subs(x: Term, y: Term): IterFuncPtnMap[O, C, F, TT, DT]

  /**
   * function induced by f: W -> X of type (A -> W) -> (A -> X) etc
   *
   * @param f function from which to induce
   *
   * @param W the inductive type
   *
   * @param X codomain of the given function
   */
  def induced(f: Func[O, Cod]): Family => TT

  /**
   * dependent function induced by dependent f: W -> X(s) of type (A -> W) -> ((a : A) ~> Xs(a)) etc
   *
   * @param f dependent function from which to induce
   *
   * @param W the inductive type
   *
   * @param Xs family of codomains of the given dependent function
   */
  def inducedDep(f: FuncLike[O, Cod]): Family => DT
}

object IterFuncPtnMap {

  /**
   * The identity family
   */
  case class IdIterPtnMap[O <: Term with Subs[O], C <: Term with Subs[C]]()
    extends IterFuncPtnMap[O, C, O, C, C] {

    def apply(W: Typ[O]) = W

    //    type Family =  O

    type FamilyType = Typ[O]

    type Total = O

    def value(x: Total): O = x

    type IterFunc = Func[O, C]

    type IterTypFunc = Func[O, Typ[C]]

    type IterDepFunc = FuncLike[O, C]

    def iterFuncTyp(w: FamilyType, x: Typ[Cod]): Typ[IterFunc] = w ->: x

    def iterDepFuncTyp(w: FamilyType, xs: IterTypFunc) = PiDefn(xs)

    type TargetType = C

    type DepTargetType = C

    def target(x: Typ[Cod]) = x

    def depTarget(xs: Func[O, Typ[Cod]]) = xs

    def subs(x: Term, y: Term) = this

    //    type Cod = C

    //    type MemberType = O

    val univLevel = 0

    /**
     * induced function is the given one.
     */
    def induced(f: Func[O, C]) = f

    /**
     * induced function is the given one.
     */
    def inducedDep(f: FuncLike[O, C]) = f
  }

  case class FuncIterPtnMap[TT <: Term with Subs[TT], V <: Term with Subs[V], T <: Term with Subs[T], D <: Term with Subs[D], O <: Term with Subs[O], C <: Term with Subs[C]](
    tail: Typ[TT],
    head: IterFuncPtnMap[O, C, V, T, D]) extends IterFuncPtnMap[O, C, Func[TT, V], Func[TT, T], FuncLike[TT, D]] {
    self =>
    def apply(W: Typ[O]) = FuncTyp[TT, V](tail, head(W))

    type DepTargetType = FuncLike[TT, D]

    type TargetType = Func[TT, T]

    def target(x: Typ[Cod]) = tail ->: head.target(x)

    def depTarget(xs: Func[O, Typ[Cod]]) =
      (fmly: Family) => {
        val a = tail.Var
        val b = fmly(a)
        val targfibre = lmbda(a)(headfibre(a).depTarget(xs)(b))
        piDefn(a)(headfibre(a).depTarget(xs)(b))
      }

    //    type Cod = head.Cod

    def subs(x: Term, y: Term) =
      FuncIterPtnMap(tail, head.subs(x, y))

    val headfibre = (arg: Term) => head

    val univLevel = max(head.univLevel, univlevel(tail.typ))

    /**
     * inductively defining the induced function.
     * maps (g : tail --> head(W)) to func : tail --> head(X) given (head(W) --> head(X))
     *
     */
    def induced(f: Func[O, Cod]): Family => TargetType = {
      val x = tail.Var
      val g = apply(f.dom).Var
      lmbda(g)(
        lmbda(x)(head.induced(f)(g(x))))
    }

    /**
     * inductively defining the induced function.
     * maps (g : tail --> head(W)) to func : (t : tail) ~> head(Xs(t)) given (head(W) --> (t: tail) ~> head(Xs(t)))
     *
     */
    def inducedDep(f: FuncLike[O, Cod]): Family => DepTargetType = {
      val x = tail.Var
      val g = apply(f.dom).Var
      lambda(g)(
        lambda(x)(head.inducedDep(f)(g(x))))
    }
  }

  /**
   * Extending by a constant type A a family of type patterns depending on (a : A).
   *
   */
  case class DepFuncIterPtnMap[TT <: Term with Subs[TT], V <: Term with Subs[V], T <: Term with Subs[T], D <: Term with Subs[D], O <: Term with Subs[O], C <: Term with Subs[C]](
    tail: Typ[TT],
    headfibre: TT => IterFuncPtnMap[O, C, V, T, D],
    headlevel: Int = 0) extends IterFuncPtnMap[O, C, FuncLike[TT, V], FuncLike[TT, T], FuncLike[TT, D]] {

    //    type Family =  FuncLike[Term, V]

    //    type TargetType = FuncLike[TT, T]

    // type DepTargetType = FuncLike[Term, D]

    def apply(W: Typ[O]) = {
      val x = tail.Var
      val fiber = lmbda(x)(headfibre(x)(W))
      //   val fiber = typFamily(tail,  (t : Term) => headfibre(t)(W))
      piDefn[TT, V](x)(headfibre(x)(W))
    }

    //  type Cod = C

    def target(x: Typ[C]) = {
      val a = tail.Var
      val targfibre = lmbda(a)(headfibre(a).target(x))
      piDefn(a)(headfibre(a).target(x))
    }

    def depTarget(xs: Func[O, Typ[C]]) =
      (fmly: FuncLike[TT, V]) => {
        val a = tail.Var
        val b = fmly(a)
        val targfibre = lmbda(a)(headfibre(a).depTarget(xs)(b))
        piDefn(a)(headfibre(a).depTarget(xs)(b))
      }

    def subs(x: Term, y: Term) =
      DepFuncIterPtnMap(
        tail.replace(x, y),
        (tt: TT) => headfibre(tt).subs(x, y))

    //    val head = headfibre(tail.Var)

    //    type Family = FuncLike[Term, head.Family]

    def induced(f: Func[O, C]): FuncLike[TT, V] => FuncLike[TT, T] = {
      val x = tail.Var
      val g = apply(f.dom).Var
      lambda(g)(
        lambda(x)(headfibre(x).induced(f)(g(x))))
    }

    def inducedDep(f: FuncLike[O, C]): FuncLike[TT, V] => FuncLike[TT, D] = {
      val x = tail.Var
      val g = apply(f.dom).Var
      lambda(g)(
        lambda(x)(headfibre(x).inducedDep(f)(g(x))))
    }

    val univLevel = max(univlevel(tail.typ), headlevel)
  }
}
import scala.language.existentials

// sealed trait IterFuncMapper[
//   S <: Term with Subs[S],
//   O <: Term with Subs[O], C <: Term with Subs[C], F <: Term with Subs[F],
//       TT <: Term with Subs[TT], DT <: Term with Subs[DT]] {
//         def mapper(shape: IterFuncShape[S]) : IterFuncPtnMap[S, O, C, F,TT, DT]
//       }

/**
 * Given the scala type of the codomain, lifts [[IterFuncShape]] to [[IterFuncMapper]]
 */
sealed trait IterFuncMapper[O <: Term with Subs[O], C <: Term with Subs[C], F <: Term with Subs[F], TT <: Term with Subs[TT], DT <: Term with Subs[DT]] {

  /**
   * Given the scala type of the codomain, lifts [[IterFuncShape]] to [[IterFuncMapper]]
   */
  def mapper: IterFuncShape[O, F] => IterFuncPtnMap[O, C, F, TT, DT]
}

import IterFuncPtnMap._
import IterFuncShape._

object IterFuncMapper {
  implicit def idIterMapper[O <: Term with Subs[O], C <: Term with Subs[C]] =
    new IterFuncMapper[O, C, O, C, C] {
      def mapper =
        (shape: IterFuncShape[O, O]) => IdIterPtnMap[O, C]
    }

  import translation.Translator.unmatched

  implicit def funcIterMapper[Tail <: Term with Subs[Tail], O <: Term with Subs[O], C <: Term with Subs[C], HF <: Term with Subs[HF], HTT <: Term with Subs[HTT], HDT <: Term with Subs[HDT]](
    implicit
    hm: IterFuncMapper[O, C, HF, HTT, HDT]) =
    new IterFuncMapper[O, C, Func[Tail, HF], Func[Tail, HTT], FuncLike[Tail, HDT]] {
      def mapper = {
        case FuncShape(t, h) => FuncIterPtnMap(t, hm.mapper(h))
        case s => unmatched(s)
      }
    }

  implicit def depFuncIterMapper[Tail <: Term with Subs[Tail], O <: Term with Subs[O], C <: Term with Subs[C], HF <: Term with Subs[HF], HTT <: Term with Subs[HTT], HDT <: Term with Subs[HDT]](
    implicit
    hm: IterFuncMapper[O, C, HF, HTT, HDT]) =
    new IterFuncMapper[O, C, FuncLike[Tail, HF], FuncLike[Tail, HTT], FuncLike[Tail, HDT]] {
      def mapper = {
        case DepFuncShape(t, hf) =>
          DepFuncIterPtnMap(t, (tt: Tail) => hm.mapper(hf(tt)))
          case s => unmatched(s)
      }
    }
}

/**
 * a family of the form `P: A -> B -> W` etc, or dependent versions of this as a function of W.
 *
 * @tparam O scala type of terms of the type `W`
 * @tparam F scala type of the family eg `P: A -> W`
 */
sealed trait IterFuncShape[O <: Term with Subs[O], F <: Term with Subs[F]] {

  /**
   * returns the type corresponding to the pattern, such as A -> W, given the (inductive) type W,
   *  this is used mainly for constructor patterns, with the W being fixed.
   */
  def apply(tp: Typ[O]): Typ[F]

  def mapper[C <: Term with Subs[C]]: IterFuncMapper[O, C, F, TT, DT] forSome {
    type TT <: Term with Subs[TT];
    type DT <: Term with Subs[DT]
  }

  def subs(x: Term, y: Term): IterFuncShape[O, F]

  // def mapped[C <: Term with Subs[C]] =
  //   mapper[O].mapper(this)
}

object IterFuncShape {

  case class IdIterShape[O <: Term with Subs[O]]()
    extends IterFuncShape[O, O] {
    def apply(tp: Typ[O]) = tp

    def subs(x: Term, y: Term) = IdIterShape[O]

    def mapper[C <: Term with Subs[C]] =
      implicitly[IterFuncMapper[O, C, O, C, C]]
    //  (shape: IterFuncShape[HeadTerm]) => IdIterPtnMap[O, C]
  }

  case class FuncShape[TT <: Term with Subs[TT], O <: Term with Subs[O], HF <: Term with Subs[HF]](
    tail: Typ[TT],
    head: IterFuncShape[O, HF])
    extends IterFuncShape[O, Func[TT, HF]] {
    def apply(tp: Typ[O]) = tail ->: head(tp)

    def subs(x: Term, y: Term) = FuncShape(tail.replace(x, y), head.subs(x, y))

    def mapper[C <: Term with Subs[C]] = {
      IterFuncMapper.funcIterMapper(head.mapper)
      // case FuncShape(t, h) => FuncIterPtnMap(t, head.mapper(h))
    }
  }

  case class DepFuncShape[TT <: Term with Subs[TT], O <: Term with Subs[O], HF <: Term with Subs[HF]](
    tail: Typ[TT],
    headfibre: TT => IterFuncShape[O, HF])
    extends IterFuncShape[O, FuncLike[TT, HF]] {

    def apply(W: Typ[O]) = {
      val x = tail.Var
      piDefn(x)(headfibre(x)(W))
    }

    def subs(x: Term, y: Term) =
      DepFuncShape(tail.replace(x, y), (t: TT) => headfibre(t).subs(x, y))

    def mapper[C <: Term with Subs[C]] = {
      IterFuncMapper.depFuncIterMapper(headfibre(tail.Var).mapper)
    }
  }

  trait Exst {
    // type O <: Term with Subs[O]

    type F <: Term with Subs[F]

    val shape: IterFuncShape[Term, F]

    def piShape[TT <: Term with Subs[TT]](variable: TT, dom: Typ[TT]) = {
      DepFuncShape(dom, (t: TT) => shape.subs(variable, t))
    }

    def piWrap[TT <: Term with Subs[TT]](variable: TT, dom: Typ[TT]) =
      Exst(piShape(variable, dom))

    def ->:[TT <: Term with Subs[TT]](dom: Typ[TT]) =
      Exst(FuncShape(dom, shape))
  }

  object Exst {
    def apply[Fm <: Term with Subs[Fm]](sh: IterFuncShape[Term, Fm]) =
      new Exst {
        type F = Fm
        val shape = sh
      }
  }

  def getExst(w: Typ[Term], fmly: Typ[Term]): Exst = fmly match {
    case ft: FuncTyp[u, v] => ft.dom ->: getExst(w, ft.codom)
    case pd: PiDefn[u, v] =>
      val targWrap = getExst(w, pd.value)
      targWrap.piWrap(pd.variable, pd.domain)
    case `w` => Exst(IdIterShape[Term])
  }

  def fromTyp[F <: Term with Subs[F]](
    w: Typ[Term],
    fmly: F): IterFuncShape[Term, F] =
    fmly match {
      case ft: FuncTyp[u, v] =>
        FuncShape(ft.dom, fromTyp(w, ft.codom))
          .asInstanceOf[IterFuncShape[Term, F]]
      case ft: GenFuncTyp[u, v] =>
        DepFuncShape(ft.domain, (t: u) => fromTyp(w, ft.fib(t)))
          .asInstanceOf[IterFuncShape[Term, F]]
      case `w` => IdIterShape[Term].asInstanceOf[IterFuncShape[Term, F]]
    }
}
