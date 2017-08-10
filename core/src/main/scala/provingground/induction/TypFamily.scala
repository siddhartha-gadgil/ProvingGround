package provingground.induction

import provingground._, HoTT._

import scala.language.existentials

import shapeless._

import HList._

/**
 * allows substitution of a `Term` by another.
 */
trait Subst[A] {
  def subst(a: A)(x: Term, y: Term): A
}

object Subst {
  def apply[A: Subst]: Subst[A] = implicitly[Subst[A]]

  implicit def funcSubst[A: Subst]: Subst[Term => A] =
    new Subst[Term => A] {
      def subst(f: Term => A)(x: Term, y: Term) =
        (t: Term) => Subst[A].subst(f(t))(x, y)
    }
}

/**
 * allows substitution of a `Term` by another, as well as mapping to a vector of terms
 * chiefly subtypes of `Term` and `HList`s of these;
 *
 */
trait TermList[A] extends Subst[A] {

  def terms(a: A): Vector[Term]
}

object TermList extends TermListImplicits {
  def apply[A: TermList]: TermList[A] = implicitly[TermList[A]]

  implicit def termTermList[U <: Term with Subs[U]]: TermList[U] =
    new TermList[U] {
      def subst(a: U)(x: Term, y: Term) = a.replace(x, y)

      def terms(a: U) = Vector(a)
    }

  // implicit object UnitTermList extends TermList[Unit] {
  //   def subst(a: Unit)(x: Term, y: Term) = a
  // }

  implicit object HNilTermList extends TermList[HNil] {
    def subst(a: HNil)(x: Term, y: Term) = a

    def terms(a: HNil) = Vector()
  }

  implicit def hConsTermList[U: TermList, V <: HList: TermList]: TermList[U :: V] =
    new TermList[U :: V] {
      def subst(a: U :: V)(x: Term, y: Term) =
        implicitly[TermList[U]].subst(a.head)(x, y) :: implicitly[TermList[V]]
          .subst(a.tail)(x, y)

      def terms(a: U :: V) =
        implicitly[TermList[U]].terms(a.head) ++ implicitly[TermList[V]]
          .terms(a.tail)
    }

  // implicit def pairTermList[U: TermList, V: TermList]: TermList[(U, V)] =
  //   new TermList[(U, V)] {
  //     def subst(a: (U, V))(x: Term, y: Term) =
  //       (implicitly[TermList[U]].subst(a.head)(x, y),
  //        implicitly[TermList[V]].subst(a.tail)(x, y))
  //   }

}

trait SubstImplicits {
  implicit class SubstOp[A: Subst](a: A) {
    def subst(x: Term, y: Term) = implicitly[Subst[A]].subst(a)(x, y)

    def ~->:(x: Term) = (y: Term) => implicitly[Subst[A]].subst(a)(x, y)
  }
}

trait TermListImplicits extends SubstImplicits {
  implicit class TermListOp[A: TermList](a: A) extends SubstOp(a) {
    // def subst(x: Term, y: Term) = implicitly[Subst[A]].subst(a)(x, y)

    def terms = implicitly[TermList[A]].terms(a)
  }

}

object TestS { implicitly[TermList[Term :: HNil]] }

/**
 * the shape of a type family.
 */
sealed abstract class TypFamilyPtn[H <: Term with Subs[H], F <: Term with Subs[F], Index <: HList: TermList] {

  /**
   * optional index `a` given family `W` and type `W(a)`
   */
  def getIndex(w: F, typ: Typ[H]): Option[Index]

  /**
   * type `W(a)` given the family `W` and index `a`
   */
  def typ(w: F, index: Index): Typ[H]

  def subs(x: Term, y: Term): TypFamilyPtn[H, F, Index]

  /**
   * existentital typed mappper to bridge to [[TypFamilyMap]] given scala type of codomain
   */
  def mapper[C <: Term with Subs[C]]: TypFamilyMapper[H, F, C, Index, IF, IDF, IDFT] forSome {
    type IF <: Term with Subs[IF];
    type IDF <: Term with Subs[IDF];
    type IDFT <: Term with Subs[IDFT]
  }

  def finalCod[IDFT <: Term with Subs[IDFT]](depCod: IDFT): Typ[_]

  def codFamily(typ: Typ[Term]) : Term

  /**
   * mappper to bridge to [[TypFamilyMap]] given scala type of codomain based on implicits
   */
  def getMapper[C <: Term with Subs[C], IF <: Term with Subs[IF], IDF <: Term with Subs[IDF], IDFT <: Term with Subs[IDFT]](cod: Typ[C])(
    implicit
    mpr: TypFamilyMapper[H, F, C, Index, IF, IDF, IDFT]) = mpr

  /**
   * lift to [[TypFamilyMap]] based on [[mapper]]
   */
  def mapped[C <: Term with Subs[C]] = mapper[C].mapper(this)
}

object TypFamilyPtn {
  def apply[H <: Term with Subs[H], F <: Term with Subs[F], Index <: HList: TermList](w: F)(
    implicit
    g: TypFamilyPtnGetter[F, H, Index]) =
    g.get(w)

  import TypFamilyMapper._

  case class IdTypFamily[H <: Term with Subs[H]]()
    extends TypFamilyPtn[H, Typ[H], HNil] {
    def getIndex(w: Typ[H], typ: Typ[H]) = Some(HNil)

    def typ(w: Typ[H], index: HNil): Typ[H] = w

    def subs(x: Term, y: Term) = this

    def mapper[C <: Term with Subs[C]] = idTypFamilyMapper[H, C]

    def finalCod[IDFT <: Term with Subs[IDFT]](depCod: IDFT): Typ[_] =
      depCod match {
        case tf: Func[a, b] =>
          val x = tf.dom.Var
          tf(x) match {
            case tp: Typ[u] => tp
            case t: Term =>
              import translation.FansiShow._
              throw new IllegalArgumentException(
                s"expected type but got ${t.fansi}")
          }
      }

    def codFamily(typ: Typ[Term]) : Term = typ match {
      case tf: FuncLike[a, b] =>
        val x = tf.dom.Var
        tf(x) match {
          case tp: Typ[u] => x :-> (tp : Typ[Term])
          case t: Term =>
            import translation.FansiShow._
            throw new IllegalArgumentException(
              s"expected type but got ${t.fansi}")
        }
    }
  }

  case class FuncTypFamily[U <: Term with Subs[U], H <: Term with Subs[H], TF <: Term with Subs[TF], TI <: HList: TermList](
    head: Typ[U],
    tail: TypFamilyPtn[H, TF, TI])
    extends TypFamilyPtn[H, Func[U, TF], U :: TI] {

    def getIndex(w: Func[U, TF], typ: Typ[H]) = {
      val argOpt = getArg(w)(typ)
      argOpt flatMap { (arg) =>
        tail.getIndex(w(arg), typ).map((arg :: _))
      }
    }

    def typ(w: Func[U, TF], index: (U :: TI)): Typ[H] =
      tail.typ(w(index.head), index.tail)

    def subs(x: Term, y: Term) =
      FuncTypFamily(head.replace(x, y), tail.subs(x, y))

    def mapper[C <: Term with Subs[C]] =
      funcTypFamilyMapper(tail.mapper[C], implicitly[TermList[TI]])

    def finalCod[IDFT <: Term with Subs[IDFT]](depCod: IDFT): Typ[_] =
      tail.finalCod(fold(depCod)(head.Var))

    def codFamily(typ: Typ[Term]) : Term = {
      val x = head.Var
      typ match {
        case ft: GenFuncTyp[u, v] if ft.domain == head =>
          x :-> tail.codFamily(ft.fib(x.asInstanceOf[u]))
      }
    }

  }

  case class DepFuncTypFamily[U <: Term with Subs[U], H <: Term with Subs[H], TF <: Term with Subs[TF], TI <: HList: TermList](
    head: Typ[U],
    tailfibre: U => TypFamilyPtn[H, TF, TI])
    extends TypFamilyPtn[H, FuncLike[U, TF], U :: TI] {

    def getIndex(w: FuncLike[U, TF], typ: Typ[H]) = {
      val argOpt = getArg(w)(typ)
      argOpt flatMap { (arg) =>
        tailfibre(arg).getIndex(w(arg), typ).map((arg :: _))
      }
    }

    def typ(w: FuncLike[U, TF], index: U :: TI): Typ[H] =
      tailfibre(index.head).typ(w(index.head), index.tail)

    def subs(x: Term, y: Term) =
      DepFuncTypFamily(head.replace(x, y), (u: U) => tailfibre(u).subs(x, y))

    def mapper[C <: Term with Subs[C]] =
      depFuncTypFamilyMapper(
        tailfibre(head.Var).mapper[C],
        implicitly[TermList[TI]])

    def finalCod[IDFT <: Term with Subs[IDFT]](depCod: IDFT): Typ[_] = {
      val x = head.Var
      tailfibre(x).finalCod(fold(depCod)(x))
    }

    def codFamily(typ: Typ[Term]) : Term = {
      val x = head.Var
      typ match {
        case ft: GenFuncTyp[u, v] if ft.domain == head =>
          x :~> tailfibre(x).codFamily(ft.fib(x.asInstanceOf[u]))
      }
    }
  }

  sealed trait Exst {
    type F <: Term with Subs[F]
    type Index <: HList

    val value: TypFamilyPtn[Term, F, Index]

    implicit val subst: TermList[Index]

    def lambdaExst[TT <: Term with Subs[TT]](variable: TT, dom: Typ[TT]) =
      Exst(
        DepFuncTypFamily(dom, (t: TT) => value.subs(variable, t)))

    def ~>:[TT <: Term with Subs[TT]](variable: TT) =
      Exst(
        DepFuncTypFamily(
          variable.typ.asInstanceOf[Typ[TT]],
          (t: TT) => value.subs(variable, t)))

    def ->:[TT <: Term with Subs[TT]](dom: Typ[TT]) =
      Exst(FuncTypFamily(dom, value))
  }

  object Exst {
    def apply[Fb <: Term with Subs[Fb], In <: HList: TermList](
      tf: TypFamilyPtn[Term, Fb, In]) =
      new Exst {
        type F = Fb
        type Index = In

        val subst = implicitly[TermList[Index]]

        val value = tf
      }
  }

  def getExst[F <: Term with Subs[F]](w: F): Exst = w match {
    case _: Typ[u] => Exst(IdTypFamily[Term])
    case fn: Func[u, v] => fn.dom ->: getExst(fn(fn.dom.Var))
    case g: FuncLike[u, v] =>
      val x = g.dom.Var
      x ~>: getExst(g(x))
  }
}

/**
 * shape of a type family, together with the type of a codomain;
 * fixing scala types of functions and dependent functions on the type family
 *
 * @tparam Index scala type of the index
 * @tparam IF scala type of an iterated function on the inductive type family, with codomain with terms of type `Cod`.
 * @tparam IDF scala type of an iterated  dependent function on the inductive type family, with codomain with terms of type `Cod`.
 * @tparam IDFT scala type of an iterated type family on the inductive type family, i.e.,  with codomain with terms of type `Typ[Cod]`
 *
 * methods allow restricting (dependent) functions and type families to indices and
 * building (dependent) functions from such restrictions.
 */
sealed trait TypFamilyMap[H <: Term with Subs[H], F <: Term with Subs[F], C <: Term with Subs[C], Index <: HList, IF <: Term with Subs[IF], IDF <: Term with Subs[IDF], IDFT <: Term with Subs[IDFT]] {

  /**
   * the underlying pattern (to access methods defined on it)
   */
  val pattern: TypFamilyPtn[H, F, Index]

  /**
   * returns HoTT  type for iterated functions
   */
  def iterFuncTyp(w: Typ[H], x: Typ[C]): Typ[IF]

  /**
   * returns HoTT  type for iterated dependent functions
   */
  def iterDepFuncTyp(w: Typ[H], xs: IDFT): Typ[IDF]

  /**
   * returns iterated function given functions for each index
   */
  def iterFunc(funcs: Index => Func[H, C]): IF

  /**
   * returns iterated dependent function given functions for each index
   */
  def iterDepFunc(funcs: Index => FuncLike[H, C]): IDF

  /**
   * restricts iterated function to an index
   */
  def restrict(f: IF, ind: Index): Func[H, C]

  /**
   * restricts iterated dependent function to an index
   */
  def depRestrict(f: IDF, ind: Index): FuncLike[H, C]

  /**
   * restricts iterated type  family to an index
   */
  def typRestrict(xs: IDFT, ind: Index): Func[H, Typ[C]]

  def subs(x: Term, y: Term): TypFamilyMap[H, F, C, Index, IF, IDF, IDFT]
}

object TypFamilyMap {

  import TypFamilyPtn._

  case class IdTypFamilyMap[H <: Term with Subs[H], C <: Term with Subs[C]]()
    extends TypFamilyMap[H, Typ[H], C, HNil, Func[H, C], FuncLike[H, C], Func[H, Typ[C]]] {

    val pattern = IdTypFamily[H]

    def iterFuncTyp(w: Typ[H], x: Typ[C]) = w ->: x

    def iterDepFuncTyp(w: Typ[H], xs: Func[H, Typ[C]]) = PiDefn(xs)

    def iterFunc(funcs: HNil => Func[H, C]) = funcs(HNil)

    def iterDepFunc(funcs: HNil => FuncLike[H, C]) = funcs(HNil)

    def restrict(f: Func[H, C], ind: HNil) = f

    def depRestrict(f: FuncLike[H, C], ind: HNil) = f

    def typRestrict(xs: Func[H, Typ[C]], ind: HNil) = xs

    def subs(x: Term, y: Term) = this
  }

  case class IdSubTypFamilyMap[H <: Term with Subs[H], TC <: Typ[Term] with Subs[TC], C <: Term with Subs[C]]()(
    implicit
    subEv: TypObj[TC, C])
    extends TypFamilyMap[H, Typ[H], C, HNil, Func[H, C], FuncLike[H, C], Func[H, TC]] {

    val pattern = IdTypFamily[H]

    def iterFuncTyp(w: Typ[H], x: Typ[C]) = w ->: x

    def iterDepFuncTyp(w: Typ[H], xs: Func[H, TC]) = {
      val x = xs.dom.Var
      PiDefn(x, subEv.me(xs(x)): Typ[C])
    }

    def iterFunc(funcs: HNil => Func[H, C]) = funcs(HNil)

    def iterDepFunc(funcs: HNil => FuncLike[H, C]) = funcs(HNil)

    def restrict(f: Func[H, C], ind: HNil) = f

    def depRestrict(f: FuncLike[H, C], ind: HNil) = f

    def typRestrict(xs: Func[H, TC], ind: HNil) = {
      val x = xs.dom.Var
      x :-> (subEv.me(xs(x)): Typ[C])
    }

    def subs(x: Term, y: Term) = this
  }

  case class FuncTypFamilyMap[U <: Term with Subs[U], H <: Term with Subs[H], TF <: Term with Subs[TF], C <: Term with Subs[C], TIndex <: HList: TermList, TIF <: Term with Subs[TIF], TIDF <: Term with Subs[TIDF], TIDFT <: Term with Subs[TIDFT]](
    head: Typ[U],
    tail: TypFamilyMap[H, TF, C, TIndex, TIF, TIDF, TIDFT])
    extends TypFamilyMap[H, Func[U, TF], C, U :: TIndex, FuncLike[U, TIF], FuncLike[U, TIDF], FuncLike[U, TIDFT]] {

    val pattern = FuncTypFamily(head, tail.pattern)

    def iterFuncTyp(w: Typ[H], x: Typ[C]) = head ->: tail.iterFuncTyp(w, x)

    def iterDepFuncTyp(w: Typ[H], xs: FuncLike[U, TIDFT]) = {
      val x = head.Var
      x ~>: (tail.iterDepFuncTyp(w, xs(x)))
    }

    def iterFunc(funcs: (U :: TIndex) => Func[H, C]) = {
      val x = head.Var
      x :~> (tail.iterFunc((ti: TIndex) => funcs(x :: ti)))
    }

    def iterDepFunc(funcs: ((U :: TIndex)) => FuncLike[H, C]) = {
      val x = head.Var
      x :~> (tail.iterDepFunc((ti: TIndex) => funcs(x :: ti)))
    }

    def restrict(f: FuncLike[U, TIF], ind: (U :: TIndex)) =
      tail.restrict(f(ind.head), ind.tail)

    def depRestrict(f: FuncLike[U, TIDF], ind: (U :: TIndex)) =
      tail.depRestrict(f(ind.head), ind.tail)

    def typRestrict(xs: FuncLike[U, TIDFT], ind: (U :: TIndex)) =
      tail.typRestrict(xs(ind.head), ind.tail)

    def subs(x: Term, y: Term) =
      FuncTypFamilyMap(head.replace(x, y), tail.subs(x, y))
  }

  case class DepFuncTypFamilyMap[U <: Term with Subs[U], H <: Term with Subs[H], TF <: Term with Subs[TF], C <: Term with Subs[C], TIndex <: HList: TermList, TIF <: Term with Subs[TIF], TIDF <: Term with Subs[TIDF], TIDFT <: Term with Subs[TIDFT]](
    head: Typ[U],
    tailfibre: U => TypFamilyMap[H, TF, C, TIndex, TIF, TIDF, TIDFT])
    extends TypFamilyMap[H, FuncLike[U, TF], C, (U :: TIndex), FuncLike[U, TIF], FuncLike[U, TIDF], FuncLike[U, TIDFT]] {

    override lazy val hashCode =
      (head, tailfibre(head.symbObj(HashSym))).hashCode

    override def equals(that: Any) = that match {
      case df: DepFuncTypFamilyMap[u, a, b, c, d, e, f, g] =>
        head == df.head && {
          val x = head.Var
          tailfibre(x) == df.tailfibre(x.asInstanceOf[u])
        }
      case _ => false
    }

    val pattern = DepFuncTypFamily(head, (u: U) => tailfibre(u).pattern)

    def iterFuncTyp(w: Typ[H], x: Typ[C]) = {
      val y = head.Var
      y ~>: tailfibre(y).iterFuncTyp(w, x)
    }

    def iterDepFuncTyp(w: Typ[H], xs: FuncLike[U, TIDFT]) = {
      val x = head.Var
      x ~>: (tailfibre(x).iterDepFuncTyp(w, xs(x)))
    }

    def iterFunc(funcs: ((U :: TIndex)) => Func[H, C]) = {
      val x = head.Var
      x :~> (tailfibre(x).iterFunc((ti: TIndex) => funcs(x :: ti)))
    }

    def iterDepFunc(funcs: ((U :: TIndex)) => FuncLike[H, C]) = {
      val x = head.Var
      x :~> (tailfibre(x).iterDepFunc((ti: TIndex) => funcs(x :: ti)))
    }

    def restrict(f: FuncLike[U, TIF], ind: (U :: TIndex)) =
      tailfibre(ind.head).restrict(f(ind.head), ind.tail)

    def depRestrict(f: FuncLike[U, TIDF], ind: (U :: TIndex)) =
      tailfibre(ind.head).depRestrict(f(ind.head), ind.tail)

    def typRestrict(xs: FuncLike[U, TIDFT], ind: (U :: TIndex)) =
      tailfibre(ind.head).typRestrict(xs(ind.head), ind.tail)

    def subs(x: Term, y: Term) =
      DepFuncTypFamilyMap(
        head.replace(x, y),
        (u: U) => tailfibre(u).subs(x, y))
  }
}

/**
 * aid for implicit calculations:
 * given a scala type that is a subtype of `Typ[C]`, recovers `C`,
 * eg shows that `FuncTyp[A, B]` is a subtype of `Typ[Func[A, B]]`
 *
 * Note that `C` is not unique, indeed `C = Term` is  always a solution,
 * so we seek the best `C`
 */
class TypObj[T <: Typ[Term] with Subs[T], C <: Term with Subs[C]](
  implicit
  ev: T <:< Typ[C]) {
  def me(t: T): Typ[C] = t
}

object TypObj {
  implicit def tautology[U <: Term with Subs[U]]: TypObj[Typ[U], U] =
    new TypObj[Typ[U], U]

  implicit def func[U <: Term with Subs[U], V <: Term with Subs[V]]: TypObj[FuncTyp[U, V], Func[U, V]] = new TypObj[FuncTyp[U, V], Func[U, V]]

  implicit def funclike[U <: Term with Subs[U], V <: Term with Subs[V]]: TypObj[GenFuncTyp[U, V], FuncLike[U, V]] =
    new TypObj[GenFuncTyp[U, V], FuncLike[U, V]]

  implicit def pi[U <: Term with Subs[U], V <: Term with Subs[V]]: TypObj[PiDefn[U, V], FuncLike[U, V]] =
    new TypObj[PiDefn[U, V], FuncLike[U, V]]

  implicit def pair[U <: Term with Subs[U], V <: Term with Subs[V]]: TypObj[ProdTyp[U, V], PairTerm[U, V]] =
    new TypObj[ProdTyp[U, V], PairTerm[U, V]]

  /**
   * given the object `fmly` of scala type `TC` that is a subtype of `Typ[C]`, recovers `C`,
   * eg shows that `FuncTyp[A, B]` is a subtype of `Typ[Func[A, B]]`
   *
   * Note that `C` is not unique, indeed `C = Term` is  always a solution,
   * so we seek the best `C`
   */
  def solve[TC <: Typ[Term] with Subs[TC], C <: Term with Subs[C]](fmly: TC)(
    implicit
    tpObj: TypObj[TC, C]) = (tpObj.me(fmly): Typ[C])
}

/**
 * bridge between [[TypFamilyPtn]] and [[TypFamilyMap]]
 */
sealed trait TypFamilyMapper[H <: Term with Subs[H], F <: Term with Subs[F], C <: Term with Subs[C], Index <: HList, IF <: Term with Subs[IF], IDF <: Term with Subs[IDF], IDFT <: Term with Subs[IDFT]] {

  val mapper: TypFamilyPtn[H, F, Index] => TypFamilyMap[H, F, C, Index, IF, IDF, IDFT]
}

trait WeakImplicit {
  implicit def idSubTypFamilyMapper[H <: Term with Subs[H], TC <: Typ[Term] with Subs[TC], C <: Term with Subs[C]](
    implicit
    subEv: TypObj[TC, C]) =
    new TypFamilyMapper[H, Typ[H], C, HNil, Func[H, C], FuncLike[H, C], Func[H, TC]] {
      val mapper = (x: TypFamilyPtn[H, Typ[H], HNil]) =>
        TypFamilyMap.IdSubTypFamilyMap[H, TC, C]
    }
}

object TypFamilyMapper extends WeakImplicit {
  import TypFamilyMap._

  import TypFamilyPtn._

  implicit def idTypFamilyMapper[H <: Term with Subs[H], C <: Term with Subs[C]] =
    new TypFamilyMapper[H, Typ[H], C, HNil, Func[H, C], FuncLike[H, C], Func[H, Typ[C]]] {
      val mapper = (x: TypFamilyPtn[H, Typ[H], HNil]) => IdTypFamilyMap[H, C]
    }

  implicit def funcTypFamilyMapper[U <: Term with Subs[U], H <: Term with Subs[H], TF <: Term with Subs[TF], C <: Term with Subs[C], TIndex <: HList, TIF <: Term with Subs[TIF], TIDF <: Term with Subs[TIDF], TIDFT <: Term with Subs[TIDFT]](
    implicit
    tail: TypFamilyMapper[H, TF, C, TIndex, TIF, TIDF, TIDFT],
    subst: TermList[TIndex]) =
    new TypFamilyMapper[H, Func[U, TF], C, (U :: TIndex), FuncLike[U, TIF], FuncLike[U, TIDF], FuncLike[U, TIDFT]] {

      val mapper: TypFamilyPtn[H, Func[U, TF], (U :: TIndex)] => TypFamilyMap[H, Func[U, TF], C, (U :: TIndex), FuncLike[U, TIF], FuncLike[U, TIDF], FuncLike[U, TIDFT]] = {
        case FuncTypFamily(h, t) =>
          FuncTypFamilyMap(h, tail.mapper(t))
      }
    }

  implicit def depFuncTypFamilyMapper[U <: Term with Subs[U], H <: Term with Subs[H], TF <: Term with Subs[TF], C <: Term with Subs[C], TIndex <: HList, TIF <: Term with Subs[TIF], TIDF <: Term with Subs[TIDF], TIDFT <: Term with Subs[TIDFT]](
    implicit
    tail: TypFamilyMapper[H, TF, C, TIndex, TIF, TIDF, TIDFT],
    subst: TermList[TIndex]) =
    new TypFamilyMapper[H, FuncLike[U, TF], C, (U :: TIndex), FuncLike[U, TIF], FuncLike[U, TIDF], FuncLike[U, TIDFT]] {

      val mapper: TypFamilyPtn[H, FuncLike[U, TF], (U :: TIndex)] => TypFamilyMap[H, FuncLike[U, TF], C, (U :: TIndex), FuncLike[U, TIF], FuncLike[U, TIDF], FuncLike[U, TIDFT]] = {
        case DepFuncTypFamily(h, tf) =>
          DepFuncTypFamilyMap(h, (u: U) => tail.mapper(tf(u)))
      }
    }
}

trait TypFamilyPtnGetter[F <: Term with Subs[F], H <: Term with Subs[H], Index <: HList] {

  //  type Index

  def get(w: F): TypFamilyPtn[H, F, Index]

  implicit val subst: TermList[Index]
}

object TypFamilyPtnGetter {
  implicit def idGetter[H <: Term with Subs[H]]: TypFamilyPtnGetter[Typ[H], H, HNil] =
    new TypFamilyPtnGetter[Typ[H], H, HNil] {
      //      type Index = HNil

      def get(w: Typ[H]) = TypFamilyPtn.IdTypFamily[H]

      val subst = TermList.HNilTermList
    }

  implicit def funcTypFamilyGetter[TF <: Term with Subs[TF], U <: Term with Subs[U], H <: Term with Subs[H], TI <: HList](
    implicit
    tail: TypFamilyPtnGetter[TF, H, TI]): TypFamilyPtnGetter[Func[U, TF], H, U :: TI] =
    new TypFamilyPtnGetter[Func[U, TF], H, U :: TI] {

      def get(w: Func[U, TF]) =
        TypFamilyPtn.FuncTypFamily(w.dom, tail.get(w(w.dom.Var)))

      // type Index = (U :: TI)

      implicit val ts: TermList[TI] = tail.subst

      val subst = TermList.hConsTermList[U, TI]
    }

  implicit def depFuncTypFamilyGetter[TF <: Term with Subs[TF], U <: Term with Subs[U], H <: Term with Subs[H], TI <: HList](
    implicit
    tail: TypFamilyPtnGetter[TF, H, TI]): TypFamilyPtnGetter[FuncLike[U, TF], H, U :: TI] =
    new TypFamilyPtnGetter[FuncLike[U, TF], H, U :: TI] {
      def get(w: FuncLike[U, TF]) =
        TypFamilyPtn.DepFuncTypFamily(w.dom, (u: U) => tail.get(w(u)))

      // type Index = (U, tail.Index)

      implicit val ts: TermList[TI] = tail.subst

      val subst = TermList.hConsTermList[U, TI]
    }
}
