package provingground 

import shapeless._, HoTT._

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
      def subst(f: Term => A)(x: Term, y: Term): Term => A =
        (t: Term) => Subst[A].subst(f(t))(x, y)
    }

  case class Lambda[T <: Term with Subs[T], A](variable: T, value: A)(
      implicit s: Subst[A]
  ) extends (T => A) {
    def apply(t: T): A = s.subst(value)(variable, t)

    override lazy val hashCode: Int = {
      val newvar = variable.typ.symbObj(Name("hash"))
      val newval = s.subst(value)(variable, newvar)
      41 * (variable.typ.hashCode + 41) + newval.hashCode
    }

    override def equals(that: Any): Boolean = that match {
      case l: Lambda[u, v] if l.variable.typ == variable.typ =>
        s.subst(value)(l.variable, variable) == value &&
          s.subst(value)(variable, l.variable) == l.value
      case _ => false
    }

    override def toString = s"$variable ${UnicodeSyms.MapsTo} $value"
  }

  object Lambda {
    implicit def substInstance[T <: Term with Subs[T], A](
        implicit s: Subst[A]
    ): Subst[Lambda[T, A]] =
      new Subst[Lambda[T, A]] {
        def subst(l: Lambda[T, A])(x: Term, y: Term) =
          Lambda(l.variable.replace(x, y), s.subst(l.value)(x, y))
      }
  }
}

/**
  * allows substitution of a `Term` by another, as well as mapping to a vector of terms
  * chiefly subtypes of `Term` and `HList`s of these;
  *
  */
sealed trait TermList[A] extends Subst[A] {

  def terms(a: A): Vector[Term]
}

object TermList extends TermListImplicits {
  def apply[A: TermList]: TermList[A] = implicitly[TermList[A]]

  implicit def termTermList[U <: Term with Subs[U]]: TermList[U] =
    new TermList[U] {
      def subst(a: U)(x: Term, y: Term): U = a.replace(x, y)

      def terms(a: U): Vector[U] = Vector(a)
    }

  implicit object HNilTermList extends TermList[HNil] {
    def subst(a: HNil)(x: Term, y: Term): HNil = a

    def terms(a: HNil): Vector[Term] = Vector()
  }

  implicit def hConsTermList[U: TermList, V <: HList: TermList]
      : TermList[U :: V] =
    new TermList[U :: V] {
      def subst(a: U :: V)(x: Term, y: Term): U :: V =
        implicitly[TermList[U]].subst(a.head)(x, y) :: implicitly[TermList[V]]
          .subst(a.tail)(x, y)

      def terms(a: U :: V): Vector[Term] =
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
    def subst(x: Term, y: Term): A = implicitly[Subst[A]].subst(a)(x, y)

    def ~->:(x: Term): Subst.Lambda[Term, A] = Subst.Lambda(x, a)
    // (y: Term) => implicitly[Subst[A]].subst(a)(x, y)
  }
}

trait TermListImplicits extends SubstImplicits {
  implicit class TermListOp[A: TermList](a: A) extends SubstOp(a) {
    // def subst(x: Term, y: Term) = implicitly[Subst[A]].subst(a)(x, y)

    def terms: Vector[Term] = implicitly[TermList[A]].terms(a)
  }

}
