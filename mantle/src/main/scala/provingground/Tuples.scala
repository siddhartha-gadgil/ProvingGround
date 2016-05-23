package provingground
import HoTT._
import scala.reflect.runtime.universe.{Try => UnivTry, Function => FunctionUniv, _}
import scala.language.implicitConversions

object Tuples {

  trait TermTuple[TermType <: Term with Subs[TermType],
                  F <: Term with Subs[F],
                  T <: Term with Subs[T]]
      extends MapsTo[F, T] {

    val term: TermType with Subs[TermType]

    def subs(x: Term, y: Term): TermTuple[TermType, F, T]

    def prepend[U <: Term with Subs[U]](head: U) = DepPairCons(head, this)

    def +:[U <: Term with Subs[U]](head: U) = prepend(head)
  }

  implicit def getTerm[
      U <: Term with Subs[U], F <: Term with Subs[F], T <: Term with Subs[T]](
      tuple: TermTuple[U, F, T]): U = tuple.term

  trait MapsTo[F, T] {
    type Func = F

    type Target = T

    def mapsTo(target: T): F
  }

  case class Singleton[U <: Term with Subs[U]](head: U)
      extends SingleEnv[U, Term](head) {
    def on[T <: Term with Subs[T]] = new SingleEnv[U, T](term)
  }

  class SingleEnv[U <: Term with Subs[U], T <: Term with Subs[T]](head: => U)
      extends TermTuple[U, FuncLike[U, T], T] {

    lazy val term = head

    def subs(x: Term, y: Term) = new SingleEnv(term.subs(x, y))

    def mapsTo(target: T) = lambda(term)(target)
  }

  implicit def singleton[U <: Term with Subs[U]](term: U) = Singleton(term)

  case class PairCons[U <: Term with Subs[U],
                      W <: Term with Subs[W],
                      F <: Term with Subs[F],
                      T <: Term with Subs[T]](
      head: U,
      tail: TermTuple[W, F, T]
  )
      extends TermTuple[PairObj[U, W], FuncLike[U, F], T] {

    lazy val term = PairObj(head, tail.term)

    def subs(x: Term, y: Term) = PairCons(head.subs(x, y), tail.subs(x, y))

    def mapsTo(target: T) = lambda(head)(tail.mapsTo(target))
  }

  case class DepPairCons[U <: Term with Subs[U],
                         W <: Term with Subs[W],
                         F <: Term with Subs[F],
                         T <: Term with Subs[T]](
      head: U,
      tail: TermTuple[W, F, T]
  )
      extends TermTuple[DepPair[U, W], FuncLike[U, F], T] {

    lazy val term = lambdaPair(head)(tail.term)

    def subs(x: Term, y: Term) = DepPairCons(head.subs(x, y), tail.subs(x, y))

    def mapsTo(target: T) = lambda(head)(tail.mapsTo(target))
  }
}
