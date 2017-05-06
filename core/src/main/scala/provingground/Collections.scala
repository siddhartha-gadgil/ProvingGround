package provingground

import scala.annotation._
import scala.util._

import scala.language.implicitConversions

object Collections {
  // lazy val random = new Random

  implicit val ZeroReal: Double = 0

  implicit def ZeroPair[A, B](za: A, zb: B): (A, B) = (za, zb)

  trait InfSeq[T] {
    val head: T
    val tail: InfSeq[T]

    @tailrec final def apply(n: Int): T = {
      require(n >= 0); if (n == 0) head else tail.apply(n - 1)
    }

    @tailrec final def drop(n: Int): InfSeq[T] =
      if (n < 1) this else tail.drop(n - 1)

    def take(n: Int) = for (i <- 0 to (n - 1)) yield (apply(i))

    def #::(head: T): InfSeq[T] = InfSeq.cons(head, this)

    def map[S](f: T => S): InfSeq[S] = InfSeq.cons(f(head), tail.map(f))

    /*
     * This is a diagonal, not a true flatMap, so works only in some cases: approximations, independent random variables.
     */
    def flatMap[S](f: T => InfSeq[S]): InfSeq[S] = InfSeq.diagonal(map(f))
  }

  object InfSeq {
    class fromFunction[T](seq: => (Int => T)) extends InfSeq[T] {
      lazy val head = seq(0)
      lazy val tail = new fromFunction((n: Int) => seq(n + 1))
    }

    def cons[T](head: T, tail: => InfSeq[T]) = {
      def seq(n: Int) = if (n < 1) head else tail(n - 1)
      new fromFunction(seq)
    }

    def apply[T](seq: => (Int => T)) = new fromFunction(seq)

    def diagonal[T](seq: InfSeq[InfSeq[T]]): InfSeq[T] =
      cons(seq.head.head, diagonal(seq.tail.tail))
  }

  trait ApproxSeq[T] extends InfSeq[T] {
    val stable: Boolean

    val tail: ApproxSeq[T]

    override def #::(head: T) = ApproxSeq.cons(head, this)

    override def map[S](f: T => S): ApproxSeq[S] =
      ApproxSeq.cons(f(head), tail.map(f))
  }

  /*
   * Does this change on disc?
   */

  object ApproxSeq {
    case class Const[T](t: T) extends ApproxSeq[T] {
      lazy val head = t
      val stable    = true

      lazy val tail = this
    }

    class Prepend[T](h: => T, t: => ApproxSeq[T], val stable: Boolean = false)
        extends ApproxSeq[T] {
      lazy val head = h
      lazy val tail = t
    }

    def cons[T](h: => T, t: => ApproxSeq[T], stable: Boolean = false) =
      new Prepend(h, t, stable)

    case class Dyn[T](init: T, dyn: T => T, halt: T => Boolean)
        extends ApproxSeq[T] {
      lazy val head = init

      lazy val stable = halt(init)

      lazy val tail: ApproxSeq[T] =
        if (stable) Const(init) else Dyn(dyn(init), dyn, halt)
    }

    def diagonal[T](seq: ApproxSeq[ApproxSeq[T]]): ApproxSeq[T] =
      cons(seq.head.head,
           diagonal(seq.tail.tail),
           seq.stable && seq.head.stable)
  }

  /*
    trait ProbabilityDistribution[T] extends Any{self =>
      def next: T

      def iid : InfSeq[T] = InfSeq((_ : Int) => next)

      def map[S](f: T => S) : ProbabilityDistribution[S]

      def flatMap[S](f: T => ProbabilityDistribution[S]) = ProbabilityDistribution(f(next).next)
    }

    trait ProbMap[T] extends ProbabilityDistribution[T]{
      def map[S](f: T => S) = ProbabilityDistribution(f(next))
    }

    object ProbabilityDistribution{
      def apply[T](nxt: => T) = new ProbabilityDistribution[T] with ProbMap[T]{
        def next = nxt
      }
    }

    class Uniform extends ProbabilityDistribution[Double] with ProbMap[Double]{


      def next = random.nextDouble
    }

   */

  case class InnerProduct[V](dot: (V, V) => Double)

  implicit class DotOp[V: InnerProduct](a: V) {
    def |*|(b: V) = implicitly[InnerProduct[V]].dot(a, b)
  }

  def vdot[V](implicit ip: InnerProduct[V]) = ip.dot

  implicit val realInnerProd = InnerProduct[Double](_ * _)

  implicit def InnerProductPairs[A, B](implicit ipA: InnerProduct[A],
                                       ipB: InnerProduct[B]) = {
    InnerProduct[(A, B)]((x, y) => ipA.dot(x._1, y._1) + ipB.dot(x._2, y._2))
  }

  trait LabelledArray[L, T] extends Any {
    def support: Traversable[L]

    def foreach[U](f: T => U): Unit = support.map(get(_).get).foreach(f)

    def get(label: L): Option[T]

    def apply(label: L)(implicit zero: T) = get(label).getOrElse(zero)

    def incl(label: L)(arg: T) = ArrayMap(Map(label -> arg))

    def proj(label: L)(arr: ArrayMap[L, T])(implicit zero: T) =
      arr(label)(zero)
    /*
      def inclusion(label: L)(implicit zero: T) = {
        require (!((support find (_ == label)).isEmpty))

        AdjDiffbleFunction[T, ArrayMap[L, T]](incl(label))((_ : T) => proj(label))
      }*/
  }

  trait LabelledVector[L] extends Any with LabelledArray[L, Double] {
    def sum = (for (l <- support; value <- get(l)) yield value).sum

    def innerProduct(that: LabelledVector[L]) =
      (for (l <- support; fst <- get(l); scnd <- that.get(l))
        yield fst * scnd).sum

    def dot(that: LabelledVector[L]) = innerProduct(that)
  }

  case class ArrayMap[L, T](coords: Map[L, T],
                            supp: Option[Traversable[L]] = None)
      extends LabelledArray[L, T] {
    lazy val support = supp getOrElse (coords.keys)

    def get(label: L) = coords.get(label)

    def map[S](f: T => S) = {
      val newmap = (for ((k, t) <- coords) yield (k -> f(t))).toMap
      ArrayMap(newmap, supp)
    }

    def total(implicit zero: T, ls: LinearStructure[T]) =
      coords.values.foldLeft(zero)(ls.sum(_, _))

    def ++(that: ArrayMap[L, T])(implicit zero: T, ls: LinearStructure[T]) = {
      val unionmap = (for (k <- coords.keySet.union(that.coords.keySet))
        yield (k -> ls.sum(this(k), that(k)))).toMap

      ArrayMap(unionmap, supp)
    }
  }

  implicit def ZeroMap[L, T]: ArrayMap[L, T] = ArrayMap(Map.empty: Map[L, T])

  implicit def VectorArray[L, T](
      implicit zero: T,
      ls: LinearStructure[T]): LinearStructure[ArrayMap[L, T]] = {
    def mult(sc: Double, arr: ArrayMap[L, T]) =
      arr map ((t: T) => ls.mult(sc, t))

    LinearStructure[ArrayMap[L, T]](ArrayMap(Map(), Some(List())),
                                    _ ++ _,
                                    mult)
  }

  implicit class Shift[B](shift: (B, B, Double) => B) {
    def apply(base: B, tang: B, sc: Double) = shift(base, tang, sc)
  }

  implicit def shiftFromVS[V](implicit ls: LinearStructure[V]) =
    Shift((base: V, tang: V, e: Double) => ls.diff(base, ls.mult(e, tang)))

  def update[B](init: B, tangent: B, epsilon: Double)(implicit s: Shift[B]) =
    s(init, tangent, epsilon)

  case class MultiSet[A](wts: Map[A, Int]) extends Set[A] {
    def weight(elem: A) = wts.getOrElse(elem, 0)

    lazy val support = wts.keySet

    // May not be needed - comes from iterator
    //     override def foreach[U](f: A => U): Unit = wts.keys.foreach((k) =>
    //       	(1 to wts(k)).foreach((j) =>
    //       	  f(k)))

    def ++(that: MultiSet[A]) = {
      val newmap = ((wts.keySet union that.wts.keySet) map
        ((k: A) => (k, weight(k) + that.weight(k)))).toMap
      MultiSet(newmap)
    }

    def +(elem: A) = {
      MultiSet(wts updated (elem, weight(elem) + 1))
    }

    def -(elem: A) = {
      MultiSet(wts updated (elem, math.min(weight(elem) - 1, 0)))
    }

    def contains(elem: A) = weight(elem) > 0

    def asSeq = (wts map (kn => (1 to kn._2) map (_ => kn._1))).flatten

    def iterator = asSeq.iterator
  }

  object MultiSet {
    def empty[A] = MultiSet[A](Map.empty)
  }

  /*
   * Should move this elsewhere
   */

  @tailrec def IterateDyn[A](init: A, step: A => A, n: Int): A =
    if (n < 1) init else IterateDyn(step(init), step, n - 1)

  @tailrec
  def transversal[A](arg: List[A],
                     equiv: (A, A) => Boolean,
                     accum: List[A] = List()): List[A] = arg match {
    case x :: ys =>
      if (accum contains ((a: A) => equiv(x, a))) transversal(ys, equiv, accum)
      else transversal(ys, equiv, x :: accum)
    case List() => accum
  }
}
