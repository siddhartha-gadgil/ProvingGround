package provingground

import scala.annotation._
import scala.util._

import scala.language.implicitConversions

import provingground.Collections._

import LinearStructure._

	trait DiffbleFunction[A, B]{self =>
    val func : A => B

    val grad: A => B => A

    def apply(a: A): B = func(a)
    	/**
    	 * Composition f *: g is f(g(_))
    	 */
    	def *:[C](that: => DiffbleFunction[B, C]) = andthen(that)

    	def andthen[C](that: => DiffbleFunction[B, C]): DiffbleFunction[A, C] =
        DiffbleFunction.Composition(this, that)
    	/**
    	 * Conjugate that by this.
    	 */
    	def ^:(that: B => B) = (a : A) => grad(a)(that(func(a)))

    	/**
    	 * post-compose by the gradient of this, for instance for a feedback.
    	 */
    	def **:(that: A => B) = (a : A) => grad(a)(that(a))

    	def oplus[C, D](that : DiffbleFunction[C, D]) =
      	DiffbleFunction.Oplus(this, that)

	}



    object DiffbleFunction{
      def apply[A, B](f: => A => B)(grd: => A => (B => A)) = new DiffbleFunction[A, B]{
        lazy val func = (a: A) => f(a)

        lazy val grad = grd
      }

      case class Composition[A, B, C](
          f: DiffbleFunction[A, B], g: DiffbleFunction[B, C]
          ) extends DiffbleFunction[A, C]{
        val func = (a: A) => g.func(f.func(a))

        val grad =
          (a: A) =>
            (c: C) =>
              f.grad(a)(g.grad(f.func(a))(c))
      }

      case class  Oplus[A, B, C, D](
          first: DiffbleFunction[A, B], second : DiffbleFunction[C, D]
          ) extends DiffbleFunction[(A, C), (B, D)] {
        val func = (ac: (A, C)) => (first.func(ac._1), second.func(ac._2))

        val grad = (ac: (A, C)) => (bd : (B, D)) => (first.grad(ac._1)(bd._1), second.grad(ac._2)(bd._2))
      }

      case class SelfAdj[A](func: A => A) extends DiffbleFunction[A, A]{
        val grad = (x: A) => (v: A) => func(v)
      }

//      def selfadj[A](f: => A => A) = SelfAdj(f)

      case class Id[A]() extends DiffbleFunction[A, A]{
        val func = (a: A) => a
        val grad = (a: A) => (b: A) => b
      }
      
      def id[A] : DiffbleFunction[A, A] = Id[A]

      case class Incl1[A, B: LinearStructure]() extends DiffbleFunction[A, (A, B)]{
        val lsB = implicitly[LinearStructure[B]]

        val func = (a: A) => (a, lsB.zero)
        val grad = (a: A) => (x: (A, B)) => x._1
      }

      case class Incl2[A :LinearStructure, B]() extends DiffbleFunction[B, (A, B)]{
        val lsA = implicitly[LinearStructure[A]]

        val func = (b: B) => (lsA.zero, b)
        val grad = (b: B) => (x: (A, B)) => x._2
      }

      case class Proj1[A, B: LinearStructure]() extends DiffbleFunction[(A, B), A]{
        val lsB = implicitly[LinearStructure[B]]

        val func = (x: (A, B)) => x._1
        val grad = (x : (A, B)) => (a: A) => (a, lsB.zero)
      }

      case class Proj2[A: LinearStructure, B]() extends DiffbleFunction[(A, B), B]{
        val lsA = implicitly[LinearStructure[A]]

        val func = (x: (A, B)) => x._2
        val grad = (x : (A, B)) => (b: B) => (lsA.zero, b)
      }


//      def Proj2[A, B](implicit lsA: LinearStructure[A]) = apply((x: (A, B)) => x._2)((x) => (b) => (lsA.zero, b))

      def block[A : LinearStructure,
        B : LinearStructure,
        C : LinearStructure,
        D : LinearStructure](f : DiffbleFunction[A, C], g: DiffbleFunction[B, D]) = {
            val add = vsum[DiffbleFunction[(A, B), (C, D)]]

            val p1 = Proj1[A, B]
            val p2 = Proj2[A, B]

            val i1 = Incl1[C, D]
            val i2 = Incl2[C, D]

            add(p1 andthen f andthen i1, p2 andthen g andthen i2)
      }

      case class ScProd[V : LinearStructure : InnerProduct]() extends DiffbleFunction[(Double, V), V]{
        val ls = implicitly[LinearStructure[V]]
        val ip = implicitly[InnerProduct[V]]

        val func = (av: (Double, V)) => ls.mult(av._1, av._2)

        val grad = (av: (Double, V)) => (w: V) => (ip.dot(av._2, w), ls.mult(av._1, w))

      }

      /**
       * raise a function to 2^(n -1) wrt composition, so for n = 0 we get identity and n = 1 gives f.
       */
      def repsquare[A : LinearStructure](f: DiffbleFunction[A, A]): Int => DiffbleFunction[A, A] = {
        case 0 => id[A]
        case 1 => f
        case n if n<0 =>
          vzero[DiffbleFunction[A, A]]
        case n =>
          {val rs = repsquare(f)
          rs(n-1) andthen (rs(n-1))
          }
      }

        /**
       * Iterate a differentiable function.
      */
      @tailrec def iterateDiffble[X](fn:  DiffbleFunction[X, X], n: Int, accum:  DiffbleFunction[X, X] = id[X]):  DiffbleFunction[X, X] = {
          if (n<1) accum else iterateDiffble(fn, n-1, accum andthen (fn :  DiffbleFunction[X, X]) )
        }

      def iterate[A](f: DiffbleFunction[A, A]) : Int => DiffbleFunction[A, A] = (n) => iterateDiffble(f, n)

      def mixinIsle[A : LinearStructure](f: DiffbleFunction[A, A],
          isle : DiffbleFunction[A, A] => DiffbleFunction[A, A],
          normalize: DiffbleFunction[A, A] = id[A]) = {
        val g = iterate(f)
        def h(m : Int) : DiffbleFunction[A, A] = m match {
        	case 0 => id[A]
        	case n if n<0 =>
        		vzero[DiffbleFunction[A, A]]
        	case n if n >0 =>
        		val dsum  = vsum[DiffbleFunction[A, A]]
        		dsum(g(n), isle(h(n -1))) andthen normalize
        }
        h _
      }

      /**
       * Big sum, with terms (via support) in general depending on the argument.
       */
      case class BigSum[A: LinearStructure, B: LinearStructure](
          fns: A => Traversable[DiffbleFunction[A, B]]
          ) extends DiffbleFunction[A, B] {
        val func = (a: A) => {
          val terms = for (f <- fns(a)) yield f.func(a)

          (terms :\ zeroB)(sumB)
        }

        private val zeroB = vzero[B]
        private val sumB = vsum[B]

        private val zeroA = vzero[A]
        private val sumA = vsum[A]

        val grad = (a: A) => (b: B) => {
          val terms = for (f <- fns(a)) yield f.grad(a)(b)
          (terms :\ zeroA)(sumA)
        }

      }

      case  class Diagonal[A : LinearStructure]() extends DiffbleFunction[A, (A, A)]{
        val  lsA = implicitly[LinearStructure[A]]

        val func = (a: A) => (a, a)

        val  grad = (a: A) => (v : (A, A)) => lsA.sum(v._1, v._2)
      }

     case class DotProd[A : LinearStructure, B : LinearStructure](sc: Double, vect: DiffbleFunction[A, B]) extends DiffbleFunction[A, B] {
        val prodB = vprod[B]

        val prodA = vprod[A]

        val func = (a: A) => prodB(sc, vect.func(a))

        val grad = (a: A) =>(b: B) => prodA(sc, vect.grad(a)(b))

      }

     case class Sum[A : LinearStructure, B : LinearStructure](
         first: DiffbleFunction[A, B], second: DiffbleFunction[A, B]) extends DiffbleFunction[A, B] {
        val sumB = vsum[B]

        val sumA = vsum[A]

        val func = (a: A) => sumB(first.func(a), second.func(a))

        val grad = (a: A) =>(b: B) => sumA(first.grad(a)(b), second.grad(a)(b))

      }

     case class Zero[A: LinearStructure, B: LinearStructure]() extends DiffbleFunction[A, B]{
        val zeroA = vzero[A]

        val zeroB = vzero[B]

        val func = (a: A) => zeroB

        val grad = (a: A) => (b: B) => zeroA
     }

      implicit def diffFnLS[A : LinearStructure, B : LinearStructure]
      : LinearStructure[DiffbleFunction[A, B]] = {


      val sum = (f : DiffbleFunction[A, B], g : DiffbleFunction[A, B]) => Sum[A, B](f, g)

      val zero = Zero[A, B]

      val mult = (c : Double, f : DiffbleFunction[A, B]) => DotProd(c, f)

      LinearStructure(zero, sum, mult)
    }

		trait FormalExtension[A] extends DiffbleFunction[A, A]{
			val func: A => A

			val grad = (a: A) => (b: A) => b
		}


    }
