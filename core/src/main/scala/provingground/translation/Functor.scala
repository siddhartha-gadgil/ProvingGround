package provingground.translation

import scala.language.higherKinds

import shapeless.{Id => _, _}
import HList._

import cats._

//import cats.data.Prod

import cats.implicits._

/**
  * Equivalence of Functors (without laws)
  */
trait Equiv[X[_], Y[_]] {
  def map[A]: X[A] => Y[A]

  def inv[A]: Y[A] => X[A]
}

object Equiv {

  /**
    * Functor equivalent to itself
    */
  def idEquiv[X[_]]: Equiv[X, X] = new Equiv[X, X] {
    def map[A] = (xa) => xa

    def inv[A] = (xa) => xa
  }
}

/**
  * lower priority Functor and Traverese type classes to be extended in [[Functors]]
  */
trait CompositeFunctors {

  /**
    * Travese typeclass for `HCons`
    */
  implicit def traverseHCons[X[_], Y[_] <: HList](
      implicit tx: Lazy[Traverse[X]],
      YT: Traverse[Y]): Traverse[({ type Z[A] = X[A] :: Y[A] })#Z] =
    new Traverse[({ type Z[A] = X[A] :: Y[A] })#Z] {
      val XT = tx.value
      // val YT = implicitly[Traverse[Y]]

      type F[A] = X[A] :: Y[A]
      def traverse[G[_]: Applicative, A, B](fa: F[A])(
          f: A => G[B]): G[X[B] :: Y[B]] = {
        val GA = implicitly[Applicative[G]]
        val gy = YT.traverse(fa.tail)(f)
        val gx = XT.traverse(fa.head)(f)
        val gf = gy.map((y: Y[B]) => ((x: X[B]) => x :: y))
        GA.ap(gf)(gx)
      }
      def foldLeft[A, B](fa: X[A] :: Y[A], b: B)(f: (B, A) => B): B = {
        val fy = YT.foldLeft(fa.tail, b)(f)
        XT.foldLeft(fa.head, fy)(f)
      }
      def foldRight[A, B](fa: X[A] :: Y[A], lb: cats.Eval[B])(
          f: (A, cats.Eval[B]) => cats.Eval[B]): cats.Eval[B] = {
        val fxe = XT.foldRight(fa.head, lb)(f)
        YT.foldRight(fa.tail, fxe)(f)
      }
    }

  /**
    * Traverse type class for composition
    */
  implicit def traverseCompose[X[_]: Traverse, Y[_]: Traverse]
    : Traverse[({ type Z[A] = X[Y[A]] })#Z] =
    new Traverse[({ type Z[A] = X[Y[A]] })#Z] {
      type F[A] = X[Y[A]]

      val ty = implicitly[Traverse[Y]]

      val tx = implicitly[Traverse[X]]

      def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]] = {
        def g(y: Y[A]) = ty.traverse(y)(f)
        tx.traverse(fa)(g)
      }

      def foldLeft[A, B](fa: X[Y[A]], b: B)(f: (B, A) => B): B = {
        val g: (B, Y[A]) => B = { case (b, ya) => ty.foldLeft(ya, b)(f) }
        tx.foldLeft(fa, b)(g)
      }

      def foldRight[A, B](fa: X[Y[A]], lb: cats.Eval[B])(
          f: (A, cats.Eval[B]) => cats.Eval[B]): cats.Eval[B] = {
        val g: (Y[A], Eval[B]) => Eval[B] = {
          case (ya, b) => ty.foldRight(ya, b)(f)
        }
        tx.foldRight(fa, lb)(g)
      }
    }
}

/**
  * Functor and Traverse typeclasses for Tuples, HLists and Compositions of Functors,
  * and for Constant functors.
  *
  * To work around ambiguity in inference, for example
  * so that the type of iterated triples is interpreted as
  * `III(A) = (II(A), Id(A))`, not `III(A) = ((A, A), A)`
  * several functors are explicitly named with the appropriate structure.
  */
object Functors extends CompositeFunctors {
  // type T = List[?]

  /**
    * induced map, should use cats syntax instead
    */
  def liftMap[A, B, F[_]: Functor](fa: F[A], f: A => B) = {
    implicitly[Functor[F]].map(fa)(f)
  }

  /**
    * composition of functors is a functor
    */
  implicit def composeFunctors[X[_]: Functor, Y[_]: Functor]
    : Functor[({ type Z[A] = X[Y[A]] })#Z] =
    new Functor[({ type Z[A] = X[Y[A]] })#Z] {
      def map[A, B](fa: X[Y[A]])(f: A => B) = {
        val inner = (y: Y[A]) => implicitly[Functor[Y]].map(y)(f)
        implicitly[Functor[X]].map(fa)(inner)
      }
    }

  /**
    * constant functor
    */
  implicit def constantFunctor[Cn]: Functor[({ type Z[A] = Cn })#Z] =
    new Functor[({ type Z[A] = Cn })#Z] {
      def map[A, B](fa: Cn)(f: A => B) = fa
    }

  /**
    * functor by product with a constant functor
    */
  implicit def augmentedFunctor[Cn, X[_]: Functor]
    : Functor[({ type Z[A] = (Cn, X[A]) })#Z] =
    new Functor[({ type Z[A] = (Cn, X[A]) })#Z] {
      def map[A, B](fa: (Cn, X[A]))(f: A => B) =
        (fa._1, implicitly[Functor[X]].map(fa._2)(f))
    }

  /**
    * Functor identity with a name
    */
  type Named[A] = (S[A], Id[A])

  type Numbered[A] = (N[A], Id[A])

  /**
    * Traverse type class for identity with name
    */
  implicit val namedTrav: Traverse[Named] = traversePair[S, Id]

  implicit val numberedTrav: Traverse[Numbered] = traversePair[N, Id]

  /**
    * functor for pairs
    */
  implicit def t2[X[_]: Functor, Y[_]: Functor]
    : Functor[({ type Z[A] = (X[A], Y[A]) })#Z] =
    new Functor[({ type Z[A] = (X[A], Y[A]) })#Z] {
      def map[A, B](fa: (X[A], Y[A]))(f: A => B) =
        (implicitly[Functor[X]].map(fa._1)(f),
         implicitly[Functor[Y]].map(fa._2)(f))
    }

  /**
    * pair of Lists
    */
  type LL[A] = (List[A], List[A]);

  /**
    * pair of Vectors
    */
  type VV[A] = (Vector[A], Vector[A])

  /**
    * Identity product Vector
    */
  type IV[A] = (Id[A], Vector[A])

  /**
    * Vector product Identity
    */
  type VI[A] = (Vector[A], Id[A])

  /**
    * Vector product Option
    */
  type VO[A] = (Vector[A], Option[A])

  /**
    * triple `(Id(A), (Vector(A), Id(A)))`
    */
  type IVI[A] = (Id[A], VI[A])

  /**
    * triple `(Id(A), (Id(A), Vector(A)))`
    */
  type IIV[A] = (Id[A], IV[A])

  /**
    * 4-tuple `(Vector(A), (Id(A), (Id(A), Vector(A))))`
    */
  type VIIV[A] = (Vector[A], IIV[A])

  /**
    * 5-tuple `(Id(A), (Vector(A), (Id(A), (Id(A), Vector(A)))))`
    */
  type IVIIV[A] = (Id[A], VIIV[A])

  /**
    * triple `(Id(A), (Id(A), Id(A)))`
    */
  type III[A] = (II[A], Id[A])

  /**
    * 4-tuple `(Id(A), (Id(A), (Id(A), Vector(A))))`
    */
  type IIIV[A] = (Id[A], IIV[A])

  /**
    * triple `(String, (Vector(A), Id(A)))`
    */
  type SVI[A] = (S[A], VI[A])

  /**
    * triple `(Vector(A), (Id(A), Id(A)))`
    */
  type VII[A] = (Vector[A], II[A])

  /**
    * 4-tuple `(String, (Vector(A), (Id(A), Id(A))))`
    */
  type SVII[A] = (S[A], VII[A])

  /**
    * triple `(String, (Vector(A), Option(A)))`
    */
  type SVO[A] = (S[A], VO[A])

  /**
    * pair `(String, Vector(A))`
    */
  type SV[A] = (S[A], Vector[A])

  /**
    * Identity product with List
    */
  type IL[A] = (Id[A], List[A]);

  /**
    * Identity product itself
    */
  type II[A] = (Id[A], Id[A]);

  /**
    * traverse class for pairs
    */
  implicit def traversePair[X[_]: Traverse, Y[_]: Traverse]
    : Traverse[({ type Z[A] = (X[A], Y[A]) })#Z] =
    new Traverse[({ type Z[A] = (X[A], Y[A]) })#Z] {
      val XT = implicitly[Traverse[X]]
      val YT = implicitly[Traverse[Y]]

      type F[A] = (X[A], Y[A])
      def traverse[G[_]: Applicative, A, B](fa: F[A])(
          f: A => G[B]): G[(X[B], Y[B])] = {
        val GA = implicitly[Applicative[G]]
        val gy = YT.traverse(fa._2)(f)
        val gx = XT.traverse(fa._1)(f)
        val gf = gy.map((y: Y[B]) => ((x: X[B]) => (x, y)))
        GA.ap(gf)(gx)
      }
      def foldLeft[A, B](fa: (X[A], Y[A]), b: B)(f: (B, A) => B): B = {
        val fy = YT.foldLeft(fa._2, b)(f)
        XT.foldLeft(fa._1, fy)(f)
      }
      def foldRight[A, B](fa: (X[A], Y[A]), lb: cats.Eval[B])(
          f: (A, cats.Eval[B]) => cats.Eval[B]): cats.Eval[B] = {
        val fxe = XT.foldRight(fa._1, lb)(f)
        YT.foldRight(fa._2, fxe)(f)
      }
    }

  /**
    * Constant `HNil` functor
    */
  type HN[A] = HNil

  /**
    * `Identity` product `HNil`
    */
  type IdHN[A] = Id[A] :: HN[A]

  /**
    * Pair of `Identity` functors product `HNil`
    */
  type IdIdHN[A] = Id[A] :: IdHN[A]

  /**
    * Three copies of `Identity` Functor product `HNil`
    */
  type IdIdIdHN[A] = Id[A] :: IdIdHN[A]

  /**
    * Constant `String` Functor
    */
  type St[A] = String

  /**
    * String product with `HNil`
    */
  type StHN[A] = St[A] :: HN[A]

  /**
    * Triple `(String, (A, HNil))`
    */
  type StIdHN[A] = St[A] :: IdHN[A]

  /**
    * Constant `Int` functor
    */
  type In[A] = Int

  /**
    * Product of `Int` with `HNil`
    */
  type InHN[A] = In[A] :: HN[A]

  /**
    * triple `(String, (Int, HNil))`
    */
  type StIntHN[A] = St[A] :: InHN[A]

  /**
    * Traverse class induced by equivalence of Functors
    */
  implicit def traverseEquiv[F[_], Y[_]](implicit equiv: Equiv[F, Y],
                                         TY: Traverse[Y]): Traverse[F] =
    new Traverse[F] {
      def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]] =
        TY.traverse(equiv.map(fa))(f).map(equiv.inv)

      def foldLeft[A, B](fa: F[A], b: B)(f: (B, A) => B): B =
        TY.foldLeft(equiv.map(fa), b)(f)

      def foldRight[A, B](fa: F[A], lb: cats.Eval[B])(
          f: (A, cats.Eval[B]) => cats.Eval[B]): cats.Eval[B] =
        TY.foldRight(equiv.map(fa), lb)(f)
    }

  /**
    * constant functor `X`
    */
  type C[A, X] = X

  /**
    * constant `Int` functor
    */
  type N[A] = C[A, Int]

  /**
    * constant `String` functor
    */
  type S[A] = C[A, String]

  /**
    * constant `Unit` functor
    */
  type Un[A] = C[A, Unit]

  /**
    * triple `(String, (Int(A), List(A)))`
    */
  type Coded[A] = (S[A], (IL[A]))

  /**
    * Traverse typeclass for Constant functors
    */
  implicit def trCnst[X]: Traverse[({ type Z[A] = C[A, X] })#Z] =
    new Traverse[({ type F[A] = C[A, X] })#F] {
      type F[A] = C[A, X]
      def foldLeft[A, B](fa: X, b: B)(f: (B, A) => B): B = b
      def foldRight[A, B](fa: X, lb: cats.Eval[B])(
          f: (A, cats.Eval[B]) => cats.Eval[B]): cats.Eval[B] = lb

      // Members declared in cats.Traverse
      def traverse[G[_], A, B](fa: X)(f: A => G[B])(
          implicit evidence$1: cats.Applicative[G]): G[X] =
        implicitly[Applicative[G]].pure(fa)
    }

  implicit def trCod: Traverse[Coded] = traversePair[S, IL]

  // implicit val travIIV: Traverse[IIV] = traversePair[Id, IV]
  //
  // implicit val travVIIV: Traverse[VIIV] = traversePair[Vector, ]

  type Pickled = Coded[String]

}
