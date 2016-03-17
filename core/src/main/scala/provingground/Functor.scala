package provingground

import scala.language.higherKinds

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B) : F[B]
}

object Functor{
  def liftMap[A, B, F[_] : Functor](fa: F[A], f: A => B) = {
    implicitly[Functor[F]].map(fa)(f)
  }
  
  type Id[A] = A
  
  implicit object IdFunctor extends Functor[Id]{
    def map[A, B](fa : A)(f: A => B) = f(fa)
  }
  
  implicit object ListFunctor extends Functor[List]{
    def map[A, B](fa: List[A])(f: A => B) = fa map (f)
  }
  
  class T2[X[_]: Functor, Y[_] : Functor]{
    type Z[A] = (X[A], Y[A])
    def Func : Functor[Z] = new Functor[Z]{
      def map[A, B](fa: (X[A], Y[A]))(f: A => B) = 
      (implicitly[Functor[X]].map(fa._1)(f), implicitly[Functor[Y]].map(fa._2)(f))
    }        
  }
  
  implicit def Tuple2[X[_]: Functor, Y[_] : Functor] =  
    new T2[X, Y].Func

  // Tests:  
object Tests{
  type LL[A] = (List[A], List[A]); val ll = implicitly[Functor[LL]]

  type LI[A] = (Id[A], List[A]); val li = implicitly[Functor[LI]]
  
  val xx : LI[Int] = (1, List(1, 2))
  
  val a = liftMap(List(1, 2, 3), (n: Int) => n + 1)
  
  val b = liftMap[Int, Int, LL]((List(1), List(2)), (n: Int) => n +1)
  
  val c = liftMap[Int, Int, LI]((3, List(1, 2)), (n: Int) => n+1)
  }
}