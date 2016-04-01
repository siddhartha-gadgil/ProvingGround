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
  
  implicit object OptFunctor extends Functor[Option]{
    def map[A, B](fa: Option[A])(f: A => B) = fa map (f)
  }
  
  class ComposeFunctors[X[_]: Functor, Y[_] : Functor]{
    type Z[A] = X[Y[A]]
    def Func : Functor[Z] = new Functor[Z]{
      def map[A, B](fa: X[Y[A]])(f: A => B) = 
      {
        val inner = (y : Y[A]) => implicitly[Functor[Y]].map(y)(f)
        implicitly[Functor[X]].map(fa)(inner)
      }
    }
  }
  
  implicit def composeFunctors[X[_]: Functor, Y[_] : Functor] =
    new ComposeFunctors[X, Y].Func
  
  class ConstFunc[C]{
    type Z[A] = C
    def Func : Functor[Z] = new Functor[Z]{
      def map[A, B](fa: C)(f: A => B) = fa
    }
  }
  
  implicit def constantFunctor[C] = 
    new ConstFunc[C].Func
    
  type Weight[A] = (A, Double)
  
  implicit object WeightFunctor extends Functor[Weight]{
    def map[A, B](fa: (A, Double))(f: A => B)  = (f(fa._1), fa._2)
  }
    
  class T2[X[_]: Functor, Y[_] : Functor]{
    type Z[A] = (X[A], Y[A])
    def Func : Functor[Z] = new Functor[Z]{
      def map[A, B](fa: (X[A], Y[A]))(f: A => B) = 
      (implicitly[Functor[X]].map(fa._1)(f), implicitly[Functor[Y]].map(fa._2)(f))
    }        
  }
  
  implicit def tuple2[X[_]: Functor, Y[_] : Functor] =  
    new T2[X, Y].Func

    
  class T3[X1[_]: Functor, X2[_] : Functor, X3[_]: Functor]{
    type Z[A] = (X1[A], X2[A], X3[A])
    def Func : Functor[Z] = new Functor[Z]{
      def map[A, B](fa: (X1[A], X2[A], X3[A]))(f: A => B) = 
      (implicitly[Functor[X1]].map(fa._1)(f), implicitly[Functor[X2]].map(fa._2)(f), implicitly[Functor[X3]].map(fa._3)(f))
    }
    
  implicit def tuple3[X1[_]: Functor, X2[_] : Functor, X3[_]: Functor] = 
    new T3[X1, X2, X3].Func
  }  
  
  
  class T4[X1[_]: Functor, X2[_] : Functor, X3[_]: Functor, X4[_]: Functor]{
    type Z[A] = (X1[A], X2[A], X3[A], X4[A])
    def Func : Functor[Z] = new Functor[Z]{
      def map[A, B](fa: (X1[A], X2[A], X3[A], X4[A]))(f: A => B) = 
      (implicitly[Functor[X1]].map(fa._1)(f), 
          implicitly[Functor[X2]].map(fa._2)(f), 
          implicitly[Functor[X3]].map(fa._3)(f),
          implicitly[Functor[X4]].map(fa._4)(f))
    }
    
  implicit def tuple4[X1[_]: Functor, X2[_] : Functor, X3[_]: Functor, X4[_]: Functor] = 
    new T4[X1, X2, X3, X4].Func
  }
  
  class T5[X1[_]: Functor, X2[_] : Functor, X3[_]: Functor, X4[_]: Functor, X5[_]: Functor]{
    type Z[A] = (X1[A], X2[A], X3[A], X4[A], X5[A])
    def Func : Functor[Z] = new Functor[Z]{
      def map[A, B](fa: (X1[A], X2[A], X3[A], X4[A], X5[A]))(f: A => B) = 
      (implicitly[Functor[X1]].map(fa._1)(f), 
          implicitly[Functor[X2]].map(fa._2)(f), 
          implicitly[Functor[X3]].map(fa._3)(f),
          implicitly[Functor[X4]].map(fa._4)(f),
          implicitly[Functor[X5]].map(fa._5)(f))
    }
    
  implicit def tuple5[X1[_]: Functor, X2[_] : Functor, X3[_]: Functor, X4[_]: Functor, X5[_]: Functor] = 
    new T5[X1, X2, X3, X4, X5].Func
  }
  
  type LL[A] = (List[A], List[A]); 

  type IL[A] = (Id[A], List[A]); 
  
  type II[A] = (Id[A], Id[A]);
  
  // Tests:  
object Tests{
  val ll = implicitly[Functor[LL]]
  
  val li = implicitly[Functor[IL]]
  
   val ii = implicitly[Functor[II]]
  
  val xx : IL[Int] = (1, List(1, 2))
  
  val a = liftMap(List(1, 2, 3), (n: Int) => n + 1)
  
  val b = liftMap[Int, Int, LL]((List(1), List(2)), (n: Int) => n +1)
  
  val c = liftMap[Int, Int, IL]((3, List(1, 2)), (n: Int) => n+1)
  }
}