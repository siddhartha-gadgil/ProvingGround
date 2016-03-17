package provingground

import scala.language.higherKinds

import Functor.Id

trait OptNat[F[_]] {
  def optF[A](fo : F[Option[A]]): Option[F[A]]
}

object OptNat{
  def mkOpt[A, F[_] : OptNat](fo: F[Option[A]]) = 
    implicitly[OptNat[F]].optF(fo)
    
  implicit object ListON extends OptNat[List]{
    def optF[A](fo : List[Option[A]]) = 
      if (fo contains None) None else Some(fo.flatten)
  }
  
  implicit object IdON extends OptNat[Id]{
    def optF[A](fo : Option[A]) = fo
  }
  
  implicit object OptionON extends OptNat[Option]{
    def optF[A](fo : Option[Option[A]]) = fo
  }
  
  class ComposeON[X[_] : OptNat : Functor, Y[_] : OptNat]{
    type Z[A] = X[Y[A]]
    
    def OpN : OptNat[Z] = new OptNat[Z]{
      def optF[A](fo: Z[Option[A]]) = {
        val inner = implicitly[OptNat[Y]].optF[A] _
        val first = (implicitly[Functor[X]].map(_ : X[Y[Option[A]]])(inner))(fo)
        implicitly[OptNat[X]].optF(first)
      }
    }
  }
  
  class T2[X1[_] : OptNat, X2[_] : OptNat]{
    type Z[A] = (X1[A], X2[A])
    
    def OpN : OptNat[Z] = new OptNat[Z]{
      def optF[A](fo : Z[Option[A]]) = 
        for (
            x1<- implicitly[OptNat[X1]].optF(fo._1);
            x2<- implicitly[OptNat[X2]].optF(fo._2)) 
          yield (x1, x2)
    }
  }
  
  implicit def t2ON[X1[_] : OptNat, X2[_] : OptNat] = 
    new T2[X1, X2].OpN
    
    
  class T3[X1[_] : OptNat, X2[_] : OptNat, X3[_] : OptNat]{
    type Z[A] = (X1[A], X2[A], X3[A])
    
    def OpN : OptNat[Z] = new OptNat[Z]{
      def optF[A](fo : Z[Option[A]]) = 
        for (
            x1<- implicitly[OptNat[X1]].optF(fo._1);
            x2<- implicitly[OptNat[X2]].optF(fo._2);
            x3<- implicitly[OptNat[X3]].optF(fo._3)) 
          yield (x1, x2, x3)
    }
  }
  
  implicit def t3ON[X1[_] : OptNat, X2[_] : OptNat, X3[_] : OptNat] = 
    new T3[X1, X2, X3].OpN
    
  class T4[X1[_] : OptNat, X2[_] : OptNat, X3[_] : OptNat, X4[_] : OptNat]{
    type Z[A] = (X1[A], X2[A], X3[A], X4[A])
    
    def OpN : OptNat[Z] = new OptNat[Z]{
      def optF[A](fo : Z[Option[A]]) = 
        for (
            x1<- implicitly[OptNat[X1]].optF(fo._1);
            x2<- implicitly[OptNat[X2]].optF(fo._2);
            x3<- implicitly[OptNat[X3]].optF(fo._3);
            x4<- implicitly[OptNat[X4]].optF(fo._4)) 
          yield (x1, x2, x3, x4)
    }
  }
  
  implicit def t4ON[X1[_] : OptNat, X2[_] : OptNat, X3[_] : OptNat, X4[_] : OptNat] = 
    new T4[X1, X2, X3, X4].OpN
    
  class T5[X1[_] : OptNat, X2[_] : OptNat, X3[_] : OptNat, X4[_] : OptNat, X5[_] : OptNat]{
    type Z[A] = (X1[A], X2[A], X3[A], X4[A], X5[A])
    
    def OpN : OptNat[Z] = new OptNat[Z]{
      def optF[A](fo : Z[Option[A]]) = 
        for (
            x1<- implicitly[OptNat[X1]].optF(fo._1);
            x2<- implicitly[OptNat[X2]].optF(fo._2);
            x3<- implicitly[OptNat[X3]].optF(fo._3);
            x4<- implicitly[OptNat[X4]].optF(fo._4);
            x5<- implicitly[OptNat[X5]].optF(fo._5)) 
          yield (x1, x2, x3, x4, x5)
    }
  }
  
  implicit def t5ON[X1[_] : OptNat, X2[_] : OptNat, X3[_] : OptNat, X4[_] : OptNat, X5[_] : OptNat] = 
    new T5[X1, X2, X3, X4, X5].OpN
  
}