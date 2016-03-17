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
  
}