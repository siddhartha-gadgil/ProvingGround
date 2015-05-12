package algebra

import spire.algebra._
import spire.math._
import spire.implicits._
import scala.util._

/**
 * @author gadgil
 * 
 * Warning: this creates field operations but does not give actual fields
 * 
 */
object PointWise { 
  class MultiplicativeAbGroupStruct[A, F : MultiplicativeAbGroup] extends MultiplicativeAbGroup[A => F]{
    // Members declared in spire.algebra.MultiplicativeGroup   
    def div(x: A => F,y: A => F): A => F = (a) => x(a) / y(a)      
    // Members declared in spire.algebra.MultiplicativeMonoid   
    def one: A => F = (a) => implicitly[MultiplicativeAbGroup[F]].one
    // Members declared in spire.algebra.MultiplicativeSemigroup   
    def times(x: A => F,y: A => F): A => F = (a) => x(a) * y(a)
  } 
  
  import Interval._
  
  class IntervalMult[F : Field : Order] extends MultiplicativeAbGroup[Interval[F]]{
  /** As seen from class IntervalMult, the missing signatures are as follows. 
   *   *  For convenience, these are usable as stub implementations.  */  
    // Members declared in spire.algebra.MultiplicativeGroup   
    def div(x: Interval[F],y: Interval[F]) = x / y      
    // Members declared in spire.algebra.MultiplicativeMonoid   
    def one  = Interval.point(Field[F].one)      
    // Members declared in spire.algebra.MultiplicativeSemigroup   
    def times(x: Interval[F],y: Interval[F]) = x * y  

  }

  
  implicit def intervalField[F : Field : Order] = new Field[Interval[F]]{
    // Members declared in spire.algebra.AdditiveGroup 
    def negate(x: spire.math.Interval[F]): spire.math.Interval[F] = -x 
    // Members declared in spire.algebra.AdditiveMonoid 
    def zero: spire.math.Interval[F] = Interval.point(Field[F].zero)
    // Members declared in spire.algebra.AdditiveSemigroup 
    def plus(x: spire.math.Interval[F],y: spire.math.Interval[F]): spire.math.Interval[F] = x + y 
    // Members declared in spire.algebra.EuclideanRing 
    def gcd(a: spire.math.Interval[F],b: spire.math.Interval[F]): spire.math.Interval[F] = ??? 
    def mod(a: spire.math.Interval[F],b: spire.math.Interval[F]): spire.math.Interval[F] = ??? 
    def quot(a: spire.math.Interval[F],b: spire.math.Interval[F]): spire.math.Interval[F] = ??? 
    // Members declared in spire.algebra.MultiplicativeGroup 
    def div(x: spire.math.Interval[F],y: spire.math.Interval[F]): spire.math.Interval[F] = x/y 
    // Members declared in spire.algebra.MultiplicativeMonoid 
    def one: spire.math.Interval[F] = ??? 
    // Members declared in spire.algebra.MultiplicativeSemigroup 
    def times(x: spire.math.Interval[F],y: spire.math.Interval[F]): spire.math.Interval[F] = x * y
  }
  
  class SemiringStruct[A, F: Semiring] extends Semiring[A => F]{
    def plus(x: A => F,y: A => F): A => F = (a) => x(a) + y(a)
        // Members declared in spire.algebra.MultiplicativeMonoid   
    def zero: A => F = (a) => Semiring[F].zero      
    // Members declared in spire.algebra.MultiplicativeSemigroup   
    def times(x: A => F,y: A => F): A => F = (a) => x(a) * y(a)
  }
  
//  class FieldStruct[A, F : Field] extends 
  
  implicit def fieldStruct[A, F: Field] : Field[A => F] = new Field[A => F]{   
    // Members declared in spire.algebra.AdditiveGroup   
    def negate(x: A => F): A => F = (a) => -x(a)      
    // Members declared in spire.algebra.AdditiveMonoid   
    def zero: A => F = (a) => Field[F].zero   
    // Members declared in spire.algebra.AdditiveSemigroup   
    def plus(x: A => F,y: A => F): A => F = (a) => x(a) + y(a)      
    // Members declared in spire.algebra.EuclideanFing   
    def gcd(a: A => F,b: A => F): A => F = (p) => Field[F].gcd(a(p), b(p)) 
    def mod(a: A => F,b: A => F): A => F = (p) => Field[F].mod(a(p), b(p))   
    def quot(a: A => F,b: A => F): A => F = (p) => Field[F].quot(a(p), b(p))       
    // Members declared in spire.algebra.MultiplicativeGroup   
    def div(x: A => F,y: A => F): A => F = (a) => x(a) / y(a)      
    // Members declared in spire.algebra.MultiplicativeMonoid   
    def one: A => F = (a) => Field[F].one      
    // Members declared in spire.algebra.MultiplicativeSemigroup   
    def times(x: A => F,y: A => F): A => F = (a) => x(a) * y(a)     
  }
  
  implicit def OptFieldStruct[A, F: Field] = new Field[A => Option[F]]{
    // Members declared in spire.algebra.AdditiveGroup   
    def negate(x: A => Option[F]): A => Option[F] = (a) => x(a) map ((b) => -b)
    // Members declared in spire.algebra.AdditiveMonoid   
    def zero: A => Option[F] = (a) => Some(Field[F].zero)
    // Members declared in spire.algebra.AdditiveSemigroup   
    def plus(x: A => Option[F],y: A => Option[F]): A => Option[F] = 
      (a) => for (p <- x(a); q <- y(a)) yield p + q
    // Members declared in spire.algebra.EuclideanRing   
    def gcd(a: A => Option[F],b: A => Option[F]): A => Option[F] = 
      (z) => for (p <- a(z); q <- b(z)) yield Field[F].gcd(p, q)
    def mod(a: A => Option[F],b: A => Option[F]): A => Option[F] = 
      (z) => for (p <- a(z); q <- b(z)) yield Field[F].mod(p, q)
    def quot(a: A => Option[F],b: A => Option[F]): A => Option[F] =
      (z) => for (p <- a(z); q <- b(z)) yield Field[F].quot(p, q)
    // Members declared in spire.algebra.MultiplicativeGroup   
    def div(x: A => Option[F],y: A => Option[F]): A => Option[F] = 
      (a) => for (p <- x(a); q <- y(a); r <- Try(p/q).toOption) yield r
    // Members declared in spire.algebra.MultiplicativeMonoid   
    def one: A => Option[F] = (a) => Some(Field[F].one)      
    // Members declared in spire.algebra.MultiplicativeSemigroup   
    def times(x: A => Option[F],y: A => Option[F]): A => Option[F] = 
      (a) => for (p <- x(a); q <- y(a)) yield p * q
    
  }
}