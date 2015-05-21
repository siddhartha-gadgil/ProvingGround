package compact_enumeration

/**
 * @author gadgil
 * Stub for HoTT
 */
object Stub {
    /**
   * local version of Types, Terms etc.
   * should not be imported if HoTT is to be used.
   * 
   */
  trait Typ
  
  /**
   * Constatnt types
   */
  trait ConstantTyp extends Typ
  
  /**
   * local version of Term
   */
  trait Term{
    val typ : Typ
  }
  
  /**
   * Constant Terms
   */
  trait ConstantTerm extends Term
  
  sealed class FormalFunction[A, B] extends (A => B){
    def apply(a: A): B = ???
  }
  
  case class Derivative[A](func: A => A) extends FormalFunction[A, A]
  
  case class PartialDerivative[A, I](func: Vector[A] => A, i: I) extends FormalFunction[A, A]
}