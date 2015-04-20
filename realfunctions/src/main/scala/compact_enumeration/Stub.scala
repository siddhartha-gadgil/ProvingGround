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
}