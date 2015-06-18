package compact_enumeration
import compact_enumeration.FieldOps._

/**
 * @author gadgil
 * 
 * Elementary functions with values in A.
 */
trait ElementaryFunctions[A]{
  val sin : A
  val cos : A
  val log: A
  val exp: A
  
  val sqrt: A
  
  val pi: A
  
  val proj: Int => A
  
  implicit val fieldOps: FieldOps[A]
  
  import FieldOpsSyms._
  
  
}