package algebra
import algebra.PointWise._

/**
 * @author gadgil
 */
trait ElementaryFunctions[A]{
  val sin : A
  val cos : A
  val log: A
  val exp: A
  
  val pi: A
  
  val proj: Int => A
  
  implicit val fieldOps: FieldOps[A]
  
  import FieldOpsSyms._
  
  
}