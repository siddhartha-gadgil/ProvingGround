package algebra
import algebra.PointWise._

/**
 * @author gadgil
 */
trait ElementaryFunctions[A] extends Any{
  def sin : A
  def cos : A
  def log: A
  def exp: A
  
  def proj: Int => A
}