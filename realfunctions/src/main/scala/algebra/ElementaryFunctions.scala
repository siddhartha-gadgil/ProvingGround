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
  
  implicit def fieldOps: FieldOps[A]
  
  import FieldOpsSyms._
  
  def tan = sin/cos
  
  def one : A = natField[A](1)
  
  def zero : A = natField[A](0)
  
  def sec = one / cos
  
  def cosec = one/ sin
 
  def x= proj(0)
  
  def y = proj(1)
  
  def z = proj(2)
}