package compact_enumeration
import compact_enumeration.FieldOps._

/**
  * @author gadgil
  *
  * Elementary functions of type  A (e.g. A = Real => Real).
  * this is not quite correct for multi-variable functions.
  */
trait ElementaryFunctions[A] {
  val sin: A
  val cos: A
  val log: A
  val exp: A

  val sqrt: A

  val pi: A

  val proj: Int => A

  implicit val fieldOps: FieldOps[A]

}
