package compact_enumeration

/**
  * @author gadgil
  */
/**
  * A composition on the type A.
  */
trait Circ[A] extends Any {
  def circ(x: A, y: A): A
}

object Circ {

  /**
    * having apply, i.e., f(g), and the method andThen correspond to using circ method.
    */
  implicit class applyCirc[A : Circ](x: A) {
    def apply(y: A) = implicitly[Circ[A]].circ(x, y)

    def circ(y: A) = apply(y)

    def andThen(y: A) = implicitly[Circ[A]].circ(y, x)
  }
}
