package provingground

/**
 * @author gadgil
 */
trait Simplifier[A] {
  def rule(subsimplify: =>(A => A)): A => Option[A]
  
  lazy val simplify: A => A = (a) =>
      rule(simplify)(a) getOrElse (a)
      
  def apply(a: A) = simplify(a)
}

case class OrElse[A](first: Simplifier[A], second: Simplifier[A]) extends Simplifier[A]{
  def rule(subsimplify :  => (A => A)) = (a) => 
    first.rule(subsimplify)(a) orElse second.rule(subsimplify)(a)
}

case class Refine[A](base: Simplifier[A]) extends Simplifier[A]{
  def rule(subsimplify: =>(A => A)): A => Option[A] = (a) => base.rule(subsimplify)(a) map ((b) => rule(subsimplify)(b) getOrElse b)
}