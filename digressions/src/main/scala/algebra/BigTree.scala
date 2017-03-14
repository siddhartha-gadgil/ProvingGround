package algebra

/**
  * @author gadgil
  */
class BigTree[A]

trait BigNode[A] extends BigTree[A] {
  val left: BigTree[A]
  val right: BigTree[A]
}

class Tree[A] extends BigTree[A]

case class Leaf[A](label: A) extends Tree[A]

case class Node[A](left: Tree[A], right: Tree[A])
    extends Tree[A] with BigNode[A]

object BigTree {

  class LazyNode[A](_left: => BigTree[A], _right: => BigTree[A])
      extends BigNode[A] {
    lazy val left = _left
    lazy val right = _right
  }

  lazy val bigTree: BigNode[Int] = new LazyNode(bigTree, bigTree)
}
