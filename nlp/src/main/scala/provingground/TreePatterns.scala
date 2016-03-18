package provingground

import edu.stanford.nlp.trees.Tree

import PennTrees._

object TreePatterns {
  object word{
    def unapply(s: String) : Option[String] = Some(s.toLowerCase)
  }
  
  object IfClause{
    def unapply(tree: Tree) = tree match {
  case Node("SBAR", List(Node("IN", List(Leaf(word("if")))), t)) => Some(t)
  case _ => None
  }
  }
  
  object Then{
    def unapply(tree: Tree) = tree match {
      case Node("S", x :: Node("ADVP",List(Node("RB",List(Leaf(word("then")))))) :: ys) => Some((x, ys))
      case _ => None
    }
  }
  
  object IfTree{
    def unapply(tree: Tree) = tree match {
      case Node("S", IfClause(x) :: ys) => Some((x, ys))
      case IfClause(Then(x, ys)) => Some((x, ys))
      case _ => None
    }
  }
  
  object NPVP{
    def unapply(tree: Tree) = tree match {
      case Node("S", List(Node("NP", xs), Node("VP", ys))) => Some((xs, ys))
      case _ => None
    }
  }
}