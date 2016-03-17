package provingground

import edu.stanford.nlp._
import simple._
import edu.stanford.nlp.trees.Tree

object PennTrees {
  object Leaf{
    def unapply(t: Tree) = {
      if (t.isLeaf) Some(t.value) else None 
    }
  }
  
  object Node{
    def unapply(t: Tree) = {
      if (t.isLeaf) None else Some((t.value, t.children().toList))
    }
  }
  
  sealed class TreeView
  
  case class LeafView(value: String) extends TreeView
  
  case class NodeView(value: String, children: List[TreeView]) extends TreeView
  
  def view(t: Tree) : TreeView = t match {
    case Leaf(s) => LeafView(s)
    case Node(s, children) => NodeView(s, children map view)
  }
}