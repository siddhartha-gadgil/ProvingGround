package provingground

import edu.stanford.nlp._
import simple._
import edu.stanford.nlp.trees.Tree

object PennTrees {
  object Leaf {
    def unapply(t: Tree) = {
      if (t.isLeaf) Some(t.value) else None
    }
  }

  object Node {
    def unapply(t: Tree) = {
      if (t.isLeaf) None else Some((t.value, t.children().toList))
    }
  }

  sealed trait TreeModel

  case class LeafModel(value: String) extends TreeModel {
    override def toString = s"""Leaf("$value")"""
  }

  case class NodeModel(value: String, children: List[TreeModel])
      extends TreeModel {
    override def toString = s"""Node("$value", ${children})"""
  }

  def model(t: Tree): TreeModel = t match {
    case Leaf(s) => LeafModel(s)
    case Node(s, children) => NodeModel(s, children map model)
  }
}
