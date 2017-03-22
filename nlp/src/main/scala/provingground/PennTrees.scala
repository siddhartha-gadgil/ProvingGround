package provingground

import edu.stanford.nlp._
import simple._
import edu.stanford.nlp.trees.Tree
import scala.collection.JavaConverters._

object PennTrees {
  object Leaf {
    def unapply(t: Tree) = {
      if (t.isLeaf) Some(t.value) else None
    }
  }

  object Node {
    def unapply(t: Tree) = {
      if (t.isLeaf) None else Some((t.value, t.children().toVector))
    }
  }

  object Twig {
    def unapply(t: Tree): Option[String] = t match {
      case Leaf(w)            => Some(w.toLowerCase)
      case Node(_, Vector(t)) => unapply(t)
      case _                  => None
    }
  }

  object LeftBinTree {
    def unapply(t: Tree): Option[(Tree, Tree)] = t match {
      case Node(_, Vector(x, y)) => Some((x, y))
      case parent @ Node(tag, x +: ys) if ys.size > 0 =>
        Some((x, mkTree(ys, tag, parent)))
      case _ => None
    }
  }

  object RightBinTree {
    def unapply(t: Tree): Option[(Tree, Tree)] = t match {
      case Node(_, Vector(x, y)) => Some((x, y))
      case parent @ Node(tag, ys :+ x) if ys.size > 0 =>
        Some((x, mkTree(ys, tag, parent)))
      case _ => None
    }
  }

  object WordDash {
    def unapply(t: Tree): Option[(String, Tree)] = t match {
      case LeftBinTree(Twig(w), y) => Some((w, y))
      case _                       => None
    }
  }

  object WordDashDash {
    def unapply(t: Tree): Option[(String, Tree, Tree)] = t match {
      case LeftBinTree(WordDash(w, y), z) => Some((w, y, z))
      case _                              => None
    }
  }

  object DashWord {
    def unapply(t: Tree): Option[(Tree, String)] = t match {
      case RightBinTree(y, Twig(w)) => Some((y, w))
      case _                        => None
    }
  }

  object DashDashWord {
    def unapply(t: Tree): Option[(Tree, Tree, String)] = t match {
      case RightBinTree(x, DashWord(y, w)) => Some((x, y, w))
      case _                               => None
    }
  }

  object DashWordDash {
    def unapply(t: Tree): Option[(Tree, String, Tree)] = t match {
      case LeftBinTree(DashWord(y, w), z)  => Some((y, w, z))
      case RightBinTree(y, WordDash(w, z)) => Some((y, w, z))
      case _                               => None
    }
  }

  def model(t: Tree): TreeModel = t match {
    case Leaf(s)           => TreeModel.Leaf(s)
    case Node(s, children) => TreeModel.Node(s, children map model)
  }

  implicit class ShowModel(sent: Sentence) {
    def show = model(sent.parse)
  }

  implicit class ShowString(st: String) {
    def show = model(parsed)

    def sentence = new Sentence(st)

    def parsed = StanfordParser.parse(st)
  }

  def mkTree(children: Vector[Tree], tag: String, parent: Tree) =
    parent
      .treeFactory()
      .newTreeNode(tag, children.asJava: java.util.List[Tree])

  def sentence(children: Vector[Tree]) =
    children.head
      .treeFactory()
      .newTreeNode("S", children.asJava: java.util.List[Tree])
}

sealed trait TreeModel

object TreeModel {
  case class Leaf(value: String) extends TreeModel {
    override def toString = s"""Leaf("$value")"""
  }

  case class Node(value: String, children: Vector[TreeModel])
      extends TreeModel {
    // override def toString = s"""Node("$value", ${children})"""
  }
}
