package provingground.examples

import edu.stanford.nlp._
import trees.Tree
import simple._
import scala.collection.JavaConverters._

import provingground._, translation._
import TreePatterns._

object IfThen {

  import provingground._
  val sent = new Sentence(
    "if a prime number P divides MN, P divides one of M and N")

  val tree = sent.parse

  val st = tree.subTrees.asScala.toList

  val matches = st.map(IfTree.unapply).flatten

  val sent2 = new Sentence(
    "if a prime number P divides MN then P divides one of M and N")

  val tree2 = sent2.parse

  val st2 = tree2.subTrees.asScala.toList

  val matches2 = st2.map(IfTree.unapply).flatten
}
