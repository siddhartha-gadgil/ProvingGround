package provingground

import edu.stanford.nlp.trees.Tree

import PennTrees._

import Functors._

import Translator.Pattern

import cats._

import cats.implicits._

import shapeless.{:: => :::, Id => IdS, _}

import HList._

object TreePatterns {
  object word {
    def unapply(s: String): Option[String] = Some(s.toLowerCase)
  }

  object IfClause
      extends Pattern.Partial[Tree, Id]({
        case Node("SBAR", List(Node("IN", List(Leaf(word("if")))), t)) => t
      })

  object Then
      extends Pattern.Partial[Tree, IL]({
          case Node("S",
                    x :: Node(
                      "ADVP",
                      List(Node("RB", List(Leaf(word("then")))))) :: ys) =>
            (x, ys)
        })

  object IfTree
      extends Pattern.Partial[Tree, IL]({
        case Node("S", IfClause(x) :: ys) => (x, ys)
        case IfClause(Then(x, ys))        => (x, ys)
      })

//  import Translator._

  val ifPattern = Translator.Pattern[Tree, IL](IfTree.unapply)

  object Test {
    val ifTrans = ifPattern.join((xl: (Int, List[Int])) =>
      xl._2.headOption map (_ + xl._1))
  }

  object VP extends Pattern.Partial[Tree, List]({ case Node("VP", xs) => xs })

  object NP extends Pattern.Partial[Tree, List]({ case Node("NP", xs) => xs })

  object NPVP
      extends Pattern.Partial[Tree, LL]({
        case Node("S", List(NP(xs), VP(ys))) => (xs, ys)
      })

  object SimpleNPVP
      extends Pattern.Partial[Tree, II]({
        case Node("S", List(NP(List(x)), VP(List(y)))) => (x, y)
      }) {
    val pattern = Translator.Pattern[Tree, Functors.II](unapply)

    def translate[E: ExprLang] = {
      pattern.join(ExprLang.appln[E])
    }
  }

  val npvpPattern = Translator.Pattern[Tree, Functors.LL](NPVP.unapply)
}
