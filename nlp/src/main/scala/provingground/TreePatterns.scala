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
        case Node("SBAR", Vector(Node("IN", Vector(Leaf(word("if")))), t)) => t
      })

  object Then
      extends Pattern.Partial[Tree, IV]({
          case Node("S",
                    x +: Node(
                      "ADVP",
                      Vector(Node("RB", Vector(Leaf(word("then")))))) +: ys) =>
            (x, ys)
        })

  object IfTree
      extends Pattern.Partial[Tree, IV]({
        case Node("S", IfClause(x) +: ys) => (x, ys)
        case IfClause(Then(x, ys))        => (x, ys)
      })

//  import Translator._

  val ifPattern = Translator.Pattern[Tree, IV](IfTree.unapply)

  object Test {
    val ifTrans = ifPattern.join((xl: (Int, Vector[Int])) =>
      xl._2.headOption map (_ + xl._1))
  }

  object VP extends Pattern.Partial[Tree, Vector]({ case Node("VP", xs) => xs })

  object NP extends Pattern.Partial[Tree, Vector]({ case Node("NP", xs) => xs })

  object NPVP
      extends Pattern.Partial[Tree, II]({
        case Node("S", Vector(x @ NP(_), y @ VP(_))) => (x, y)
      })

  object PP extends Pattern.Partial[Tree, II]({
          case Node("PP",
            Vector(
              jj @ Node("IN", Vector(Leaf(_))),
              np @ Node("NP", _)
            )
          ) => (jj, np)
      })


  object SimpleNPVP
      extends Pattern.Partial[Tree, II]({
        case Node("S", Vector(NP(Vector(x)), VP(Vector(y)))) => (x, y)
      }) {
    val pattern = Translator.Pattern[Tree, Functors.II](unapply)

    def translate[E: ExprLang] = {
      pattern.join(ExprLang.appln[E])
    }
  }

  val npvpPattern = Translator.Pattern[Tree, Functors.II](NPVP.unapply)

  object DPBase extends Pattern.Partial[Tree, SVO]({
    case Node("NP", Node("DT", Vector(Leaf(det))) +: adjs :+ nn)
      if (adjs.forall(_.value == "JJ") && (nn.value == "NN"))
        => (det, (adjs, Some(nn)))
    case Node("NP", Node("DT", Vector(Leaf(det))) +: adjs)
        if (adjs.forall(_.value == "JJ"))
            => (det, (adjs, None))

  })

  object NN extends Pattern.Partial[Tree, S]({
    case Node(tag, Vector(Leaf(nn))) if tag.startsWith("NN") => nn
  })


  object VB extends Pattern.Partial[Tree, S]({
    case Node(tag, Vector(Leaf(vb))) if tag.startsWith("VB") => vb
  })

  object JJ extends Pattern.Partial[Tree, S]({
    case Node("JJ", Vector(Leaf(nn))) => nn
  })

  object IN extends Pattern.Partial[Tree, S]({
    case Node("IN", Vector(Leaf(nn))) => nn
  })
}

object TreeToMathExpr{
  val npvp = TreePatterns.NPVP >>>[MathExpr]
    {case (np, vp) => MathExpr.NPVP(np, vp)}

  val pp = TreePatterns.PP >>> [MathExpr]{
    case (pp, np) => MathExpr.PP(false, pp, np)
  }

  val nn = TreePatterns.NN >>>[MathExpr](MathExpr.NN(_))

  val vb = TreePatterns.VB >>>[MathExpr](MathExpr.VB(_))

  val jj = TreePatterns.JJ >>>[MathExpr](MathExpr.JJ(_))

  val prep = TreePatterns.IN >>>[MathExpr](MathExpr.Prep(_))

  val dpBase =
    TreePatterns.DPBase >>>[MathExpr] {
      case (det, (adjs, nnOpt)) =>
      MathExpr.DP(MathExpr.Determiner(det), adjs, nnOpt)}

  val trans =
    nn ||
    vb ||
    jj ||
    pp ||
    prep ||
    npvp ||
    dpBase ||
    FormalExpr.translator
}
