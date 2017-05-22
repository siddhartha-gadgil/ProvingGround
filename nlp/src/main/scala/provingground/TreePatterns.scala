package provingground

import edu.stanford.nlp.trees.Tree

import PennTrees._

import translation._

import Functors._

import Translator.Pattern

import cats._

import cats.implicits._

// import shapeless.{:: => :::, Id => IdS, _}

// import HList._

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
          (x, ys filter ((y) => Set(",", "RB") contains (y.value)))
      })

  object ThenSent
      extends Pattern.Partial[Tree, II]({
        case Then(x, Vector(y)) => (x, y)
        case Then(x, ys) if ys.size > 1 =>
          (x, PennTrees.sentence(ys))
      })

  object IfTree
      extends Pattern.Partial[Tree, IV]({
        case Node("S", IfClause(x) +: ys) => (x, ys filter (_.value != ","))
        case IfClause(Then(x, ys))        => (x, ys)
      })

  object IfTreeSent
      extends Pattern.Partial[Tree, II]({
        case Node("S", IfClause(x) +: ys) =>
          (x, PennTrees.sentence(ys filter (_.value != ",")))
        case IfClause(ThenSent(x, y)) => (x, y)
      })

//  import Translator._

  val ifPattern = Translator.Pattern[Tree, IV](IfTree.unapply)

  object Test {
    val ifTrans = ifPattern.join((xl: (Int, Vector[Int])) =>
      xl._2.headOption map (_ + xl._1))
  }

  object VP
      extends Pattern.Partial[Tree, Vector]({ case Node("VP", xs) => xs })

  object NP
      extends Pattern.Partial[Tree, Vector]({ case Node("NP", xs) => xs })

  object NPVP
      extends Pattern.Partial[Tree, II]({
        case Node("S", Vector(x @ NP(_), y @ VP(_))) => (x, y)
      })

  object VerbObj
      extends Pattern.Partial[Tree, II]({
        case Node("VP", Vector(x @ Node(vb, _), y @ Node(nn, _)))
            if (vb.startsWith("V") && nn.startsWith("N")) =>
          (x, y)
      })

  object VerbNotObj
      extends Pattern.Partial[Tree, II]({
        case Node("VP",
                  Vector(x @ Node(vb, _),
                         Node(_, Vector(Leaf("not"))),
                         y @ Node(nn, _)))
            if (vb.startsWith("V") && nn.startsWith("N")) =>
          (x, y)
      })

  object VerbAdj
      extends Pattern.Partial[Tree, II]({
        case Node("VP", Vector(x @ Node(vb, _), y @ Node("ADJP", _)))
            if vb.startsWith("V") =>
          (x, y)
      })

  object VerbNotAdj
      extends Pattern.Partial[Tree, II]({
        case Node("VP",
                  Vector(x @ Node(vb, _),
                         Node(_, Vector(Leaf("not"))),
                         y @ Node("ADJP", _))) if vb.startsWith("V") =>
          (x, y)
      })

  object JJPP
      extends Pattern.Partial[Tree, IV]({
        case Node("ADJP", x +: ys)
            if x.value.startsWith("JJ") &&
              ys.forall(_.value.startsWith("PP")) =>
          (x, ys)
      })

  object VerbPP
      extends Pattern.Partial[Tree, IV]({
        case Node("VP", x +: ys)
            if x.value.startsWith("V") &&
              ys.forall(_.value.startsWith("PP")) =>
          (x, ys)
      })

  object PP
      extends Pattern.Partial[Tree, II]({
        case Node("PP",
                  Vector(
                    jj @ Node("IN", Vector(Leaf(_))),
                    np @ Node("NP", _)
                  )) =>
          (jj, np)
      })

  object NPPP
      extends Pattern.Partial[Tree, II]({
        case Node("NP", Vector(x @ NP(_), y @ PP(_, _))) => (x, y)
      })

  object NPWH
      extends Pattern.Partial[Tree, II]({
        case Node("NP",
                  Vector(
                    x @ NP(_),
                    Node("SBAR", Vector(Node("WHADVP", _), y))
                  )) =>
          (x, y)
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

  object Purge
      extends Pattern.Partial[Tree, Id]({
        case parent @ Node(tag, v) if v.exists(_.value == ",") =>
          PennTrees.mkTree(v filter (_.value != ","), tag, parent)
      })

  val npvpPattern = Translator.Pattern[Tree, Functors.II](NPVP.unapply)

  object DPBase
      extends Pattern.Partial[Tree, SVO]({
        case Node("NP", Node("DT", Vector(Leaf(det))) +: adjs :+ nn)
            if (adjs.forall(_.value == "JJ") && (nn.value == "NN")) =>
          (det, (adjs, Some(nn)))
        case Node("NP", Node("DT", Vector(Leaf(det))) +: adjs)
            if (adjs.forall(_.value == "JJ")) =>
          (det, (adjs, None))
      })

  object DPBaseZero
      extends Pattern.Partial[Tree, VO]({
        case Node("NP", adjs :+ nn)
            if (adjs.forall(_.value == "JJ") && (nn.value == "NNS")) =>
          (adjs, Some(nn))
      })

  object DPQuant
      extends Pattern.Partial[Tree, SVI]({
        case Node("NP", Node("DT", Vector(Leaf(det))) +: adjs :+ np)
            if (adjs.forall(_.value == "JJ") && (np.value != "NN") &&
              np.value.startsWith("N")) =>
          (det, (adjs, np))
      })

  object DPBaseQuant
      extends Pattern.Partial[Tree, SVII]({
        case Node("NP", Node("DT", Vector(Leaf(det))) +: adjs :+ np :+ npp)
            if (adjs.forall(_.value == "JJ") && (np.value.startsWith("NN")) &&
              npp.value.startsWith("N")) =>
          (det, (adjs, (np, npp)))
        case Node("NP",
                  Vector(
                    Node("NP", Node("DT", Vector(Leaf(det))) +: adjs :+ np),
                    Node("NP", Vector(npp))
                  ))
            if (adjs.forall(_.value == "JJ") && (np.value.startsWith("NN")) &&
              npp.value.startsWith("N")) =>
          (det, (adjs, (np, npp)))
      })

  object DPBaseQuantZero
      extends Pattern.Partial[Tree, VII]({
        case Node("NP", adjs :+ np :+ npp)
            if (adjs.forall(_.value == "JJ") && (np.value.startsWith("NNS")) &&
              npp.value.startsWith("N")) =>
          (adjs, (np, npp))
        case Node("NP",
                  Vector(
                    Node("NP", adjs :+ np),
                    Node("NP", Vector(npp))
                  ))
            if (adjs.forall(_.value == "JJ") && (np.value.startsWith("NNS")) &&
              npp.value.startsWith("N")) =>
          (adjs, (np, npp))
      })

  val isModalDo: Tree => Boolean = {
    case Node("MD", _)            => true
    case Node(_, Vector(Leaf(v))) => Set("do", "does") contains (v)
    case _                        => false
  }

  object NotVP
      extends Pattern.Partial[Tree, Id]({
        case Node("VP",
                  head +: Node("RB", Vector(Leaf("not"))) +: Vector(
                    vp @ Node("VP", _))) if isModalDo(head) =>
          vp
      })

  object NN
      extends Pattern.Partial[Tree, S]({
        case Node(tag, Vector(Leaf(nn)))
            if tag.startsWith("N") & tag != "NNP" =>
          nn
      })

  object NNP
      extends Pattern.Partial[Tree, S]({
        case Node("NP", Vector(Node("NNP", Vector(Leaf(nn))))) => nn
        case Node("NNP", Vector(Leaf(nn)))                     => nn
      })

  object DisjunctNP
      extends Pattern.Partial[Tree, Vector]({
        case Node(
            "NP",
            init :+ Node("CC", Vector(Leaf("or"))) :+ last
            ) =>
          init.filter(_.value != ",") :+ last
      })

  object ConjunctNP
      extends Pattern.Partial[Tree, Vector]({
        case Node(
            "NP",
            init :+ Node("CC", Vector(Leaf("and"))) :+ last
            ) =>
          init.filter(_.value != ",") :+ last
      })

  object VB
      extends Pattern.Partial[Tree, S]({
        case Node(tag, Vector(Leaf(vb))) if tag.startsWith("VB") => vb
      })

  object JJ
      extends Pattern.Partial[Tree, S]({
        case Node("JJ", Vector(Leaf(nn))) => nn
      })

  object IN
      extends Pattern.Partial[Tree, S]({
        case Node("IN", Vector(Leaf(nn))) => nn
      })

  object DropRoot
      extends Pattern.Partial[Tree, Id]({
        case Node("ROOT", Vector(x)) => x
      })

  object DropNP
      extends Pattern.Partial[Tree, Vector]({
        case Node("NP", x) => x
      })

  def phraseFromVec(s: Vector[String]): Pattern[Tree, Vector] =
    Pattern[Tree, Vector](
      (t: Tree) =>
        s match {
          case Vector("_") => Some(Vector(t))
          case Vector(w) =>
            t match {
              case Twig(u) if u == w => Some(Vector())
              case _                 => None
            }
          case Vector(w, "_") =>
            t match {
              case WordDash(u, v) if u == w => Some(Vector(v))
              case _                        => None
            }
          case Vector("_", w) =>
            t match {
              case DashWord(a, b) if b == w => Some(Vector(a))
            }
          case "_" +: w +: tail =>
            t match {
              case DashWordDash(a, b, c) if b == w =>
                phraseFromVec(tail).unapply(c).map(a +: _)
              case _ => None
            }
          case w +: "_" +: tail =>
            t match {
              case WordDashDash(a, b, c) if a == w =>
                phraseFromVec(tail).unapply(c).map(b +: _)
              case WordDash(u, v) if u == v =>
                phraseFromVec("_" +: tail).unapply(v)
              case _ => None
            }
          case _ => None
      }
    )

  def phrase(s: String) = phraseFromVec(s.split(" ").toVector)
}

object TreeToMath {
  val npvp =
    TreePatterns.NPVP.>>>[MathExpr]({
      case (np, vp) => MathExpr.NPVP(np, vp)
    })

  val verbObj =
    TreePatterns.VerbObj.>>>[MathExpr]({
      case (vp, np) => MathExpr.VerbObj(vp, np)
    })

  val verbAdj =
    TreePatterns.VerbAdj.>>>[MathExpr]({
      case (vp, adj) => MathExpr.VerbAdj(vp, adj)
    })

  val verbNotObj =
    TreePatterns.VerbNotObj.>>>[MathExpr]({
      case (vp, np) => MathExpr.VerbObj(MathExpr.NegVP(vp), np)
    })

  val verbNotAdj =
    TreePatterns.VerbNotAdj.>>>[MathExpr]({
      case (vp, adj) => MathExpr.VerbAdj(MathExpr.NegVP(vp), adj)
    })

  val pp =
    TreePatterns.PP.>>>[MathExpr]({
      case (pp, np) => MathExpr.PP(false, pp, np)
    })

  val nn = TreePatterns.NN.>>>[MathExpr](MathExpr.NN(_))

  val fmla = TreePatterns.NNP.>>>[MathExpr](MathExpr.Formula(_))

  val vb = TreePatterns.VB.>>>[MathExpr](MathExpr.VB(_))

  val jj = TreePatterns.JJ.>>>[MathExpr](MathExpr.JJ(_))

  val prep = TreePatterns.IN.>>>[MathExpr](MathExpr.Prep(_))

  val dpBase =
    TreePatterns.DPBase.>>>[MathExpr]({
      case (det, (adjs, nnOpt)) =>
        MathExpr.DP(MathExpr.Determiner(det), adjs, nnOpt)
    })

  val dpBaseZero =
    TreePatterns.DPBaseZero.>>>[MathExpr]({
      case (adjs, nnOpt) =>
        MathExpr.DP(MathExpr.Determiner.Zero, adjs, nnOpt)
    })

  val dpQuant =
    TreePatterns.DPQuant.>>>[MathExpr]({
      case (det, (adjs, np)) =>
        MathExpr.DP(MathExpr.Determiner(det), adjs, None, Some(np))
    })

  val dpBaseQuant =
    TreePatterns.DPBaseQuant.>>>[MathExpr]({
      case (det, (adjs, (np, npp))) =>
        MathExpr.DP(MathExpr.Determiner(det), adjs, Some(np), Some(npp))
    })

  val dpBaseQuantZero =
    TreePatterns.DPBaseQuantZero.>>>[MathExpr]({
      case (adjs, (np, npp)) =>
        MathExpr.DP(MathExpr.Determiner.Zero, adjs, Some(np), Some(npp))
    })

  val addPP =
    TreePatterns.NPPP.>>[MathExpr] {
      case (dp: MathExpr.DP, pp)        => Some(dp.add(pp))
      case (fmla: MathExpr.Formula, pp) => Some(fmla.dp.add(pp))
      case _                            => None
    }

  val addST =
    TreePatterns.NPWH.>>[MathExpr] {
      case (dp: MathExpr.DP, wh)        => Some(dp.st(wh))
      case (fmla: MathExpr.Formula, wh) => Some(fmla.dp.st(wh))
      case _                            => None
    }

  val jjpp =
    TreePatterns.JJPP.>>>[MathExpr]({
      case (adj, pps) => MathExpr.JJPP(adj, pps)
    })

  val verbpp =
    TreePatterns.VerbPP.>>>[MathExpr]({
      case (verb, pps) => MathExpr.VerbPP(verb, pps)
    })

  val or = TreePatterns.DisjunctNP.>>>[MathExpr](MathExpr.DisjunctNP(_))

  val and = TreePatterns.ConjunctNP.>>>[MathExpr](MathExpr.ConjunctNP(_))

  val dropRoot =
    TreePatterns.DropRoot.>>>[MathExpr] { (x) =>
      x
    }

  val dropNP =
    TreePatterns.DropNP.>>[MathExpr] {
      case Vector(x: MathExpr.DP)      => Some(x)
      case Vector(x: MathExpr.Formula) => Some(x)
      case v =>
        println(v)
        println(v.size)
        None
    }

  val purge = TreePatterns.Purge.>>>[MathExpr]((x) => x)

  // val ifThen = TreePatterns.IfTree >>[MathExpr]{
  //   case (x, Vector(y)) =>
  //     Some(MathExpr.IfThen(x, y))
  //   case (x, Vector(np, vp)) =>
  //     Some(MathExpr.IfThen(x, MathExpr.NPVP(np, vp)))
  //   case _ =>
  //     None
  // }

  val ifThen =
    TreePatterns.IfTreeSent.>>>[MathExpr]({
      case (x, y) => MathExpr.IfThen(x, y)
    })

  val notvp = TreePatterns.NotVP.>>>[MathExpr](MathExpr.NegVP(_))

  val mathExpr =
    fmla || ifThen || and || or || addPP || addST || nn || vb || jj || pp ||
      prep || npvp || verbObj || verbAdj || verbNotObj || verbNotAdj || jjpp ||
      verbpp || notvp || dpBase || dpQuant || dpBaseQuant || dpBaseZero ||
      dpBaseQuantZero || dropRoot || dropNP || purge

  val mathExprTree = mathExpr || FormalExpr.translator
}
