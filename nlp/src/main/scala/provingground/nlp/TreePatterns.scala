package provingground.translation
import provingground._

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

  val ifPattern: Pattern[Tree, IV] = Translator.Pattern[Tree, IV](IfTree.unapply)

  object Test {
    val ifTrans: Translator.Junction[Tree, Int, IV] = ifPattern.join((xl: (Int, Vector[Int])) =>
      xl._2.headOption map (_ + xl._1))
  }

  object VP
      extends Pattern.Partial[Tree, Vector]({
        case parent @ Node("VP",
                           Vector(VB(s1), Node("VP", (VB(s2)) +: tail))) =>
          val w = PennTrees.mkLeaf(s1 + " " + s2, parent)
          val n = PennTrees.mkTree(Vector(w), "VB", parent)
//          pprint.log(n +: tail)
          n +: tail
        case parent @ Node(
              "VP",
              Vector(MD(s0),
                     Node("VP", Vector(VB(s1), Node("VP", (VB(s2)) +: tail))))
            ) =>
          val w = PennTrees.mkLeaf(s0 + " " + s1 + " " + s2, parent)
          val n = PennTrees.mkTree(Vector(w), "VB", parent)
          pprint.log(n +: tail)
          n +: tail
        case Node("VP", xs) =>
          // pprint.log(xs)
          xs
      })

  object ExistSP
      extends Pattern.Partial[Tree, Id]({
        case parent @ Node(
              "S",
              Vector(
                Node("NP", Vector(Node("EX", _))),
                Node(
                  "VP",
                  (vex @ VB(w)) +: tail
                )
              )
            ) =>
          // if w.startsWith("exist") =>
          PennTrees.mkTree(tail, "NP", parent)
      })

  object VPIf
      extends Pattern.Partial[Tree, II]({
        case vp @ Node("VP", xs :+ IfClause(t)) =>
          (PennTrees.mkTree(xs, "VP", vp), t)
      })

  object NP extends Pattern.Partial[Tree, Vector]({ case Node("NP", xs) => xs })

  object NPVP
      extends Pattern.Partial[Tree, II]({
        case Node(h, Vector(x @ NP(_), y @ VP(_)))
            if h.startsWith("S") || h.startsWith("N") =>
          (x, y)
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
            if (x.value.startsWith("JJ") || (x.value.startsWith("ADJ"))) &&
              ys.forall(_.value.startsWith("PP")) =>
          (x, ys)
      })

  object VerbPP
      extends Pattern.Partial[Tree, IV]({
        case VP(x +: ys)
            if x.value.startsWith("V") &&
              ys.forall(_.value.startsWith("PP")) =>
          (x, ys)
      })

  object PP
      extends Pattern.Partial[Tree, II]({
        case Node("PP",
                  Vector(
                    jj @ IN(_),
                    np @ Node("NP", _)
                  )) =>
          (jj, np)
      })

  object QP
      extends Pattern.Partial[Tree, III]({
        case Node("NP",
                  Vector(
                    Node("QP",
                         Vector(
                           nn @ JJ(_),
                           pp @ IN(_),
                           fmla @ Node("CD", _)
                         )))) =>
          ((nn, pp), fmla)
        case Node("QP",
                  Vector(
                    nn @ JJ(_),
                    pp @ IN(_),
                    fmla @ Node("CD", _)
                  )) =>
          ((nn, pp), fmla)
      })

  object NPPP
      extends Pattern.Partial[Tree, II]({
        case Node("NP", Vector(x @ NP(_), y @ PP(_, _))) => (x, y)
      })

  object NPWH
      extends Pattern.Partial[Tree, II]({
        case Node(n,
                  Vector(
                    x @ NP(_),
                    Node("SBAR", Vector(Node(w, _), y))
                  )) if n.startsWith("N") && w.startsWith("WHA") =>
          (x, y)
      })

  object NPPPWH
      extends Pattern.Partial[Tree, III]({
        case Node(n,
                  Vector(
                    x @ NP(_),
                    pp @ PP(_, _),
                    Node("SBAR", Vector(Node(w, _), y))
                  )) if n.startsWith("N") && w.startsWith("WHA") =>
          ((x, pp), y)
      })

  object SimpleNPVP
      extends Pattern.Partial[Tree, II]({
        case Node("S", Vector(NP(Vector(x)), VP(Vector(y)))) => (x, y)
      }) {
    val pattern: Pattern[Tree, II] = Translator.Pattern[Tree, Functors.II](unapply)

    def translate[E: ExprLang]: Translator.Junction[Tree, E, II] = {
      pattern.join(ExprLang.appln[E])
    }
  }

  object Purge
      extends Pattern.Partial[Tree, Id]({
        case parent @ Node(tag, v) if v.exists(_.value == ",") =>
          PennTrees.mkTree(v filter (_.value != ","), tag, parent)
      })

  object IfNode
      extends Pattern.Partial[Tree, Id](
        {
          case Node("SBAR",
                    Vector(
                      Node("IN", Vector(Leaf("if"))),
                      tail
                    )) =>
            tail
        }
      )

  val ifSplit: Tree => Option[(Tree, Tree)] = {
    case parent @ Node(tag, init :+ IfNode(condition)) =>
      // pprint.log(parent)
      pprint.log(condition)
      pprint.log(ExistSP.unapply(condition))
      Some(condition -> PennTrees.mkTree(init, tag, parent))
    case parent @ Node(tag, init :+ last) =>
      // pprint.log(parent)
      // pprint.log(last)
      // pprint.log(FormalExpr.translator(last))
      // pprint.log(IfNode.unapply(last))
      for {
        (condition, stump) <- ifSplit(last)
      } yield condition -> PennTrees.mkTree(init :+ stump, tag, parent)
    case _ => None
  }

  object IfSplit extends Pattern[Tree, II](ifSplit)

  val npvpPattern: Pattern[Tree, II] = Translator.Pattern[Tree, Functors.II](NPVP.unapply)

  object Det
      extends Pattern.Partial[Tree, S]({
        case Node("DT", Vector(Leaf(det)))                     => det
        case Node("CD", Vector(Leaf(n))) if !n.startsWith("$") => "#" + n
      })

  object DPBase
      extends Pattern.Partial[Tree, SVO]({
        case Node("NP", Det(det) +: adjs :+ nn)
            if (adjs.forall(_.value.startsWith("JJ")) && (nn.value == "NN")) =>
          (det, (adjs, Some(nn)))
        case Node("NP", Det(det) +: adjs)
            if (adjs.forall(_.value.startsWith("JJ"))) =>
          (det, (adjs, None))
        case Node("NP", Vector(Node("QP", Vector(Det(det), nn)))) =>
          (det, (Vector(), Some(nn)))
      })

  object DPBaseZero
      extends Pattern.Partial[Tree, VO]({
        case Node("NP", adjs :+ nn)
            if (adjs.forall(_.value.startsWith("JJ")) && (nn.value == "NNS")) =>
          (adjs, Some(nn))
      })

  object DPQuant
      extends Pattern.Partial[Tree, SVI]({
        case Node("NP", Det(det) +: adjs :+ np)
            if (adjs.forall(_.value.startsWith("JJ")) && (np.value != "NN") &&
              (np.value.startsWith("N") || np.value == "CD")) =>
          (det, (adjs, np))
      })

  object DPBaseQuant
      extends Pattern.Partial[Tree, SVII]({
        case Node("NP", Det(det) +: adjs :+ np :+ npp)
            if (adjs.forall(_.value.startsWith("JJ")) && (np.value
              .startsWith("NN") || np.value == "CD") &&
              (npp.value.startsWith("N") || npp.value == "CD")) =>
          // pprint.log(npp)
          (det, (adjs, (np, npp)))
        case Node("NP",
                  Vector(
                    Node("NP", Det(det) +: adjs :+ np),
                    npp @ Node("NP", _)
                  ))
            if (adjs.forall(_.value.startsWith("JJ")) && (np.value
              .startsWith("NN") || np.value == "CD") &&
              (npp.value.startsWith("N") || npp.value == "CD")) =>
          (det, (adjs, (np, npp)))
      })

  object DPBaseQuantZero
      extends Pattern.Partial[Tree, VII]({
        case Node("NP", adjs :+ np :+ npp)
            if (adjs.forall(_.value.startsWith("JJ")) && (np.value.startsWith(
              "NNS")) &&
              (npp.value.startsWith("N") || npp.value == "CD")) =>
          (adjs, (np, npp))
        case Node("NP",
                  Vector(
                    Node("NP", adjs :+ np),
                    npp @ Node("NP", _)
                  ))
            if (adjs.forall(_.value.startsWith("JJ")) && (np.value.startsWith(
              "NNS")) &&
              (npp.value.startsWith("N") || npp.value == "CD")) =>
          (adjs, (np, npp))
      })

  object Which
      extends Pattern.Partial[Tree, Id]({
        case Node(
            "SBAR",
            Vector(
              Node("WHNP", Vector(Node("WDT", Vector(Leaf(_))))),
              Node("S", Vector(condition))
            )
            ) =>
          condition
      })

  object DPWhich
      extends Pattern.Partial[Tree, II](
        {
          case Node(
              "NP",
              Vector(
                dp @ DPBase(_, _),
                wh @ Which(_)
              )
              ) =>
            (dp, wh)
          case Node(
              "NP",
              Vector(
                dp @ DPBaseZero(_, _),
                wh @ Which(_)
              )
              ) =>
            (dp, wh)
          case Node(
              "NP",
              Vector(
                dp @ DPQuant(_, _),
                wh @ Which(_)
              )
              ) =>
            (dp, wh)
          case Node(
              "NP",
              Vector(
                dp @ DPBaseQuant(_, _),
                wh @ Which(_)
              )
              ) =>
            (dp, wh)
          case Node(
              "NP",
              Vector(
                dp @ DPBaseQuantZero(_, _),
                wh @ Which(_)
              )
              ) =>
            (dp, wh)

        }
      )

  object DPPPWhich
      extends Pattern.Partial[Tree, III](
        {
          case Node(
              "NP",
              Vector(
                dp @ DPBase(_, _),
                pp @ PP(_, _),
                wh @ Which(_)
              )
              ) =>
            ((dp, pp), wh)
          case Node(
              "NP",
              Vector(
                dp @ DPBaseZero(_, _),
                pp @ PP(_, _),
                wh @ Which(_)
              )
              ) =>
            ((dp, pp), wh)
          case Node(
              "NP",
              Vector(
                dp @ DPQuant(_, _),
                pp @ PP(_, _),
                wh @ Which(_)
              )
              ) =>
            ((dp, pp), wh)
          case Node(
              "NP",
              Vector(
                dp @ DPBaseQuant(_, _),
                pp @ PP(_, _),
                wh @ Which(_)
              )
              ) =>
            ((dp, pp), wh)
          case Node(
              "NP",
              Vector(
                dp @ DPBaseQuantZero(_, _),
                pp @ PP(_, _),
                wh @ Which(_)
              )
              ) =>
            ((dp, pp), wh)

        }
      )

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
        case Node("NP", Vector(Node(tag, Vector(Leaf(nn)))))
            if tag.startsWith("N") & tag != "NNP" =>
          nn
        case parent @ Node(
              "NP",
              Vector(
                Node(n1, Vector(Leaf(s1))),
                Node(n2, Vector(Leaf(s2)))
              )
            ) if (n1.startsWith("N") && n2.startsWith("N")) =>
          s1 + " " + s2
      })

  object NNP
      extends Pattern.Partial[Tree, S]({
        case Node("NP", Vector(Node("NNP", Vector(Leaf(nn))))) => nn
        case Node("NNP", Vector(Leaf(nn)))                     => nn
        case Node("NP", Vector(Node("CD", Vector(Leaf(nn)))))  => nn
        case Node("CD", Vector(Leaf(nn)))                      => nn
      })

  object DisjunctNP
      extends Pattern.Partial[Tree, Vector]({
        case Node(
            "NP",
            init :+ Node("CC", Vector(Leaf("or"))) :+ last
            ) =>
          init.filter(_.value != ",") :+ last
        case Node(
            "NP",
            Vector(
              Node("QP", init :+ Node("CC", Vector(Leaf("or"))) :+ last))) =>
          init.filter(_.value != ",") :+ last
      })

  object ConjunctNP
      extends Pattern.Partial[Tree, Vector]({
        case Node(
            "NP",
            init :+ Node("CC", Vector(Leaf("and"))) :+ last
            ) =>
          init.filter(_.value != ",") :+ last
        case Node(
            "NP",
            Vector(
              Node("QP", init :+ Node("CC", Vector(Leaf("and"))) :+ last))) =>
          init.filter(_.value != ",") :+ last
      })

  object ConjunctSP
      extends Pattern.Partial[Tree, Vector]({
        case Node(
            "S",
            init :+ Node("CC", Vector(Leaf("and"))) :+ last
            ) if init.head.value.startsWith("S") && last.value.startsWith("S") =>
          init.filter(_.value != ",") :+ last
      })

  object DisjunctSP
    extends Pattern.Partial[Tree, Vector]({
      case Node(
      "S",
      init :+ Node("CC", Vector(Leaf("or"))) :+ last
      ) if init.head.value.startsWith("S") && last.value.startsWith("S") =>
        init.filter(_.value != ",") :+ last
    })


  object Iff
      extends Pattern.Partial[Tree, II]({
        case Node(
            "NP",
            Vector(
              lhs,
              Node("IN", Vector(Leaf("iff"))),
              rhs
            )
            ) =>
          (lhs, rhs)
      })

  object VB
      extends Pattern.Partial[Tree, S]({
        case Node(tag, Vector(Leaf(vb))) if tag.startsWith("VB") => vb
      })

  object MD
      extends Pattern.Partial[Tree, S]({
        case Node("MD", Vector(Leaf(s))) => s
      })

  object JJ
      extends Pattern.Partial[Tree, S]({
        case Node(jj, Vector(Leaf(nn))) if jj.startsWith("JJ") => nn
      })

  object IN
      extends Pattern.Partial[Tree, S]({
        case Node("IN", Vector(Leaf(nn))) => nn
        case Node("TO", Vector(Leaf(nn))) => nn
      })

  object It
      extends Pattern.Partial[Tree, Un]({
        case Node("NP", Vector(Node(_, Vector(Leaf("it"))))) => ()
      })

  object They
      extends Pattern.Partial[Tree, Un]({
        case Node("NP", Vector(Node(_, Vector(Leaf("they"))))) => ()
      })

  object Exists
      extends Pattern.Partial[Tree, Un]({
        case Node("NP", Vector(Node("EX", _))) => ()
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
    Pattern[Tree, Vector]((t: Tree) =>
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
            case WordDash(u, v)  =>
              phraseFromVec("_" +: tail).unapply(v)
            case _ => None
          }
        case _ => None
    })

  def phrase(s: String): Pattern[Tree, Vector] =
    phraseFromVec(s.split(" ").toVector)

  def phraseTrans(s: String): Translator.Junction[Tree, MathExpr, Vector] =
    phrase(s) >>> { (v: Vector[MathExpr]) =>
      FormalExpr.Vec(v): MathExpr
    }

  def phrasesTrans(ss: String*): Translator[Tree, MathExpr] =
    ss.toVector.map(phraseTrans).fold(Translator.Empty[Tree, MathExpr]())(_ || _)
}

object TreeToMath {
  val npvp: Translator.Junction[Tree, MathExpr, II] =
    TreePatterns.NPVP.>>>[MathExpr]({
      case (np, vp) => MathExpr.NPVP(np, vp)
    })

  val verbObj: Translator.Junction[Tree, MathExpr, II] =
    TreePatterns.VerbObj.>>>[MathExpr]({
      case (vp, np) => MathExpr.VerbObj(vp, np)
    })

  val verbAdj: Translator.Junction[Tree, MathExpr, II] =
    TreePatterns.VerbAdj.>>>[MathExpr]({
      case (vp, adj) => MathExpr.VerbAdj(vp, adj)
    })

  val verbNotObj: Translator.Junction[Tree, MathExpr, II] =
    TreePatterns.VerbNotObj.>>>[MathExpr]({
      case (vp, np) => MathExpr.VerbObj(MathExpr.NegVP(vp), np)
    })

  val verbNotAdj: Translator.Junction[Tree, MathExpr, II] =
    TreePatterns.VerbNotAdj.>>>[MathExpr]({
      case (vp, adj) => MathExpr.VerbAdj(MathExpr.NegVP(vp), adj)
    })

  val verbIf: Translator.Junction[Tree, MathExpr, II] =
    TreePatterns.VPIf.>>>[MathExpr]({
      case (vp, ifc) => MathExpr.VPIf(vp, ifc)
    })

  val pp: Translator.Junction[Tree, MathExpr, II] =
    TreePatterns.PP.>>>[MathExpr]({
      case (p, np) => MathExpr.PP(negated = false, p, np)
    })

  val qp
    : Translator.Junction[Tree, MathExpr, III] = TreePatterns.QP >>> [MathExpr] ({
    case ((j, p), fm) => MathExpr.JJPP(j, Vector(MathExpr.PP(negated = false, p, fm)))
  })

  val nn: Translator.Junction[Tree, MathExpr, S] = TreePatterns.NN.>>>[MathExpr](MathExpr.NN(_))

  val fmla: Translator.Junction[Tree, MathExpr, S] = TreePatterns.NNP.>>>[MathExpr](MathExpr.Formula(_))

  val vb: Translator.Junction[Tree, MathExpr, S] = TreePatterns.VB.>>>[MathExpr](MathExpr.VB(_))

  val jj: Translator.Junction[Tree, MathExpr, S] = TreePatterns.JJ.>>>[MathExpr](MathExpr.JJ(_))

  val prep: Translator.Junction[Tree, MathExpr, S] = TreePatterns.IN.>>>[MathExpr](MathExpr.Prep(_))

  val it: Translator.Junction[Tree, MathExpr, Un] = TreePatterns.It.>>>[MathExpr]((_) => MathExpr.It(None))

  val they: Translator.Junction[Tree, MathExpr, Un] = TreePatterns.They.>>>[MathExpr]((_) => MathExpr.They(Vector()))

  val which: Translator.Junction[Tree, MathExpr, Id] = TreePatterns.Which.>>>[MathExpr](MathExpr.Which(_))

  val dpWhich: Translator.Junction[Tree, MathExpr, II] = TreePatterns.DPWhich.>>[MathExpr] {
    case (det: MathExpr.DP, wh) => Some(det.add(wh))
    case _                      => None
  }

  val dpPpWhich: Translator.Junction[Tree, MathExpr, III] = TreePatterns.DPPPWhich.>>[MathExpr] {
    case ((det: MathExpr.DP, p), wh) => Some(det.add(p).add(wh))
    case _                            => None
  }

  val dpBase: Translator.Junction[Tree, MathExpr, SVO] =
    TreePatterns.DPBase.>>>[MathExpr]({
      case (det, (adjs, nnOpt)) =>
        MathExpr.DP(MathExpr.Determiner(det), adjs, nnOpt)
    })

  val dpBaseZero: Translator.Junction[Tree, MathExpr, VO] =
    TreePatterns.DPBaseZero.>>>[MathExpr]({
      case (adjs, nnOpt) =>
        MathExpr.DP(MathExpr.Determiner.Zero, adjs, nnOpt)
    })

  val dpQuant: Translator.Junction[Tree, MathExpr, SVI] =
    TreePatterns.DPQuant.>>>[MathExpr]({
      case (det, (adjs, np)) =>
        MathExpr.DP(MathExpr.Determiner(det), adjs, None, Some(np))
    })

  val dpBaseQuant: Translator.Junction[Tree, MathExpr, SVII] =
    TreePatterns.DPBaseQuant.>>>[MathExpr]({
      case (det, (adjs, (np, npp))) =>
        MathExpr.DP(MathExpr.Determiner(det), adjs, Some(np), Some(npp))
    })

  val dpBaseQuantZero: Translator.Junction[Tree, MathExpr, VII] =
    TreePatterns.DPBaseQuantZero.>>>[MathExpr]({
      case (adjs, (np, npp)) =>
        MathExpr.DP(MathExpr.Determiner.Zero, adjs, Some(np), Some(npp))
    })

  val addPP: Translator.Junction[Tree, MathExpr, II] =
    TreePatterns.NPPP.>>[MathExpr] {
      case (dp: MathExpr.DP, p)        => Some(dp.add(p))
      case (fmla: MathExpr.Formula, p) => Some(fmla.dp.add(p))
      case _                            => None
    }

  val addST: Translator.Junction[Tree, MathExpr, II] =
    TreePatterns.NPWH.>>[MathExpr] {
      case (dpp: MathExpr.DP, wh)        => Some(dpp.st(wh))
      case (fmla: MathExpr.Formula, wh) => Some(fmla.dp.st(wh))
      case _                            => None
    }

  val addPPST: Translator.Junction[Tree, MathExpr, III] = TreePatterns.NPPPWH.>>[MathExpr] {
    case ((dp: MathExpr.DP, p), wh)        => Some(dp.add(p).st(wh))
    case ((fmla: MathExpr.Formula, p), wh) => Some(fmla.dp.add(p).st(wh))
    case _                                  => None
  }

  val jjpp: Translator.Junction[Tree, MathExpr, IV] =
    TreePatterns.JJPP.>>>[MathExpr]({
      case (adj, pps) => MathExpr.JJPP(adj, pps)
    })

  val verbpp: Translator.Junction[Tree, MathExpr, IV] =
    TreePatterns.VerbPP.>>>[MathExpr]({
      case (verb, pps) => MathExpr.VerbPP(verb, pps)
    })

  val exists: Translator.Junction[Tree, MathExpr, Un] =
    TreePatterns.Exists.>>>[MathExpr]({ (_) =>
      MathExpr.Exists
    })

  val existsSP: Translator.Junction[Tree, MathExpr, Id] =
    TreePatterns.ExistSP.>>>[MathExpr](identity)

  val or: Translator.Junction[Tree, MathExpr, Vector] = TreePatterns.DisjunctNP.>>>[MathExpr](MathExpr.DisjunctNP(_))

  val and: Translator.Junction[Tree, MathExpr, Vector] = TreePatterns.ConjunctNP.>>>[MathExpr](MathExpr.ConjunctNP(_))

  val andS: Translator.Junction[Tree, MathExpr, Vector] = TreePatterns.ConjunctSP.>>>[MathExpr](MathExpr.ConjunctSP(_))

  val orS: Translator.Junction[Tree, MathExpr, Vector] = TreePatterns.DisjunctSP.>>>[MathExpr](MathExpr.DisjunctSP(_))


  val iff: Translator.Junction[Tree, MathExpr, II] =
    TreePatterns.Iff.>>>[MathExpr]({ case (x, y) => MathExpr.Iff(x, y) })

  val dropRoot: Translator.Junction[Tree, MathExpr, Id] =
    TreePatterns.DropRoot.>>>[MathExpr] { (x) =>
      x
    }

  val dropNP: Translator.Junction[Tree, MathExpr, Vector] =
    TreePatterns.DropNP.>>[MathExpr] {
      case Vector(x: MathExpr.DP)      => Some(x)
      case Vector(x: MathExpr.Formula) => Some(x)
      case v                           =>
        // println(v)
        // println(v.size)
        None
    }

  val purge: Translator.Junction[Tree, MathExpr, Id] = TreePatterns.Purge.>>>[MathExpr]((x) => x)

  // val ifThen = TreePatterns.IfTree >>[MathExpr]{
  //   case (x, Vector(y)) =>
  //     Some(MathExpr.IfThen(x, y))
  //   case (x, Vector(np, vp)) =>
  //     Some(MathExpr.IfThen(x, MathExpr.NPVP(np, vp)))
  //   case _ =>
  //     None
  // }

  val ifThen: Translator.Junction[Tree, MathExpr, II] =
    TreePatterns.IfTreeSent.>>>[MathExpr]({
      case (x, y) => MathExpr.IfThen(x, y)
    })

  val notvp: Translator.Junction[Tree, MathExpr, Id] = TreePatterns.NotVP.>>>[MathExpr](MathExpr.NegVP(_))

  val iffP: Translator.Junction[Tree, MathExpr, Vector] = TreePatterns.phrase("_ iff _").>>>[MathExpr] {
    case Vector(x, y) => MathExpr.Iff(x, y)
  }

  val dropThen: Translator.Junction[Tree, MathExpr, Vector] = TreePatterns.phrase("then _").>>>[MathExpr] {
    case Vector(x) => x
  }

  val innerIf: Translator.Junction[Tree, MathExpr, II] = TreePatterns.IfSplit.>>>[MathExpr] {
    case (x, y) =>
      pprint.log(x)
      pprint.log(y)
      MathExpr.IfThen(x, y)
  }

  val mathExpr: Translator.OrElse[Tree, MathExpr] =
    fmla || ifThen || addPP || addST || addPPST || nn || vb || jj || pp || iffP ||
      prep || npvp || verbObj || verbAdj || verbNotObj || verbNotAdj || // verbIf ||
      existsSP || exists || jjpp || qp ||
      verbpp || notvp || it || they || which || dpWhich || dpPpWhich || dpBase || dpQuant || dpBaseQuant || dpBaseZero ||
      dpBaseQuantZero || and || or || andS || orS || dropRoot || dropNP || purge || iff || dropThen || innerIf

  val mathExprTree
    : Translator.OrElse[Tree, MathExpr] = mathExpr || FormalExpr.translator

  def mathExprFormal(ss: String*): Translator.OrElse[Tree, MathExpr] =
    mathExpr || TreePatterns.phrasesTrans(ss: _*) || FormalExpr.translator
}
