package provingground

import HoTT._

sealed trait ShapeTree {
  def subTrees: Set[ShapeTree] // excuding the single leaf

  def subShapeTrees = subTrees + ShapeTree.Leaf
}

object ShapeTree {
  case object Leaf extends ShapeTree {
    def subTrees = Set()
  }

  case class ApplnNode(func: ShapeTree, arg: ShapeTree) extends ShapeTree {
    def subTrees =
      for (funcTree <- func.subShapeTrees; argTree <- arg.subShapeTrees) yield
        ApplnNode(funcTree, argTree)
  }

  case class ArrowNode(dom: ShapeTree, codom: ShapeTree) extends ShapeTree {
    def subTrees =
      for (domTree <- dom.subShapeTrees; codomTree <- codom.subShapeTrees) yield
        ArrowNode(domTree, codomTree)
  }

  case class PiNode(fibre: ShapeTree) extends ShapeTree {
    def subTrees = fibre.subShapeTrees map (PiNode(_))
  }

  case class SigmaNode(fibre: ShapeTree) extends ShapeTree {
    def subTrees = fibre.subShapeTrees map (SigmaNode(_))
  }

  case class LambdaNode(variable: ShapeTree, dom: ShapeTree, value: ShapeTree)
      extends ShapeTree {
    def subTrees =
      for (domTree <- dom.subShapeTrees;
           varTree <- variable.subShapeTrees;
           valTree <- value.subShapeTrees) yield
        EqualityNode(varTree, domTree, valTree)
  }

  case class EqualityNode(dom: ShapeTree, lhs: ShapeTree, rhs: ShapeTree)
      extends ShapeTree {
    def subTrees =
      for (domTree <- dom.subShapeTrees;
           lhsTree <- lhs.subShapeTrees;
           rhsTree <- rhs.subShapeTrees) yield
        EqualityNode(domTree, lhsTree, rhsTree)
  }

  case class PairNode(first: ShapeTree, second: ShapeTree) extends ShapeTree {
    def subTrees =
      for (firstTree <- first.subShapeTrees; secondTree <- second.subShapeTrees) yield
        PairNode(firstTree, secondTree)
  }

  case class PlusNode(first: ShapeTree, second: ShapeTree) extends ShapeTree {
    def subTrees =
      for (firstTree <- first.subShapeTrees; secondTree <- second.subShapeTrees) yield
        PlusNode(firstTree, secondTree)
  }
}

class ShapeTreeFormat(isAtom: Term => Boolean) extends TermRec[ShapeTree] {
  import ShapeTree._

  val specialTerms: PartialFunction[Term, ShapeTree] = {
    case atom: Term if isAtom(atom) => Leaf
  }

  def fromString(str: String)(implicit typ: Typ[Term]): ShapeTree = Leaf

  def appln(func: ShapeTree, arg: ShapeTree): ShapeTree = ApplnNode(func, arg)

  def arrow(dom: ShapeTree, codom: ShapeTree): ShapeTree =
    ArrowNode(dom, codom)

  def lambda(
      variable: ShapeTree, typ: ShapeTree, value: ShapeTree): ShapeTree =
    LambdaNode(variable, typ, value)

  def pi(fibre: ShapeTree): ShapeTree = PiNode(fibre)

  def sigma(fibre: ShapeTree): ShapeTree = SigmaNode(fibre)

  def plus(first: ShapeTree, second: ShapeTree): ShapeTree =
    PlusNode(first, second)

  def equality(dom: ShapeTree, lhs: ShapeTree, rhs: ShapeTree): ShapeTree =
    EqualityNode(dom, lhs, rhs)

  def pair(first: ShapeTree, second: ShapeTree): ShapeTree =
    PairNode(first, second)

  def symbobj(term: SymbObj[Term]): ShapeTree = Leaf

  def symbtyp(term: SymbTyp): ShapeTree = Leaf

  def symbolic(name: AnySym, typ: Typ[Term]): ShapeTree = Leaf

  def univ(n: Int): ShapeTree = Leaf
}

object TermShapeTree extends ShapeTreeFormat(isVar)
