package provingground.fol

import Formula.negate

trait Literal {
  val p: Formula
  def replaceFormula(p: Formula): Literal
  def subs(m: PartialFunction[Var, Term]): Literal

  val isPositive: Boolean

  val negate: Literal
}

case class PosLit(p: Formula) extends Literal {
  def replaceFormula(p: Formula): Literal = PosLit(p)
  def subs(m: PartialFunction[Var, Term]): Literal =
    PosLit(p.subs(m).asInstanceOf[Formula])

  val isPositive: Boolean = true

  lazy val negate = NegLit(p)
}

case class NegLit(p: Formula) extends Literal {
  def replaceFormula(p: Formula): Literal = NegLit(p)
  def subs(m: PartialFunction[Var, Term]): Literal =
    NegLit(p.subs(m).asInstanceOf[Formula])

  val isPositive: Boolean = false

  lazy val negate = PosLit(p)
}

class SkolemFunction(x: Var, d: Int) extends Func(d)

case class Clause(ls: Set[Literal]) {
  val isContradiction = ls.isEmpty
  val isUnit          = ls.size == 1
  lazy val isTautology =
    ls.exists(lit => ls.contains(lit.negate))
  def |(that: Clause) = Clause(this.ls ++ that.ls)
  def subs(m: PartialFunction[Var, Term]) =
    Clause(ls map ((l: Literal) => l.subs(m)))

  def -(lit: Literal) = Clause(ls - lit)
}

case class CNF(clauses: Set[Clause]) {
  def &(that: CNF): CNF = CNF(this.clauses ++ that.clauses)

  def |(that: CNF) = {
    CNF(for (a: Clause <- this.clauses; b <- that.clauses) yield (a | b))
  }

  def subs(m: PartialFunction[Var, Term]) =
    CNF(clauses map ((l: Clause) => l.subs(m)))

  val hasContradiction: Boolean = clauses.exists(_.isContradiction)

  def findUnit: Option[Literal] = clauses.find(_.isUnit).map(_.ls.head)

  def inferFrom(lit: Literal): CNF =
    CNF(clauses.filterNot(cl => cl.ls.contains(lit)).map(_ - lit.negate))

  lazy val literals: Set[Literal] = clauses.flatMap(_.ls)

  lazy val varList = literals.map(_.p).toList

  def findPure: Option[Literal] =
    literals.find(lit => !literals.contains(lit.negate))

  def purgePure(pure: Literal) = CNF(clauses.filterNot(_.ls.contains(pure)))
}

object CNF {
  def idVar: PartialFunction[Var, Term] = { case x: Var => x }

  def cnfRec(fmla: Formula, outerVars: List[Var]): CNF = fmla match {
    // case p: Formula        => CNF(Set(Clause(Set(PosLit(p)))))
    case ConjFormula(p, "&", q)  => cnfRec(p, outerVars) & cnfRec(q, outerVars)
    case ConjFormula(p, "|", q)  => cnfRec(p, outerVars) | cnfRec(q, outerVars)
    case ConjFormula(p, "=>", q) => cnfRec(!p, outerVars) | cnfRec(q, outerVars)
    case ConjFormula(p, "<=>", q) =>
      (cnfRec(p, outerVars) & cnfRec(q, outerVars)) |
        (cnfRec(!p, outerVars) & cnfRec(!q, outerVars))
    case NegFormula(NegFormula(p)) => cnfRec(p, outerVars)
    // case NegFormula(p: Formula) => CNF(Set(Clause(Set(NegLit(p)))))
    case NegFormula(p: Formula) => cnfRec(negate(p), outerVars)

    case UnivQuantFormula(x: Var, p) => cnfRec(p, x :: outerVars)
    case ExQuantFormula(x, p) =>
      val vars = outerVars
      val t    = new SkolemFunction(x, vars.length)(vars)
      cnfRec(p, outerVars) subs (Map(x -> t) orElse idVar)
    case Prop(name) => atom(Prop(name))
  }

  def atom(fmla: Formula) = CNF(Set(Clause(Set(PosLit(fmla)))))

  def fromFormula(fmla: Formula): CNF = cnfRec(fmla, fmla.freeVars.toList)

  def union(cnfs: Set[CNF]): CNF = CNF(cnfs.flatMap(_.clauses))

  def fromFormulas(fmlas: Set[Formula]): CNF = union(fmlas.map(fromFormula(_)))
}
