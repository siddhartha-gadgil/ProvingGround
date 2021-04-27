package provingground.fol

import Formula.negate

trait Literal {
  val p: Formula
  def replaceFormula(p: Formula): Literal
  def subs(m: PartialFunction[Var, Term]): Literal

  val isPositive: Boolean
}

case class PosLit(p: Formula) extends Literal {
  def replaceFormula(p: Formula): Literal = PosLit(p)
  def subs(m: PartialFunction[Var, Term]): Literal =
    PosLit(p.subs(m).asInstanceOf[Formula])

  val isPositive: Boolean = true
}

case class NegLit(p: Formula) extends Literal {
  def replaceFormula(p: Formula): Literal = NegLit(p)
  def subs(m: PartialFunction[Var, Term]): Literal =
    NegLit(p.subs(m).asInstanceOf[Formula])

  val isPositive: Boolean = false
}

class SkolemFunction(x: Var, d: Int) extends Func(d)

case class Clause(ls: Set[Literal]) {
  def |(that: Clause) = Clause(this.ls ++ that.ls)
  def subs(m: PartialFunction[Var, Term]) =
    Clause(ls map ((l: Literal) => l.subs(m)))
}

case class CNF(clauses: Set[Clause]) {
  def &(that: CNF): CNF = CNF(this.clauses ++ that.clauses)

  def |(that: CNF) = {
    CNF(for (a: Clause <- this.clauses; b <- that.clauses) yield (a | b))
  }

  def subs(m: PartialFunction[Var, Term]) =
    CNF(clauses map ((l: Clause) => l.subs(m)))
}

object CNF {
  def idVar: PartialFunction[Var, Term] = { case x: Var => x }

  def cnfRec(fmla: Formula, outerVars: List[Var]): CNF = fmla match {
    case p: Formula        => CNF(Set(Clause(Set(PosLit(p)))))
    case ConjFormula(p, "&", q)  => cnfRec(p, outerVars) & cnfRec(q, outerVars)
    case ConjFormula(p, "|", q)  => cnfRec(p, outerVars) | cnfRec(q, outerVars)
    case ConjFormula(p, "=>", q) => cnfRec(!p, outerVars) | cnfRec(q, outerVars)
    case ConjFormula(p, "<=>", q) =>
      (cnfRec(p, outerVars) & cnfRec(q, outerVars)) |
        (cnfRec(!p, outerVars) & cnfRec(!q, outerVars))
    case NegFormula(NegFormula(p))    => cnfRec(p, outerVars)
    case NegFormula(p: Formula) => CNF(Set(Clause(Set(NegLit(p)))))
    case NegFormula(p: Formula)       => cnfRec(negate(p), outerVars)

    case UnivQuantFormula(x: Var, p) => cnfRec(p, x :: outerVars)
    case ExQuantFormula(x, p) =>
      val vars = outerVars
      val t    = new SkolemFunction(x, vars.length)(vars)
      cnfRec(p, outerVars) subs (Map(x -> t) orElse idVar)
    case Prop(name) => ???
  } 

  def atom(fmla: Formula) = CNF(Set(Clause(Set(PosLit(fmla)))))

  def fromFormula(fmla: Formula): CNF = cnfRec(fmla, fmla.freeVars.toList)
}
