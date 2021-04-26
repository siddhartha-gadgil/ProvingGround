package provingground.fol

trait Expression {

  /** Free variables in an expression */
  def freeVars: Set[Var]
}

object Expression {

  /** Builds expressions by:
    * 1 Quantifying over free variables in a formula
    * 2 TermListituting terms for all free variables in a formula
    * 3 TermListituting terms for free variables in terms.
    * 4 Combining formulas by conjunction and negation.
    */
  val exprProdSet
      : PartialFunction[(Expression, Expression), Set[Expression]] = {
    case (p: Formula, y: Var) if (p.freeVars contains y) =>
      (for (x <- p.freeVars) yield p.subs(x, y)).toSet union Set(
        ExQuantFormula(y, p),
        UnivQuantFormula(y, p)
      )
    case (p: Formula, t: Term) =>
      (for (x <- p.freeVars) yield p.subs(x, t)).toSet
    case (p: Term, t: Term) =>
      (for (x <- p.freeVars) yield p.subs(x, t)).toSet
    case (p: Formula, q: Formula) =>
      Set(p & q, p | q, p implies q, p equiv q, !p)
    case (p: Formula, _) => Set(!p)
  }

  /** Builds terms by substituting other terms for free variables */
  val termProdSet: PartialFunction[(Term, Term), Set[Term]] = {
    case (p: Term, t: Term) if (!p.freeVars.isEmpty) =>
      (for (x <- p.freeVars) yield p.subs(x, t)).toSet
  }
}
