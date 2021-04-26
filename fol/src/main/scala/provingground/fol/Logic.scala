package provingground.fol

// import Logic._, Var.varstream

/** Parameters for a First-Order Language */
trait LanguageParam

/** Logical Functions */
class Func(val degree: Int) extends LanguageParam {

  /** TermListitute parameters in the function */
  def apply(params: List[Term]): Term = RecTerm(this, params)

  /** TermListitute parameters in the function */
  def apply(params: Term*): Term = RecTerm(this, params.toList)

  /** Make function into term with free variables replacing parameters */
  // def fill = RecTerm(this, (varstream take degree).toList)
}

/** Functions determined by their name */
case class FuncSym(name: String, d: Int) extends Func(d)

/** Logical Predicates */
class Pred(val degree: Int) extends LanguageParam {

  /** TermListitute parameters in the predicate */
  def apply(params: List[Term]): AtomFormula = AtomFormula(this, params)

  /** TermListitute parameters in the function */
  def apply(params: Term*) = AtomFormula(this, params.toList)

  /** Make predicate into formula with free variables replacing parameters */
  // def fill = AtomFormula(this, (varstream take degree).toList)
}

/** Predicate determined by its name */
case class PredSym(name: String, d: Int) extends Pred(d) {
  override def toString = name
}

/** Binary Operation */
case class BinOp(name: String) extends Func(2)

/** Unary Operation */
case class UnOp(name: String) extends Func(1)

/** Binary relation */
case class BinRel(name: String) extends Pred(2) {
  def apply(term: Term) = UnRel(name + "(" + term.toString + ")")
}
case class UnRel(name: String) extends Pred(1) {
  def ::(term: Term) = apply(term)
}

object Logic {
  

}
