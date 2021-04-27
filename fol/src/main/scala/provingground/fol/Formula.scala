package provingground.fol

trait Formula extends Expression {

  /** Logical and */
  def &(that: Formula): Formula = ConjFormula(this, "&", that)

  /** Logical or */
  def |(that: Formula): Formula = ConjFormula(this, "|", that)

  /** Logical implies */
  def implies(that: Formula): Formula = ConjFormula(this, "=>", that)

  /** Logical Equivalent */
  def equiv(that: Formula): Formula =
    ConjFormula(this, "<=>", that) // change to <-> ??
  /** Logical not */
  def unary_! : Formula = NegFormula(this)

  /** TermListitute terms for variables, should be abstract*/
  def subs(xt: Var => Term): Formula

  /** TermListituting a sigle variable */
  def subs(x: Var, t: Term): Formula = {
    val xt: (Var => Term) = (y: Var) => if (y == x) t else y
    subs(xt)
  }

  /** ForAll added for free variables */
  lazy val sentence =
    if (freeVars == Set.empty) this
    else freeVars.foldRight(this)(UnivQuantFormula(_, _))

  val freeVars: Set[Var]
}

/** Logical Formulas */
object Formula {
  object empty extends Formula {
    def subs(xt: Var => Term): Formula = this
    val freeVars: Set[Var]             = Set.empty
  }

  def or(p: Formula, q: Formula): Formula = p | q

  /** Given Boolean values for base formulas, recursively deduces for Conjunctions and Negations.
    * Typically base cases are:
    * 1. Atomic Formulas in interpretations,
    * 2. Formula Variables for verifying tautologies.
    */
  def recValue(f: Formula, r: Formula => Boolean): Boolean = f match {
    case NegFormula(p)           => !(recValue(p, r))
    case ConjFormula(p, "&", q)  => recValue(p, r) && recValue(q, r)
    case ConjFormula(p, "|", q)  => recValue(p, r) || recValue(q, r)
    case ConjFormula(p, "=>", q) => (!recValue(p, r)) || recValue(q, r)
    case ConjFormula(p, "<=>", q) =>
      (recValue(p, r) && recValue(q, r)) ||
        (!recValue(p, r) && !recValue(q, r))
    case Eq(p, q)   => p == q
    case p: Formula => r(p)
  }

  /* def idFormula: PartialFunction[Formula,Formula] = {case x: Formula => x} */

  /** Given a map on base formulas (eg Formula variables), recursively extends to Conjunctions and Negations. */
  def recLogicFormula(
      r: PartialFunction[Formula, Formula]
  ): PartialFunction[Formula, Formula] = {
    case ConjFormula(p, conj, q) => ConjFormula(r(p), conj, r(q))
    case NegFormula(p)           => NegFormula(r(p))
    case ExQuantFormula(v, p)    => ExQuantFormula(v, r(p))
    case UnivQuantFormula(v, p)  => UnivQuantFormula(v, r(p))
    case Eq(p, q)                => Eq(p, q)
  }

  /** Given a map on base formulas, recursively extends to Conjunctions and Negations and default to Identity */
  def recFormula(f: Formula, r: PartialFunction[Formula, Formula]): Formula =
    recLogicFormula(r).applyOrElse(f, (q: Formula) => q)

  /** Offsping of Recursive formulas */
  def offspring(f: Formula): Set[Formula] = f match {
    case NegFormula(p)          => Set(p)
    case ConjFormula(p, _, q)   => Set(p, q)
    case ExQuantFormula(x, p)   => Set(p)
    case UnivQuantFormula(x, p) => Set(p)
    case _                      => Set()
  }

  /* Descendants of (recursive) formulas */
  def desc(f: Formula): Set[Formula] = offspring(f) flatMap (desc(_))

  def forAll(xs: Var*)(p: Formula): Formula =
    xs.foldRight(p)(UnivQuantFormula(_, _))

  def exists(xs: Var*)(p: Formula): Formula =
    xs.foldRight(p)(ExQuantFormula(_, _))

  def dual(x: Var): (Var => Formula) => Formula = {
    def evalx(P: Var => Formula): Formula = P(x)
    evalx(_)
  }

  type Condition = Formula => Formula
  type Propt     = Var => Formula
  type CondPropt = (Var, Formula) => Formula

  def eval(x: Var, p: Var => Formula): Formula = p(x)

  def evalF(c: Formula => Formula, p: Formula) = c(p)

  def subs(baseFmla: Formula, vars: List[Var], props: List[Propt]): Formula = {
    val pairs    = vars zip props
    val fmlaList = for ((x, p) <- pairs) yield p(x)
    fmlaList.foldLeft(baseFmla)(_ & _)
  }

  def subs(baseFmla: Formula, x: Var, props: List[Propt]): Formula = {
    def evalx(p: Var => Formula): Formula = p(x)
    val fmlaList: List[Formula]           = props map (evalx)
    fmlaList.foldLeft(baseFmla)(_ & _)
  }

  /** Zips together a list of Formula => Formula variables and a base Formula. */
  def fmlaZip(c: List[Formula => Formula], base: Formula) =
    c.foldRight(base)(evalF)

  /** TermListitutes variables in (Var, Formula)=> Formula to get Formula=> Formula, and then zips with base. */
  def zipSubs(
      c: List[(Var, Formula) => Formula],
      xs: List[Var],
      base: Formula
  ) = {
    def evalx(p: (Var, Formula) => Formula, x: Var): Formula => Formula =
      p(x, _)
    val fmlaChain: List[Formula => Formula] = c.lazyZip(xs).map(evalx)
    fmlaZip(fmlaChain, base)
  }

  def negate(fmla: Formula): Formula = fmla match {
    case p: AtomicFormula         => NegFormula(p)
    case NegFormula(p)            => p
    case ConjFormula(p, "&", q)   => negate(p) | negate(q)
    case ConjFormula(p, "|", q)   => negate(p) & negate(q)
    case ConjFormula(p, "=>", q)  => p & negate(q)
    case ConjFormula(p, "<=>", q) => (p & negate(q)) | (q & negate(p))
    case ExQuantFormula(x, p)     => UnivQuantFormula(x, negate(p))
    case UnivQuantFormula(x, p)   => ExQuantFormula(x, negate(p))
    case Prop(name) => NegFormula(Prop(name))
  }

}

/** Formulas built by Conjunctions and Negations */
trait RecFormula extends Formula

/** Formulas built Conjunctions */
case class ConjFormula(p: Formula, conj: String, q: Formula)
    extends RecFormula {
  override def subs(xt: Var => Term): Formula =
    ConjFormula(p subs xt, conj, q subs xt)
  val freeVars: Set[Var] = p.freeVars union q.freeVars
  override def toString  = p.toString + conj + q.toString
}

/** Formulas built by Negation */
case class NegFormula(p: Formula) extends RecFormula {
  def subs(xt: Var => Term): Formula = NegFormula(p subs xt)
  val freeVars: Set[Var]             = p.freeVars
  override def toString              = "!" + p.toString
}

/** Exists(x) Formula */
case class ExQuantFormula(v: Var, p: Formula) extends Formula {
  def subs(xt: Var => Term): Formula = ExQuantFormula(v, p subs xt)
  val freeVars: Set[Var]             = p.freeVars - v

  override def toString = "Ex. " + v.toString + " " + p.toString
}

/** ForAll(x) Formula */
case class UnivQuantFormula(v: Var, p: Formula) extends Formula {
  def subs(xt: Var => Term): Formula = UnivQuantFormula(v, p subs xt)
  val freeVars: Set[Var]             = p.freeVars - v

  override def toString = "ForAll " + v.toString + " " + p.toString
}

/** Atomic Formulas */
trait AtomicFormula extends Formula {
  val pred: Pred
  val params: List[Term]
}

case class AtomFormula(pred: Pred, params: List[Term]) extends AtomicFormula {
  def this(p: Pred, t: Term) = this(p, List(t))
  def subs(xt: Var => Term): Formula =
    AtomFormula(pred, params map (_.subs(xt)))
  val freeVars: Set[Var] = (params map (_.freeVars)) reduce (_ union _)

  override def toString = pred.toString + params.mkString("(", ", ", ")")
}

/** Equality formula; equality may also be given by conjunction formula */
case class Eq(p: Term, q: Term) extends AtomicFormula {
  def subs(xt: Var => Term): Formula = Eq(p subs xt, q subs xt)
  val freeVars: Set[Var]             = p.freeVars union q.freeVars
  val pred: Pred                     = BinRel("=")
  val params                         = List(p, q)
  override def toString              = p.toString + "=" + q.toString
}

/** Boolean True as Formula */
case object True extends Formula {
  def subs(xt: Var => Term): Formula = this
  val freeVars: Set[Var]             = Set()
}

/** Boolean False as Formula */
case object False extends Formula {
  def subs(xt: Var => Term): Formula = this
  val freeVars: Set[Var]             = Set()
}

/** Formula valued variable */
class FormulaVar(val freeVars: Set[Var]) extends Formula {
  def subs(xt: Var => Term): Formula = this
  def this() = this(Set())
}

case class Prop(name: String) extends Formula{
  def subs(xt: Var => Term): Formula = this
  
  val freeVars: Set[Var] = Set()
  
}

/** Formula with an explicit list of Formula variables*/
trait Schema extends Formula {
  import Formula._

  /** Formula depending on Formula variables */
  val formula: Formula

  /** The formula variables */
  val params: List[FormulaVar]

  /** TermListitute some formulas - typically free variables */
  def apply(f: PartialFunction[Formula, Formula]) = recFormula(formula, f)

  /** TermListitute free variables (in sequence)*/
  def apply(ps: List[Formula]) = {
    val psubs: PartialFunction[Formula, Formula] = (params zip ps).toMap
    recFormula(formula, psubs)
  }

  /** Formulas built by Conjunctions and Negations */
  def apply(ps: Formula*) = {
    val psubs: PartialFunction[Formula, Formula] =
      (params zip ps.toList).toMap
    recFormula(formula, psubs)
  }
}
