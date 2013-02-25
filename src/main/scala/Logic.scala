package provingGround 

/** Defines predicate calculus and several associated methods, including:
  *
  * $ - interpretations;
  *
  * $ - substitutions to be used by an evolver
  */
 
object Logic {

private def stringFn(name:String, params: List[String])={
  val head = name + "(" + params.head
  val terms = for (t<- params.tail) yield (", "+t)
  (head/:terms)(_+_)  + ")"
  }

/** Logical expressions */
trait Expression{
  /** Free variables in an expression */
  def freeVars: Set[Var]
  }

/** Logical Functions */
class Func(val degree: Int){
    /** Substitute parameters in the function */
    def apply(params: List[Term]): Term = RecTerm(this, params)
    /** Substitute parameters in the function */
    def apply(params: Term*): Term = RecTerm(this, params.toList)
    
    /** Make function into term with free variables replacing parameters */
    def fill = RecTerm(this, (varstream take degree).toList)
}

/** Functions determined by their name */
case class FuncSym(name: String, d: Int) extends Func(d)

/** Logical Predicates */
class Pred(val degree: Int){
  /** Substitute parameters in the predicate */
  def apply(params: List[Term]): AtomFormula = AtomFormula(this, params)
  /** Substitute parameters in the function */
  def apply(params: Term*) = AtomFormula(this, params.toList)

  /** Make predicate into formula with free variables replacing parameters */
  def fill = AtomFormula(this, (varstream take degree).toList)
  }

/** Predicate determined by its name */
case class PredSym(name: String, d :Int) extends Pred(d)

@deprecated("Use instead formal relations", "8/2/2012") object Pred{
    val Eql = BinRel("=")
    def eqls(s: Term, t:Term)= AtomFormula(Eql, List(s, t))

    val Gt = BinRel(">")
    def gt(s: Term, t:Term)= AtomFormula(Gt, List(s, t))    
    val Lt = BinRel("<")
    def lt(s: Term, t:Term)= AtomFormula(Lt, List(s, t))    
  }
  
/** Binary Operation */  
case class BinOp(name: String) extends Func(2)

/** Unary Operation */
case class UnOp(name: String) extends Func(1)

implicit def binOpFunc(b: BinOp): FuncSym = FuncSym(b.name, 1)

implicit def UnOpFun(u: UnOp): FuncSym = FuncSym(u.name, 1)


/** Binary relation */
case class BinRel(name: String) extends Pred(2)

implicit def binRelPred(b: BinRel) = PredSym(b.name, 2)


/** Logical terms */
trait Term extends Expression{

  /** Formula giving equality */
  def eqls(that:Term) = Eq(this, that)

  /** Substitutes variables by terms: should make xt a PartialFunction */
  def subs(xt: Var=> Term): Term
  
  /** Single variable substituted by a term */
  def subs(x: Var, t: Term): Term = {
    val xt: (Var => Term) = (y: Var) => if (y==x) t else y
    subs(xt)
    }
   
  /** Formal + operation */  
  def +(that: Term) = BinOp("+")(this, that)
  /** Formal -(binary) operation */
  def -(that: Term) = BinOp("-")(this, that)
  /** Formal * operation */
  def *(that: Term) = BinOp("*")(this, that)
  /** Formal / operation */
  def /(that: Term) = BinOp("/")(this, that)
  /** Formal ** operation */
  def **(that: Term) = BinOp("**")(this, that)
  /** Formal | operation */
  def |(that: Term) = BinOp("|")(this, that)
  /** Formal unary - operation */
  def unary_- = UnOp("-")(this)
  
  /** Formal < relation */
  def <(that: Term): Formula = BinRel("<")(this, that)
  /** Formal > relation */
  def >(that: Term): Formula = BinRel(">")(this, that)
  /** Formal = relation */
  def =:=(that: Term): Formula = BinRel("=")(this, that)
  /** Formal <= relation */
  def <=(that: Term): Formula = BinRel("<=")(this, that)
  /** Formal >= relation */
  def >=(that: Term): Formula = BinRel(">=")(this, that)
  /** Formal ~ relation */
  def ~(that: Term): Formula = BinRel("~")(this, that)
  }

/** Logical Variable */
case class Var(name: String) extends Term{
  override def toString = name 
  val freeVars = Set(this)
  def subs(xt: Var => Term): Term = xt(this)
  }
 
/** stream of Variables starting with Var("a") */
val varstream: Stream[Var] = (Stream.from (0)) map ((x: Int) => Var((x + 'a').toChar.toString))

/** Logical constants */
trait Const extends Term{
  override val freeVars: Set[Var] = Set()
  override def subs(xt: Var=> Term)  = this
  }

/** Constants given by name */
case class ConstSym(name: String) extends Const

/** Integer constant */
case class IntConst(value: Int) extends Const{
	override def toString = value.toString
}
  
/** Unparsed term formally wrapped */
case class TermFmla(name:String) extends Term{
  override def toString = name
  val freeVars: Set[Var] = Set()
  def subs(xt: Var=> Term): Term = this  
  }

/** Recursive term */
case class RecTerm(f: Func, params: List[Term]) extends Term{
  def this(f:Func, t: Term)= this(f, List(t))
    
  override def toString = f match {
     case FuncSym(name, _) => stringFn(name, params map (_.toString))
     case _ => super.toString
     }
  val freeVars: Set[Var] = (params map (_.freeVars)) reduce (_ union _)
  def subs(xt: Var=> Term): Term = RecTerm(f, params map (_.subs(xt)))
  }

type Condition = Formula => Formula
type Propt = Var=> Formula
type CondPropt = (Var, Formula) => Formula

/** Logical Formulas */
trait Formula extends Expression{

  /** Logical and */
  def &(that:Formula) : Formula = ConjFormula(this, "&", that)
  /** Logical or */
  def |(that:Formula) : Formula = ConjFormula(this, "|", that)
  /** Logical implies */
  def implies(that:Formula) : Formula = ConjFormula(this, "=>", that)
  /** Logical Equivalent */
  def equiv(that:Formula) : Formula = ConjFormula(this, "<=>", that) // change to <-> ??
  /** Logical not */
  def unary_! : Formula = NegFormula(this)
  override def toString = {
    this match {
        case AtomFormula(PredSym(name, deg), params) => 
          if (deg != 2) stringFn(name, params map (_.toString)) else {
            params.head.toString+" "+ name +" "+params.tail.head.toString
            }  
        case AtomFormula(BinRel(name), params) => 
          params.head.toString+" "+ name +" "+params.tail.head.toString 
        case ConjFormula(p, conj, q) => "("+p.toString +") "+conj+" ("+ q.toString+")"
        case NegFormula(p) => "!"+p.toString
        case ExQuantFormula(v, p) => "Exists "+ v.toString + " " + p.toString
        case UnivQuantFormula(v, p) => "Forall "+ v.toString + " " + p.toString
        case Eq(p, q) => p.toString+" = "+q.toString
        case _ => "Some other formula" 
        }
  
  }
  
  /** Substitute terms for variables, should be rewritten using recFormula */
  def subs(xt: Var=> Term): Formula = this match {
    case AtomFormula(p, params) => AtomFormula(p, params map (_.subs(xt)))
    case ConjFormula(p, conj, q) => ConjFormula(p.subs(xt), conj, q.subs(xt))
    case NegFormula(p) => NegFormula(p.subs(xt))
    case ExQuantFormula(v, p) => ExQuantFormula(v, p.subs(xt))
    case UnivQuantFormula(v,p) => UnivQuantFormula(v, p.subs(xt))
    case Eq(p,q) => Eq(p.subs(xt), q.subs(xt))
    case p: Formula => p
  }
  
  /** Substituting a sigle variable */
  def subs(x: Var, t: Term): Formula = {
    val xt: (Var => Term) = (y: Var) => if (y==x) t else y
    subs(xt)
  }
  
  /** ForAll added for free variables */
  val sentence = (freeVars :\ this) (UnivQuantFormula(_,_)) 
  
  def freeVars: Set[Var] = this match {
    case AtomFormula(_, params) => (params flatMap (_.freeVars)).toSet
    case _ => desc(this) flatMap (_.freeVars) 
  }
}



/** Formulas built by Conjunctions and Negations */
trait RecFormula extends Formula

def or(p: Formula, q: Formula) : Formula = p | q

/** Formulas built Conjunctions */
case class ConjFormula(p:Formula, conj:String, q:Formula) extends RecFormula{
//  val freeVars: Set[Var] = p.freeVars union q.freeVars
  }

/** Formulas built by Negation */
case class NegFormula(p:Formula) extends RecFormula{
//  val freeVars: Set[Var] = p.freeVars
  }

/** Exists(x) Formula */
case class ExQuantFormula(v: Var, p:Formula) extends Formula{
//  val freeVars: Set[Var] = p.freeVars - v
  }

/** ForAll(x) Formula */
case class UnivQuantFormula(v: Var, p:Formula) extends Formula{
//  val freeVars: Set[Var] = p.freeVars - v
  }


/** Atomic Formulas */  
case class AtomFormula(p: Pred, params: List[Term]) extends Formula{
  def this(p:Pred, t: Term)= this(p, List(t))
//  val freeVars: Set[Var] = (params map (_.freeVars)) reduce (_ union _)
  }

/** Equality formula; equality may also be given by conjunction formula */  
case class Eq(p: Term, q: Term) extends Formula{
//  val freeVars: Set[Var] = p.freeVars union q.freeVars
  }
  
implicit def eqFmla(pq: Eq) : AtomFormula = AtomFormula(BinRel("="), List(pq.p, pq.q))
  
/** Boolean True as Formula */
case object True extends Formula

/** Boolean False as Formula */
case object False extends Formula

/** Treat Boolean as Formula */
implicit def trueFalse(b: Boolean): Formula = b match{
  case true => True
  case false => False
  }  
  
  
/** Formula valued variable */
class FormulaVar extends Formula 


private def dual(x: Var): (Var=> Formula) => Formula = {
  def evalx(P: Var=>Formula): Formula = P(x)
  evalx(_)
}

private def eval(x: Var, p: Var=> Formula): Formula = p(x)

private def evalF(c: Formula=> Formula, p: Formula) = c(p)




def subs(baseFmla : Formula, vars : List[Var], props : List[Propt]): Formula ={
  val pairs= vars zip props
  val fmlaList = for ((x, p) <- pairs) yield p(x) 
  (baseFmla /: fmlaList)(_ & _)
}
 
def subs(baseFmla : Formula, x : Var, props : List[Propt]):Formula ={
  def evalx(p: Var=> Formula): Formula = p(x)
  val fmlaList : List[Formula] = props map(evalx)
  (baseFmla /: fmlaList)(_ & _)
}

/** Zips together a list of Formula => Formula variables and a base Formula. */
def fmlaZip(c : List[Formula=>Formula], base: Formula)= (c :\ base)(evalF)

/** Substitutes variables in (Var, Formula)=> Formula to get Formula=> Formula, and then zips with base. */
def zipSubs(c: List[(Var,Formula)=>Formula], xs : List[Var], base: Formula) ={
  def evalx(p: (Var, Formula) => Formula, x: Var): Formula=> Formula = p(x,_)
  val fmlaChain: List[Formula=> Formula] = (c, xs).zipped.map(evalx)
  fmlaZip(fmlaChain, base)
}

/** And over a stream, stops with first false */
def bigAnd(bs: Stream[Boolean]): Boolean = {
  if (bs.isEmpty) true else {
    if (!bs.head) false else bigAnd(bs.tail)
  }
}

/** Or over a stream, stops with first true */
def bigOr(bs: Stream[Boolean]): Boolean = {
  if (bs.isEmpty) false else {
    if (bs.head) true else bigAnd(bs.tail)
  }
}

/** Builds expressions by:
  * 1 Quantifying over free variables in a formula
  * 2 Substituting terms for all free variables in a formula
  * 3 Substituting terms for free variables in terms.
  * 4 Combining formulas by conjunction and negation.
  */
val exprProdSet: PartialFunction[(Expression, Expression), Set[Expression]] ={
  case (p: Formula, y: Var) if (p.freeVars contains y) =>
    (for (x<-p.freeVars) yield p.subs(x, y)).toSet union Set(ExQuantFormula(y, p), UnivQuantFormula(y,p))
  case (p: Formula, t: Term) => 
    (for (x<-p.freeVars) yield p.subs(x, t)).toSet
  case (p: Term, t: Term) => 
    (for (x<-p.freeVars) yield p.subs(x, t)).toSet
  case (p: Formula, q: Formula) =>
    Set(p & q, p | q, p implies q, p equiv q, !p)
  case (p: Formula, _) => Set(!p)
}

/** Builds terms by substituting other terms for free variables */
val termProdSet: PartialFunction[(Term, Term), Set[Term]] ={
  case (p: Term, t: Term) if (!p.freeVars.isEmpty) => 
    (for (x<-p.freeVars) yield p.subs(x, t)).toSet
}

/** Given Boolean values for base formulas, recursively deduces for Conjunctions and Negations.
  * Typically base cases are: 
  * 1. Atomic Formulas in interpretations,
  * 2. Formula Variables for verifying tautologies.
  */
def recValue(f: Formula, r: Formula => Boolean): Boolean = f match {
    case NegFormula(p) => !(recValue(p,r))
    case ConjFormula(p, "&", q) => recValue(p,r) && recValue(q,r)
    case ConjFormula(p, "|", q) => recValue(p,r) || recValue(q,r)
    case ConjFormula(p, "=>", q) => (!recValue(p,r)) || recValue(q,r)
    case ConjFormula(p, "<=>", q) => (recValue(p,r) && recValue(q,r)) || (!recValue(p,r) && !recValue(q,r))
    case Eq(p, q) => p==q
    case p: Formula => r(p)
}

def idFormula: PartialFunction[Formula,Formula] = {case x: Formula => x}

/** Given a map on base formulas (eg Formula variables), recursively extends to Conjunctions and Negations. */
def recFormula(f: Formula, r: PartialFunction[Formula, Formula]): Formula = f match {
    case AtomFormula(p, params) => AtomFormula(p, params)
    case ConjFormula(p, conj, q) => ConjFormula(r(p), conj, r(q))
    case NegFormula(p) => NegFormula(r(p))
    case ExQuantFormula(v, p) => ExQuantFormula(v, r(p))
    case UnivQuantFormula(v,p) => UnivQuantFormula(v, r(p))
    case Eq(p,q) => Eq(p, q)
    case p: Formula => (r orElse idFormula)(p)
}

/** Formula with an explicit list of Formula variables*/
trait Schema extends Formula{
  /** Formula depending on Formula variables */
  val formula : Formula
  /** The formula variables */
  val params: List[FormulaVar]
  /** Substitute some formulas - typically free variables */ 
  def apply(f: PartialFunction[Formula, Formula]) = recFormula(formula, f)
  /** Substitute free variables (in sequence)*/
  def apply(ps: List[Formula])={
    val psubs: PartialFunction[Formula, Formula] = (params zip ps).toMap
    recFormula(formula, psubs)
  }
  /** Formulas built by Conjunctions and Negations */
  def apply(ps: Formula*)={
    val psubs: PartialFunction[Formula, Formula] = (params zip ps.toList).toMap
    recFormula(formula, psubs)
  }
}

/** Offsping of Recursive formulas */
def offspring(f: Formula): Set[Formula]=f match{
    case NegFormula(p) => Set(p)
    case ConjFormula(p, _ , q) => Set(p,q)
    case ExQuantFormula(x, p) => Set(p)
    case UnivQuantFormula(x, p) => Set(p)
    case _ => Set()
}

/* Descendants of (recursive) formulas */
def desc(f: Formula): Set[Formula] = offspring(f) flatMap (desc(_)) 

    
type OnParams[A, B] = PartialFunction[List[A], B]

/** Primary maps for an interpretation */
trait PrimMap[A]{
  def primConst: PartialFunction[Const, A]
  def primFunc: PartialFunction[Func, OnParams[A, A]]
  def primPred: PartialFunction[Pred ,OnParams[A, Boolean]]
}
// This is fixed for a given interpretation as the stream varies

/** Logical Interpretation (data for Model, but no axioms) */  
trait Model[A] extends PrimMap[A]{
  /** The universe */
  val M: Stream[A]
  
  /** Parameter space for free variables */
  type Mbar = Var => A
  /** Secondary map on terms */
  def sec(t: Term, z: Mbar): A = t match {
    case x: Var => z(x)
    case c: Const => primConst(c)
    case RecTerm(f, params) => primFunc(f)(params map (sec(_, z)))
    }
  
  
  private def changeVar(x: Var, a: A, z: Mbar): Mbar = (y: Var) => if (x==y) a else z(y)
    
  /** Variation of z along x */  
  def variation(x: Var, z: Mbar): Stream[Mbar] = M map (changeVar(x, _ , z))
  
  
  private def varFormula(x: Var, z: Mbar, phi: Formula): Stream[Boolean] = variation(x, z) map (sec(phi,_)) 
  
  /** Secondary map for formulas, depending of parameters for variables */
  def sec(phi: Formula, z: Mbar): Boolean = phi match {
    case AtomFormula(f, params) => primPred(f)(params map (sec(_, z)))
    case f: RecFormula => recValue(phi, (p:Formula) => sec(p,z))
//    case NegFormula(p) => !(sec(p,z))
//    case ConjFormula(p, "&", q) => sec(p,z) && sec(q,z)
//    case ConjFormula(p, "|", q) => sec(p,z) || sec(q,z)
//    case ConjFormula(p, "=>", q) => (!sec(p,z)) || sec(q,z)
//    case ConjFormula(p, "<=>", q) => (sec(p,z) && sec(q,z)) || (!sec(p,z) && !sec(q,z))
    case ExQuantFormula(x, p) => bigOr(varFormula(x, z, p))
    case UnivQuantFormula(x, p) => bigAnd(varFormula(x, z, p))
  }
  
  
  
  private val zhead: Mbar = (x: Var) => M.head
  
  /** Check formula in interpretation after Universal quantification of free variables */  
  def check(f: Formula): Boolean = sec(f.sentence, zhead)
}
}