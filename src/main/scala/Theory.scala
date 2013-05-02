package provingGround

import provingGround.Logic._
import provingGround.Structures._
import provingGround.Aware._
import scala.language.implicitConversions

/** The Meta-logical layer, including Definitions, Propositions, Proofs etc. */
object Theory{
  
	/** Fragments of sentences */
  trait Phrases
  
  /** Any block of full sentences*/
  trait Paragraph extends Phrases
  
  /** Recursive Paragraph */
  case class Para(paraList: List[Paragraph]) extends Paragraph
  
	/** Sequences of Paragraphs made into a recursive paragraph */
  def para(paras: Paragraph*) = Para(paras.toList)
  
  
  /** Text not to be translated into mathematics */ 
  case class Text(text: String) extends Paragraph 

  /** Formula regarded as a paragraph */
  case class Fmla(p: Formula) extends Paragraph
  
  /** Paragraph in a fixed domain */
  case class InDomain(S: ZFC.AbsSet, p: Paragraph) extends Paragraph
  
  def inDomain(S: ZFC.AbsSet)(paras: Paragraph*) = InDomain(S, Para(paras.toList)) 
  
  /** Axiom */
  trait Axiom extends Phrases{
		/** the assumption of the Axiom */
    val axiom: Formula
    
		/** New Axiom adding Formula to axiom */
    def &(that: Formula) = Axioms(axiom, that)

		/** New Axiom combining Axioms */
    def &(that: Axiom) = Axioms(axiom, that.axiom) 
    
		/** Returns (axiom => formula) given Formula */
    def have(that: Formula): Formula = axiom implies that
		/** Returns (axiom => claim) given Claim */
    def have(that: Claim): Formula = axiom implies (that.claim)
    }
    
	/** A collection of axioms giving an Axiom*/	
  case class Axioms(axioms: Formula*) extends Paragraph with Axiom{
    val axiomList = axioms.toList
    val axiom = axiomList reduce (_ & _)
    }

	/** Generator for axioms from a list */
	def axiom(axioms:List[Formula]) = Axioms(axioms reduce (_ & _)) 
    
	
  val x = VarSym("X")
  val y = VarSym("y")
  val z = VarSym("z")
    
	/** Axiom saying a Binary relation is symmetric */
  def symmetric(r: BinRel): Axiom = Axioms(forAll(x, y)(r(x,y) implies r(y,x)))
    
	/** Axiom saying a Binary relation is transitive */
  def transitive(r: BinRel): Axiom = Axioms(forAll(x, y, z)((r(x,y) & r(y,z)) implies r(x,z)))

	/** Axiom saying a Binary relation is reflexive */  
  def reflexive(r: BinRel): Axiom = Axioms(forAll(x)(r(x,x)))
  
	/** Axiom saying a Binary relation is an Equivalence relation */
  def equivRelation(r: BinRel): Axiom = symmetric(r) & reflexive(r) & transitive(r)

    
  /** Data: A collection of terms and an axiom they are supposed to satisfy 
		*
		* In practice, the axiom is a collection of axioms combined with &
		*/  
  trait Data extends Term with Axiom{
    /** Substitute in term as well as in the assumed axiom */
    def subsData(xt: Var => Term): Data = DataTerm(subs(xt), axiom.subs(xt))
      
    /** Such that : Add an axiom */
    def suchthat(cond: Formula) = DataCond(this, cond)
    /** Such that: Add an axiom */
    def st(cond: Formula) = suchthat(cond)
    
    /** Add a data term */
    def and(that: Data) = given(this, that)
    
    /** Add a data term */
    def &(that: Data) = and(that)
    
    /** Quotient Data */
    def upto(r: EquivRelation) = QuotientData(this, r)
    
    /** Data representing other term */
    def represents(term: Term) = Represent(this, term)
  }

  case class Represent(data: Data, term: Term) extends Formula{
		def subs(xt: Var => Term): Formula = Represent(dataTerm(data subs xt), term subs xt)
		val freeVars: Set[Var] = Set.empty
  }

	/** The equivalence relation generated by the given generator axiom */
  case class EquivRelation(rel: BinRel, generator: Axiom) extends Axiom{
    val axiom = (generator & equivRelation(rel)).axiom
    }
  
	/** Data considered upto an Equivalence relation */
  case class QuotientData(d: Data, r: EquivRelation) extends Data{
    val freeVars = d.freeVars
    def subs(xt: Var => Term): Term = QuotientData(d.subsData(xt), r)
    val axiom = (d.axiom) & (r.axiom) 
    }
  
  /** A single term as data */
  case class DataTerm(term: Term, axiom: Formula = True) extends Data{
    def subs(xt: Var=> Term): Term = DataTerm(term.subs(xt))
    val freeVars = term.freeVars
    } 
  
  /** Convert term to data */
  implicit def dataTerm(term: Term): Data = DataTerm(term)
  
  /** A list of terms as data */
  case class DataList(lt: List[Data]) extends Data{
    def subs(xt: Var=> Term): Term = DataList(lt map (_.subsData(xt)))
    val freeVars: Set[Var] = (lt map (_.freeVars)) reduce (_ union _)
    val axiom = True
  }
  
  /** Add a condition to the data */
  case class DataCond(data: Data, cond: Formula) extends Data{
    def subs(xt: Var=> Term): Term = DataCond(subsData(xt), cond.subs(xt))
    val freeVars = data.freeVars
    val axiom = data.axiom & cond
  }
  
  /** Constructs a DataList from several data parameters */ 
  def given(ds: Data*) = DataList(ds.toList)
  
  class LanguageMap(cm: Map[Const, Const] = Map.empty, fm: Map[Func, Func]= Map.empty, pm: Map[Pred, Pred]=Map.empty){
    def apply(t: Term): Term = t match {
      case c: Const => cm.applyOrElse(c, (c: Const) => c)
      case RecTerm(f, params) => RecTerm(fm.applyOrElse(f, (g: Func)=> g), params map ((t: Term) => apply(t)))
			case term: Term => term
			}
		
		val formulaMap: PartialFunction[Formula, Formula] = {
				case AtomFormula(p, params) => AtomFormula(pm.applyOrElse(p, (g: Pred)=> g), params map ((t: Term) => apply(t)))
				}
	
		def apply(formula: Formula): Formula = recFormula(formula, formulaMap)
    }
  
  val DefaultLangMap = new LanguageMap()
  
  case class Structure(signature: List[LanguageParam], axiom: Formula) extends Axiom
  
  class SetObject(val set: ZFC.AbsSet, val struct: Structure, lm: LanguageMap = DefaultLangMap)
  
  /** A result; may be just a reference to one */
  trait Result extends Phrases
  
	/** Result refered to by name */
  case class ResultRef(name: String) extends Result
  
  /** A claim */
  trait Claim extends Paragraph with Result{val claim: Formula}
  
	/** A logical contradiction */
  case object Contradiction extends Claim{val claim: Formula = False}
  

  /** Formal Property: Given data gives a Formula corresponding to satisfying this 
		* This is simply a property name, which we can bind to a definition by an axiom
		*/
  abstract class Property(deg: Int) extends Pred(deg) with Axiom{
		/** returns data with additional condition given by the formal property
			* For example, {{{p :: prime}}} returns p with axiom prime(p)
			*/       
    def ::(d: Data)= DataCond(d, apply(d))
		/** Returns condition corresponding this viewed as a predicate
			* For example, {{{p ::: prime}}} returns prime(p)
			*/
		def :::(t: Term): Formula = apply(t)
  }
  
	/** Property given by a definition 
		* The definition is given by a function of a predicate, for example
		* to define prime(p) we use definition
		* {{(prime: Pred) => (prime(p) implies ((p=:=1) or (p=:=p))}} 
		*/
  case class PropertyDefn(deg: Int, defn: Pred => Formula) extends Property(deg) with Axiom{
    val axiom = defn(this)
  }
  
	/** Function with a given condition */
  abstract class Mapping(deg: Int) extends Func(deg) with Axiom
  
	/** Function with a defining axiom. For example, we can define square by
		* (sq: Func) => sq(x) =:= x * x
		*/
  case class MapDefn(deg: Int, defn: Func => Formula) extends Mapping(deg){
    val axiom = defn(this)
  }

	case class TermDefn(term: Term, defn: Term => Formula) extends Assumption{
		val axiom = defn(term)
		}

	implicit def termPropt(p: Var => Formula) : Term => Formula = (t: Term) => {
				p(VarSym("x")).subs(VarSym("x"), t)
			}
                         
	/** Justification for a claim */
  trait Justification extends Phrases{
		/** combining justifications */
    def &(that: Justification) = PolyJust(this, that)
		/** combining justifications */ 
    def and(that: Justification) = PolyJust(this, that)

		/** Assertion based on justification */
		def have(p: Formula) = Assert(p, List(this))
		/** Assertion based on justification */
		def observe(p: Formula) = Assert(p, List(this))
  }
  
	/** Recursive step for a collection of justifications*/
  case class PolyJust(first: Justification, second: Justification) extends Justification
  

  /** A method of proving, e.g. induction */
  trait Method
  
	/** Method given by its name */
  case class NamedMethod(name: String) extends Method
  
	/** Justification: by method */
  case class By(meth: Method) extends Justification
  
	/** Justification: by method */
  def by(meth: Method) = By(meth)
  
	/** Justification: by method with the given name */
  def by(name: String) = By(NamedMethod(name))
  
  /** Justification: using result */
  case class Using(result: Result) extends Justification
  
	/** Justification: using result */
  def using(result: Result) = Using(result)
  
	/** Justification: using result with the given name */
  def using(name: String) = Using(ResultRef(name))
  
  /** ConditionClause for a Formula */
  trait ConditionClause extends Phrases{
		/** returns formula given condition */
    def apply(p: Formula): Formula
    
		/** returns formula given condition */
    def thenhave(p: Formula) = apply(p)

    }

	/** If condition */
  case class If(q: Formula) extends ConditionClause{
    def apply(p: Formula) = q implies p
    }    
  
	/** Justification: As condition holds */  
  case class As(c: ConditionClause) extends Justification
  
	/** Justification: As condtion holds */
  def as(c: ConditionClause) = As(c)
  
	/** Justification: As result holds */
  def as(result: Result) = Using(result)
  
	/** An Assumption */
  trait Assumption extends ConditionClause with Axiom with Paragraph{
    def apply(p: Formula) = axiom implies p
  }
  
	/** Assume axiom */
  case class Assume(axiom: Formula) extends Assumption
 
	/** Stop Assuming */
	case class DontAssume(ass: Assumption) extends Paragraph

	/** Fix a term with assumptions; globally become ForAll */
  trait Fix extends Data with Paragraph
  
	/** Stop fixing a term */
	case class UnFix(fix: Fix) extends Paragraph

	/** Choose a term with assumptions; globally becomes Exists */
  trait Choose extends Data with Paragraph
      
	/** Consequence of some assumption, not a full claim by itself */
  trait Consequence extends Axiom with Paragraph
  
	/** A consequence of some assumptions */
  case class Then(axiom: Formula) extends Consequence

	/** A consequence of some assumptions */  
  def have(p: Formula) = Then(p)

	/** A consequence of some assumptions */
	object Have{
		def apply(p: Formula) = Then(p)
		}

	/** Justifications for a statement */  
  trait Because{
    val because: List[Justification]
  }
    
	/** Assertion: justified claim */
  trait Assertion extends Claim with Because{
		/** Justify assertion by method*/
    def by(meth: Method) = Assert(claim, because :+ By(meth))
		/** Justify assertion by method with given name*/
    def by(name: String) = Assert(claim, because :+ By(NamedMethod(name)))
    
		/** Justify assertion by result*/
    def using(result: Claim) = Assert(claim, because :+ Using(result))
		/** Justify assertion by result with given name or description*/
    def using(name: String) = Assert(claim, because :+ Using(ResultRef(name)))
	
	/** Additional conditions for assertion */
	def where(assumption: Formula) = Assert(assumption implies claim, because)
	
  }
  
	/** Assert without justification */
  case class Assert(claim: Formula, because: List[Justification] = List()) extends Assertion

	/** Assert without justification */
	object Observe{
		def apply(claim: Formula, because: List[Justification] = List())= Assert(claim, because)
		}  

	/** Conclusion, i.e., assertion justified by above */
  trait Conclusion extends Assertion
  
	/** Make conclusion */
  case class Conclude(claim: Formula, because: List[Justification] = List()) extends Conclusion
  
	/** QED */
  case object QED extends Paragraph
    
	/** Refer to present where reference required but not explicit */
  implicit case object ThisPara extends Paragraph
  
	/** Start a proof */
  case class BeginProof(result: Paragraph = ThisPara) extends Paragraph 
  
	/** Conclude a proof */
	case class ConcudeProof(result: Paragraph = ThisPara) extends Paragraph
  
	/** Assert a given formula (implicitly) */
  implicit def assert(p: Formula): Assertion = Assert(p)
  
  /** Justified by above */
  case object ByAbove extends Justification
  
	/** Justified by above and possible explicit justifications */
  def thus(ass: Assertion) = Assert(ass.claim, ByAbove :: ass.because)
  
	/** Justified by above and possible explicit justifications */
  def hence(ass: Assertion) = Assert(ass.claim, ByAbove :: ass.because)

	/** Justified by above and possible explicit justifications */
  def therefore(ass: Assertion) = Assert(ass.claim, ByAbove :: ass.because)

	/** Justified by above and possible explicit justifications */
  def deduce(ass: Assertion) = Assert(ass.claim, ByAbove :: ass.because)

	/** Justified by above */
	def thus(p: Formula) = Assert(p, List(ByAbove))
  
	/** Justified by above */
  def hence(p: Formula) = Assert(p, List(ByAbove))

	/** Justified by above */
  def therefore(p: Formula) = Assert(p, List(ByAbove))

	/** Justified by above */
  def deduce(p: Formula) = Assert(p, List(ByAbove))


	/** Claim in the form of Data, Hypothesis and Conclusion */
  class Propn(val data: Data, val hypothesis: Formula, val conclusion: Formula) extends Claim{
    val claim = (data.axiom & hypothesis) implies conclusion
    }
  
  trait Label{
    val label: String
    }
  

  def theorem(name: Any)(clm: Claim) = new Claim with Label{val claim = clm.claim; val label = name.toString}
  
  def theorem(name: Any)(clm: Formula) = new Claim with Label{val claim = clm; val label = name.toString}



  def thm(name: Any)(clm: Claim) = new Claim with Label{val claim = clm.claim; val label = name.toString}

  def thm(name: Any)(clm: Formula) = new Claim with Label{val claim = clm; val label = name.toString}


  def lemma(name: Any)(clm: Claim) = new Claim with Label{val claim = clm.claim; val label = name.toString}

  def lemma(name: Any)(clm: Formula) = new Claim with Label{val claim = clm; val label = name.toString}


  def proposition(name: Any)(clm: Claim) = new Claim with Label{val claim = clm.claim; val label = name.toString}

  def proposition(name: Any)(clm: Formula) = new Claim with Label{val claim = clm; val label = name.toString}


  def propn(name: Any)(clm: Claim) = new Claim with Label{val claim = clm.claim; val label = name.toString}

  def propn(name: Any)(clm: Formula) = new Claim with Label{val claim = clm; val label = name.toString}

 
	case class Prove(p: Formula) extends DeterminateTask 

	case class Solve(p: Formula, xs: List[Var]=List()) extends Task 
	

	def solve(p: Formula) = Solve(p, p.freeVars.toList)

	def solve(p: Formula, x: Var) = Solve(p, List(x))

	def solve(p: Formula, xs: List[Var]) = Solve(p, xs)

	case class NextProve(task: Prove) extends Attention[Prove] with Promise{
		val subject = task
	}

	trait WillProve extends Attention[Prove] with Promise

	case class Consider(para: Paragraph) extends Attention[Paragraph]{
		val subject = para
		}

	type Transformation = PartialFunction[Para, Para]

  trait ProofSketch extends Paragraph
  
  case class Proof(p: Paragraph) extends ProofSketch
  
  trait CheckedProof extends ProofSketch{
    val hyp : Set[Formula]
    val concl : Set[Formula]
    def verify: Boolean
  }
  
  trait LogicProof extends CheckedProof
  
  case class ModusPoens(p: Formula, q: Formula) extends LogicProof{
    val hyp = Set(p, p implies q)
    val concl = Set(q)
    val verify = true
  }
  
  case class Gen(x: Var, p: Formula) extends LogicProof{
    val hyp = Set(p)
    val concl:Set[Formula] = Set(UnivQuantFormula(x, p))
    val verify = true
  }
  
  
  trait noFreeVars{
    val freeVars: Set[Var] =Set()
  }
  

  
  def atoms(p: Formula): Set[Formula] = p match {
    case NegFormula(p) => atoms(p)
    case ConjFormula(p, conj, q) => atoms(p) union atoms(q)
    case ExQuantFormula(x, p) => atoms(p)
    case UnivQuantFormula(x, p) => atoms(p)
    case p: Formula => Set(p)
  }
  
  def isLogicPoly(p: Formula) = (atoms(p) map (_.isInstanceOf[FormulaVar])) reduce (_ && _)

  def isTautology(p: Formula) = isLogicPoly(p) && {
    (allMaps(atoms(p), Set(true, false)) map (recValue(p, _))) reduce (_ && _)
  }
  
  def isTautology(p: Schema) = isLogicPoly(p) && (p.params.toSet == atoms(p.formula)) && {
    (allMaps(atoms(p.formula), Set(true, false)) map (recValue(p.formula, _))) reduce (_ && _)
  }
  
  case class Tautology(f: Schema, ps: Formula*) extends LogicProof{
    val hyp: Set[Formula] = Set.empty
    val fmla =f(ps.toList)
    val concl = Set(fmla)
    lazy val verify = isTautology(f)
  }
}

