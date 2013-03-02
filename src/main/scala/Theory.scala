package provingGround

import provingGround.Logic._
import provingGround.Structures._

/** The Meta-logical layer, including Definitions, Propositions, Proofs etc. */
object Theory{
  
  trait Phrases
  
  /** Any block of full sentences*/
  trait Paragraph extends Phrases
  
  /** Recursive Paragraph */
  case class Para(phraseList: List[Phrases]) extends Paragraph
  
  def para(phrases: Phrases*) = Para(phrases.toList)
  
  
  /** Text not to be translated into mathematics */ 
  case class Text(text: String) extends Paragraph 
  
  /** Axiom */
  trait Axiom extends Phrases{
    val axiom: Formula
    
    def &(that: Formula) = Axioms(axiom, that)
    def &(that: Axiom) = Axioms(axiom, that.axiom) 
    
    def have(that: Formula): Formula = axiom implies that
    def have(that: Axiom): Formula = axiom implies (that.axiom)
    }
    
  case class Axioms(axioms: Formula*) extends Paragraph with Axiom{
    val axiomList = axioms.toList
    val axiom = axiomList reduce (_ & _)
    }
    
  val x = Var("X")
  val y = Var("y")
  val z = Var("z")
    
  def symmetric(r: BinRel): Axiom = Axioms(forAll(x, y)(r(x,y) implies r(y,x)))
    
  def transitive(r: BinRel): Axiom = Axioms(forAll(x, y, z)((r(x,y) & r(y,z)) implies r(x,z)))
  
  def reflexive(r: BinRel): Axiom = Axioms(forAll(x)(r(x,x)))
  
  def equivRelation(r: BinRel): Axiom = symmetric(r) & reflexive(r) & transitive(r)

    
  /** Data: A collection of terms and an axiom they are supposed to satisfy */  
  trait Data extends Term with Axiom{
    /** Substitute in terms but keep axiom with substitutions */
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
  }



  case class EquivRelation(rel: BinRel, generator: Axiom) extends Axiom{
    val axiom = (generator & equivRelation(rel)).axiom
    }
  
  case class QuotientData(d: Data, r: EquivRelation) extends Data{
    val freeVars = d.freeVars
    def subs(xt: Var => Term): Term = QuotientData(d.subsData(xt), r)
    val axiom = (d.axiom) & (r.axiom) 
    }
  
  case class Fix(d: Data) extends Paragraph
  
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
    def freeVars = data.freeVars
    val axiom = data.axiom & cond
  }
  
  /** Constructs a DataList from several data parameters */ 
  def given(ds: Data*) = DataList(ds.toList)
  
  /** A result, may be just a reference to one */
  trait Result extends Phrases
  
  case class ResultRef(name: String) extends Result
  
  /** A claim */
  trait Claim extends Paragraph with Result{val claim: Formula}
  
  case object Contradiction extends Claim{val claim: Formula = False}
  

  /** Property: Given data gives a formula corresponding to satisfying this */
  abstract class Property(deg: Int) extends Pred(deg) with Axiom{       
    def ::(d: Data)= DataCond(d, apply(d))
  }
  
  case class PropertyDefn(deg: Int, defn: Pred => Formula) extends Property(deg){
    val axiom = defn(this)
  }
  
  abstract class Mapping(deg: Int) extends Func(deg) with Axiom
  
  case class MapDefn(deg: Int, defn: Func => Formula) extends Mapping(deg){
    val axiom = defn(this)
  }
                         
  trait Justification extends Phrases{
    def &(that: Justification) = PolyJust(this, that)
    def and(that: Justification) = PolyJust(this, that)
  }
  
  case class PolyJust(first: Justification, second: Justification) extends Justification
  

  
  trait Method
  
  case class NamedMethod(name: String) extends Method
  
  case class By(meth: Method) extends Justification
  
  def by(meth: Method) = By(meth)
  
  def by(name: String) = By(NamedMethod(name))
  
  
  
  case class Using(result: Result) extends Justification
  
  def using(result: Result) = Using(result)
  
  def using(name: String) = Using(ResultRef(name))
  
  
  trait Condition extends Phrases{
    def apply(p: Formula): Formula
    
    def then(p: Formula) = apply(p)

    }

  case class If(q: Formula) extends Condition{
    def apply(p: Formula) = q implies p
    }    
    
  case class As(c: Condition) extends Justification
  
  def as(c: Condition) = As(c)
  
  def as(result: Result) = Using(result)
  
  trait Assumption extends Condition with Axiom{
    def apply(p: Formula) = axiom implies p
  }
  
  case class Assume(axiom: Formula) extends Assumption
      
  trait Consequence extends Axiom
  
  case class Then(axiom: Formula) extends Consequence
 
  def then(p: Formula) = Then(p)
  
  def have(p: Formula) = Then(p)
  
  trait Because{
    val because: List[Justification]
  }
    
  trait Assertion extends Claim with Because{
    def by(meth: Method) = Assert(claim, because :+ By(meth))
    def by(name: String) = Assert(claim, because :+ By(NamedMethod(name)))
    
    def using(result: Claim) = Assert(claim, because :+ Using(result))
    def using(name: String) = Assert(claim, because :+ Using(ResultRef(name)))
  }
  
  case class Assert(claim: Formula, because: List[Justification] = List()) extends Assertion
  
  trait Conclusion extends Assertion
  
  case class Conclude(claim: Formula, because: List[Justification] = List()) extends Conclusion
  
  case object QED extends Paragraph
    
  case object ThisPara extends Paragraph
  
  case class BeginProof(result: Paragraph= ThisPara) extends Paragraph 
  
  
  implicit def assert(p: Formula): Assertion = Assert(p)
  
  
  case object ByAbove extends Justification
  
  def thus(ass: Assertion) = Assert(ass.claim, ByAbove :: ass.because)
  
  def hence(ass: Assertion) = Assert(ass.claim, ByAbove :: ass.because)

  def therefore(ass: Assertion) = Assert(ass.claim, ByAbove :: ass.because)

  def deduce(ass: Assertion) = Assert(ass.claim, ByAbove :: ass.because)


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
  
  class FormulaVar extends Formula 
  
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

