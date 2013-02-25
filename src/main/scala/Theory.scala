package provingGround

import provingGround.Logic._
import provingGround.Structures._

/** The Meta-logical layer, including Definitions, Propositions, Proofs etc. */
object Theory{
  /** Paragraph: Any scope level */
  trait Paragraph
  
  /** A label for something */
  trait Label[A]{
    val ref: A
    }

  /** Wrap ref in a label */
  case class Ref[A](ref: A) extends Label[A]    
  
  /** Axiom */
  trait Axiom extends Paragraph{
    val axiom: Formula
    }
    
  /** Data: A collection of terms and an axiom they are supposed to satisfy */  
  trait Data extends Axiom{
    /** Terms in the data */
    val terms : List[Term]
    /** The axiom they satisy : axioms can be joined with & */
    val axiom : Formula
    
    /** Such that : Add an axiom */
    def suchthat(cond: Formula) = DataCond(this, cond)
    /** Such that: Add an axiom */
    def st(cond: Formula) = suchthat(cond)
    
    /** Add a data term */
    def and(that: Data) = given(this, that)
    
    /** Add a data term */
    def &(that: Data) = and(that)
  }
  
  /** A single term as data */
  case class DataTerm(term: Term, axiom: Formula = True) extends Data{
     val terms = List(term)
  }
  
  /** Convert term to data */
  implicit def dataTerm(term: Term): Data = DataTerm(term)
  
  /** A list of terms as data */
  case class DataList(lt: List[Data]) extends Data{
    val terms = lt flatMap (_.terms)
    val axiom = True
  }
  
  /** Add a condition to the data */
  case class DataCond(data: Data, cond: Formula) extends Data{
    val terms = data.terms
    val axiom = data.axiom & cond
  }
  
  /** Constructs a DataList from several data parameters */ 
  def given(ds: Data*) = DataList(ds.toList)
  
  /** A claim */
  trait Claim extends Paragraph{val claim: Formula}

  /** Property: Given data gives a formula corresponding to satisfying this */
  trait Property{
    def apply(d: Data): Formula
    
    def ::(d: Data)= DataCond(d, apply(d))
  }
  
  trait Justification extends Paragraph
  
  trait Method
  
  case class By(meth: Method) extends Justification
  
  case class Using(result: Claim) extends Justification
  
  trait Because{
    val because: List[Justification]
  }
    
  trait Assertion extends Claim with Because{
    def by(meth: Method) = Assert(claim, because :+ By(meth))
    def using(result: Claim) = Assert(claim, because :+ Using(result))
  }
  
  case class Assert(claim: Formula, because: List[Justification] = List()) extends Assertion
  
  class Proposition(val data: Data, val hypothesis: Formula, val conclusion: Formula) extends Claim{
    val claim = (data.axiom & hypothesis) implies conclusion
    }

  trait ProofSketch extends Paragraph
  
  trait Proof extends ProofSketch{
    val hyp : Set[Formula]
    val concl : Set[Formula]
    def verify: Boolean
  }
  
  trait LogicProof extends Proof
  
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
  
  case class Tautology(f: Schema, ps: Formula*) extends Proof{
    val hyp: Set[Formula] = Set.empty
    val fmla =f(ps.toList)
    val concl = Set(fmla)
    lazy val verify = isTautology(f)
  }
}

object ZF{
  import Theory._

  trait AbsSet extends Const{
    def contains(that: AbsSet): Formula = BinRel("in")(this, that)
    def <::(that: AbsSet): Formula = BinRel("in")(that, this)
    }
  
  val belongs = BinRel("in")

  case class FiniteUnion(sets: List[AbsSet]) extends AbsSet{
//    val freeVars: Set[Var] = Set.empty
    def this(ss: AbsSet*) = this(ss.toList)
//    def subs(xt: Var=> Term): Term = this
  }
  
  case class SetUnion(sets: AbsSet) extends AbsSet
  
  case class FiniteProd(sets: List[AbsSet]) extends AbsSet{
    def this(ss: AbsSet*) = this(ss.toList)
    def this(s: AbsSet, n:Int) = this(((1 to n) map (_ => s)).toList)
    }
    
  case class PowerSet(s: AbsSet) extends AbsSet
  
  case class RestSet(s: AbsSet, p: Formula) extends AbsSet
  
  case class FiniteSet(ss: AbsSet*) extends AbsSet
  
  case class IntSet(ss: Stream[Int]) extends AbsSet
  
  val NatNums = IntSet(Stream.from (1))
  
  abstract class AbsFunc extends AbsSet{
    val domain: AbsSet
    val codom: AbsSet
    }
  
  abstract class AbsBinRel extends AbsSet{
    val domain: AbsSet
    }    
    
  case class AbsFuncSym(name: String, domain: AbsSet, codom: AbsSet) extends AbsFunc

  case class AbsBinRelSym(name: String, domain: AbsSet) extends AbsBinRel
}