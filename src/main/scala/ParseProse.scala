package provingGround

import provingGround.NlpProse._
import provingGround.Logic._
import provingGround.TextToInt._
import provingGround.Theory._
// import provingGround.Arithmetic._
// import provingGround.Collections._

/** Translates Stanford Dependency tree to Logical expression */                               
object ParseProse{

/** Factory for variable symbols */  
trait Variables{
    
    private var usedVars : Set[Var] = Set()
  
    def newVar : Var = {
    	val newvar = (varstream filterNot (usedVars contains _)).head
    	usedVars += newvar
    	newvar
    	}
    	
    def newVars(n: Int) : List[Var] = (for(i<- 1 to n) yield (newVar)).toList
    
    }
    
/** The scope of a formula */    
trait Scope extends Variables

/** The default scope */
object Global extends Scope
    	
// The interpreter
/** Print tree for unparsed prose */
trait ProsePrint{
   val t: ProseTree
   override def toString = t.toString
}

val unParsed: PartialFunction[Formula, ProseTree] = {
   case p: ProsePrint => p.t
}

/** Makes unparsed prose function, predicate etc to unparsed formula */
trait MapProseFormula[A]{
  val t: ProseTree  
  def apply(d:A): Formula = ProseFormula(t)
}

/** Unparsed  formula */
case class ProseFormula(val t: ProseTree) extends Formula with ProsePrint

/** Unparsed Property */
case class ProsePropt(val t: ProseTree) extends Propt  with MapProseFormula[Var] with ProsePrint

/** Unparsed Condition */
class ProseCondition(val t: ProseTree) extends Condition  with MapProseFormula[Formula] with ProsePrint

/** Unparsed ConditionProperty */
case class ProseCondPropt(val t: ProseTree) extends CondPropt with ProsePrint{
  def apply(x: Var, p: Formula) = new ProseFormula(t)
}

/** Generic Extractor from ProseTree*/
case class ProseExtractor[A](fn: ProseTree => Option[A]){
  def unapply(t: ProseTree): Option[A] = fn(t)
  
  def this(fn:PartialFunction[ProseTree, A]) = this(fn.lift)
  } 

/** Extractor matching Dependency type */
case class TypeMatch(depType: String){
  def unapply(t: ProseTree): Option[(DepRel, ProseTree, ProseTree)] = t.findSplit(depType)
}

/** Extractor matching Dependency type and Word of Dependent Token*/
case class TypeWordMatch(depType: String, w: String){
  def unapply(t: ProseTree): Option[(DepRel, ProseTree, ProseTree)] = t.findSplit(depType, w)
}
  
/** Extractor for quantmod */
val QuantMod = TypeMatch("quantmod")
// use stringNumber()
/** Extractor for > */
val Gt = TypeWordMatch("quantmod", "greater than")

/** Extractor for < */
val Lt = TypeWordMatch("quantmod", "less than")

/** Extractor for "cop" relation */
val CopRel = TypeMatch("cop")

/** Extractor for nsubj */
val Nsubj = TypeMatch("nsubj")

/** Extractor for relative clause modifier */
val Rcmod = TypeMatch("rcmod")

private val copFn: PartialFunction[ProseTree, (ProseTree, ProseTree)] = {
  case CopRel(_,_,Nsubj(_,s,t)) => (s,t)
}

/** Extractor for "cop" identifying the subject */
val Cop = ProseExtractor(copFn.lift)

/** Extractor for "which" */
val Which=TypeWordMatch("nsubj", "which")

/** Extractor for Adverbial Clause */
object Advcl{
  def unapply(t: ProseTree) : Option[(ProseTree, ProseTree)]= {
    val advcls = t find "advcl"
    advcls match {
      case Some(y) => {
        val s = t -< y.dep
        Some((s, (t - s)))
        }
      case None => None
        }
      }
}      

/** Extractor for 'if' as determiner */      
object IfMark{
   def unapply(t:ProseTree) : Option[ProseTree] = {
   	val ifdet = t find ("mark", "if")
        ifdet match {
          case Some(x) => Some(t - x.dep)
          case None => None
          }
       }
   }

/** Is a vowel */
def isVowel(c:Char) = List('a','e','i','o','u') contains c

/** Has a vowel */
def hasVowel(s: String) = s.exists (isVowel _)

/** A crude criterion for being a name: has more than one letter and contains a vowel*/
def isName(s: String): Boolean = {
  if (s.length==1) false else{
    if (hasVowel(s)) true else false
    } 
  }

/** A simple property : word is taken as the name of the property */
object SimpPropt{
  def unapply(t:ProseTree) : Option[String]={
    if (t.heirs.length == (t findAll "conj").length) Some(t.root.word) else None
    }
  }

/** Extractor for determiner */
object Det{
  def unapply(t:ProseTree) : Option[(String, ProseTree)] ={
    val det = t find "det"
    det match {
      case None => Some(("",t))
      case Some(d) => Some(d.dep.word, t-d.dep)
        }
     }
  }

/** Extractor for (several) adjectival properties */
object Amods{
  def unapply(t:ProseTree) : Option[(ProseTree, List[ProseTree])] ={
    val amods = t findAll (List("amod", "nn"))
    amods match {
      case List() => None
      case _ => {
          val subtrees = amods map (node => t -<node.dep)
          Some(t -- subtrees, subtrees)
            }
      }
    }
  }

/** Extractor for a predicate and its parameters; Optionally returns predicate name and trees for parameters */ 
object PredParams{
  def unapply(t:ProseTree) : Option[(String, List[ProseTree])] ={
    val params = t findAll List("nsubj", "dobj", "prep")
    params match {
      case List() => None
      case _ => {
          val subtrees = params map (node => t -<node.dep)
          Some(t.root.word, subtrees)
            }
      }
    }
  }  
  
/** Extractor for `one of' */  
object OneOf{
  def unapply(t:ProseTree) : Option[List[ProseTree]] ={
    val preps = t findAll List("prep_of")
    preps match {
      case List() => None
      case _ => {
          val subtrees = preps map (node => t -<node.dep)
          Some(subtrees)
            }
      }
    }
  }
  
      
// This is broken into Advcl and IfMark   
@deprecated("Use Advcl followed by IfMark", "Replacement ready, to test") object If {
  def unapply(t: ProseTree) : Option[(ProseTree, ProseTree)]= {
    val advcls = t find "advcls"
    advcls match {
      case Some(y) => {
        val s = t -< y.dep
        val ifdet = s find ("det", "if")
        ifdet match {
          case Some(x) => Some((s - x.dep), (t - s))
          case None => None}
        }
      case None => None
        }
      }
  }

/** returns condition (Formula => Formula) from ProseTree */
def toCondition(d: ParseData, scope:Scope): Formula=> Formula = {
  d match {
    case IfMark(p) => {
      val ptrans: Formula = toFormula(p, scope)
      def impl(q:Formula) = ptrans implies q
      impl _
      }
    case t: ProseTree => new ProseCondition(t)
    }
    
  }
  
/** Optional Formula */
def optFormula(d: ParseData, scope: Scope): Option[Formula] = {
  val p = toFormula(d, scope)
  val unparsed = atoms(p) collect (unParsed)
  if (unparsed.isEmpty) Some(p) else {
    println(p)
    unparsed.foreach(println)
    None
  }
}

/** returns Formula from ProseTree */
def toFormula(d:ParseData, scope: Scope): Formula ={
  d match {
    case Advcl(p, q) => toCondition(p, scope)(toFormula(q,scope))
    case PredParams(p, params) =>{
        val n=params.length
        val xs=scope.newVars(n)
        val pred = PredSym(p, n)   
        val baseFormula = AtomFormula(pred, xs)
        val condPropts = params map (toCondPropt(_,scope))
        zipSubs(condPropts, xs, baseFormula)
        }
    case Cop(p, q) =>
    	val x = scope.newVar
    	ExQuantFormula(x, (toPropt(p, scope)(x)) & (toPropt(q, scope)(x)))
//    case If(p,q) => toFormula(p, scope) implies toFormula(q, scope)
    case t: ProseTree => new ProseFormula(t)
    }
}

/** Returns Property (Var => Formula) from ProseTree 
	* 
	* Note that a term is returned as Var = Term
*/ 
def toPropt(d:ParseData, scope: Scope): Var => Formula={
  d match {
    case SimpPropt(name) => {
        if (isName(name)) {
          val p = PredSym(name, 1)
          x:Var => p(x)
          }
         else {
          val t = new TermFmla(name)
          x: Var => t eqls x
          }
        }
     case OneOf(params) => 
        val propts = params map (toPropt(_,scope))
        def xval(x : Var): Formula = {
          val terms = propts map (p=>p(x))
          ((terms.head)/:(terms.tail))(or)
          }
        xval        
     case Amods(main, params) => 
     	(x => subs(toPropt(main, scope)(x), x, params map (toPropt(_, scope))))
     case Rcmod(_, s, t) => 
//			println("rcmod")
//			println(s)
//			println(t)
			(x: Var) => toPropt(s, scope)(x) & toPropt(t, scope)(x)
     case Which(_,_, t)=> 
//     	println("which")
//     	println(t)
     	(x: Var) => toPropt(t, scope)(x)
     case Cop(p, q) =>
     	println("cop")
    	(x: Var)=> (toPropt(p, scope)(x)) & (toPropt(q, scope)(x))
     case Gt(node, _, _) =>
//     	println("greater than")
//     	println(node.gov.word) 
     	(x => BinRel(">")(x, IntConst(stringNumber(node.gov.word))))
     case Lt(node, _, _) => 
     	(x => BinRel("<")(x, IntConst(stringNumber(node.gov.word))))
     case t: ProseTree => new ProsePropt(t)
     }
  }

/** returns Condition-Property (Var, Formula) => Formula from ProseTree */  
def toCondPropt(d:ParseData, scope: Scope): (Var, Formula) => Formula ={
  d match {
    case Det(s, t) => s match{
      case "" => ((x: Var, p: Formula) => toPropt(t, scope)(x) & p)
      case "a" => ((x: Var, p: Formula) => ExQuantFormula(x,toPropt(t, scope)(x) & p))
      case "every" => ((x: Var, p: Formula) => UnivQuantFormula(x,toPropt(t, scope)(x) implies p))      
      }
    case t: ProseTree => new ProseCondPropt(t)  
    }
  }

/** Optionally parses to Fmla(Formula) */ 
def optFmla(d: ParseData, scope: Scope): Option[Paragraph] = optFormula(d, scope) map (Fmla)
  
/** Optionally parses to an Assertion */
def optAssert(d: ParseData, scope: Scope): Option[Paragraph] = None

/** Parses to a paragraph, fallback to Text() */
def toPara(d: ParseData, scope: Scope): Paragraph = {
  val tryParse = (optFmla(d, scope) 
									orElse optAssert(d, scope)
								)
  tryParse.getOrElse(Text(d.toString))
}
}
