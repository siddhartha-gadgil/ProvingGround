package provingGround 

/** Stanford dependency trees and associated methods */
object NlpProse {

/** The trait for all data to be parsed to mathematics */ 
trait ParseData

/**Tokens*/
case class Token(word:String, idx: Int){
  /** Merge tokens by merging words and taking the first position */
  def +(that: Token) = Token(word+" "+that.word, idx)
  
  override def toString = "(" + word + "," + idx.toString+")" 
  }
 
/** Stanford dependency relation */ 
case class DepRel(gov: Token, dep: Token, deptype: String){
  override def toString = deptype +"(" + gov.toString + ","+ dep.toString+")"
  }

/** Returns root of dependency tree */
def findroot(t : List[DepRel]): Token = {
  val roots = t filter (_.deptype == "root")
  if (roots.length >0) roots.head.gov else {
    val govs = t map (_.gov)
    val deps = t map (_.dep)
    def notdep(g : Token) = !(deps contains g)
    val top = govs filter (notdep _)
    top.head
    }
  }

/** Stanford Dependency tree */
case class ProseTree(root: Token, tree: List[DepRel]) extends ParseData {

  override def toString = root.toString+":"+tree.toString

  /** Dependence relations with governor a given token */
  def offspring(node: Token) : List[DepRel] = tree filter (_.gov == node)
  /** Dependence relations with governor the root */
  def heirs : List[DepRel] = offspring(root)  
  
  /** Initiates ProseTree from a list of Tokens by finding root*/
  def this(t: List[DepRel]) = this(findroot(t), t)
  
  /**List of dependency relations descending from an initial list */
  def desc(init: List[DepRel]): List[DepRel] = {
    if (init.isEmpty) List() else {
      val newnodes = init map (_.dep)
      val newedges = newnodes map offspring
      init ::: ((newedges map desc).flatten)
      }
  }
  
  /** Remove dependency relations with dependent a given toke */
  def -(node: Token) = ProseTree(root, (tree filter (_.dep != node)))
  
  /** Remove all edges contained in ProseTree s and with dependent in s */
  def -(s: ProseTree) ={
    def notintree(edge: DepRel) = (edge.dep !=s.root) && !(s.tree contains edge)
    ProseTree(root, (tree filter notintree))
  }
  
  /** Deletes in the sense of method {{-}} a list of trees*/
  def --(ss:List[ProseTree]): ProseTree = ss match {
    case s :: List() => this - s
    case s :: sss => (this-s) -- sss
    case Nil => this
    }
  
  /** The tree of all descendants of a node */
  def -<(node: Token)= ProseTree(node, desc(offspring(node)))
  
  /** Splits into the tree of descendants and the rest */
  def split(r: DepRel) = (r, -<(r.dep), this-(-<(r.dep)))
  
  private def depstart(r : DepRel, s: String) = r.deptype.startsWith(s)
  
  private def depstartsome(r : DepRel, ss: List[String]) = (ss map (r.deptype.startsWith(_))) reduce (_ || _)
  
  /** Find Dependency Relations in heirs of (more generally starting with) a given dependency type  */
  def find(typ: String) = heirs find (depstart(_, typ))
  
  /** Find Dependency Relations in heirs specifying both type and dependent word*/
  def find(typ: String, word: String): Option[DepRel] = { 
    def depmatch(d:DepRel):Boolean = (depstart(d, typ)) && ((d.dep.word).toLowerCase == word)
    heirs find depmatch
   } 
  
  /** Find Dependency Relations in heirs for type and split tree accordingly if found */
  def findSplit(typ:String) = find(typ:String) map (split)
  
  /** Find Dependency Relations in heirs for type and word and split tree accordingly if found */
  def findSplit(typ:String, word: String) = find(typ:String, word: String) map (split)
  
  /** Find all heirs with dependency one of the given types */
  def findAll(typs: List[String]): List[DepRel] = heirs filter (depstartsome(_,typs))
 
  /** Find all heirs with dependency the given type */
  def findAll(typ: String): List[DepRel] = heirs filter (depstart(_,typ))
 
// Not clear what this method does  
//  def intyps(e: DepRel) = {tree exists ((e.deptype).startsWith(_))    
//    heirs filter intyps 
//    }
}

/** Find the string of multi-word expressions starting at a token */
def mweTail(t: ProseTree, node: Token): Token = {
  val nxt = t.offspring(node) find (_.deptype == "mwe")
  nxt match {
    case None => node
    case Some(x) => mweTail(t, x.dep) + node
  }
}

/** returns Dependency Relation obtained by merging in multi-word expressions */ 
def mweMerge(t: ProseTree, d: DepRel) = DepRel(mweTail(t, d.gov), mweTail(t, d.dep), d.deptype)

/** returns new tree folding together multi-word expressions */
def mweFold(t: ProseTree) = {
  val foldList = (t.tree map (mweMerge(t,_))) filter (_.deptype != "mwe")
  ProseTree(mweTail(t, t.root), foldList)
}
}