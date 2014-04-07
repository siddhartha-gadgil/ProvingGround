package provingGround

import provingGround.FreeGroups._
import provingGround.RandomWords._
import provingGround.Collections._
import annotation._

object AndrewsCurtis{
  
  /*
   * The vertices (e.g. presentations) and edgetype/movetype for the dynamics. We assume there are no combinations here.
   * Other generic objects also extend this.
   */
  type DynObj = ACobject
  
  /*
   * Empty presentation
   */
  val nullpres = Presentation(List(), 0)
  
  /*
   * Distribution on dynobjects - this is what evolves during learning.
   */
  type DynDist = FiniteDistribution[DynObj]
  
  val isPresentation : ACobject => Boolean = {
    case pres : Presentation => true
    case _ => false
  }
  
  val WtdPresentation : PartialFunction[Weighted[DynObj], Weighted[Presentation]] = {
    case Weighted(pres : Presentation, wt) => Weighted(pres, wt)
  }
  
  def FDpresentation(d : DynDist) : FiniteDistribution[Presentation] = {
     val rawpmf = d.pmf collect WtdPresentation
     FiniteDistribution(rawpmf).normalized
  }
  
  
  /*
   * Extend a path by another move. Need to do this differently while abstracting.
   */
  case object PathContinue extends DynObj
  
  
  /*
   * The type of moves
   */
  type MoveType = ACMoveType
  
  
  /*
   * Andrews-Curtis moves
   */
  trait ACMoveType extends ACobject
  
  case object ACStabMv extends ACMoveType
  case object ACDeStabMv extends ACMoveType
  case object RtMultMv extends ACMoveType
  case object LftMultMv extends ACMoveType
  case object ConjMv extends ACMoveType
  case object InvMv extends ACMoveType
  
  val MoveTypeList = List(ACStabMv, ACDeStabMv, RtMultMv, LftMultMv, ConjMv, InvMv)
  
  /*
   * Multiplicity of moves of a given type given a presentation. In general this should be a function of the presentation.
   */
  def multiplicity(rk: Int) : MoveType => Long = {
    case ACStabMv => 1 : Long
    case ACDeStabMv => (1 : Long) // can be zero in practise
    case RtMultMv => (rk : Long) * (rk : Long)
    case LftMultMv => (rk : Long) * (rk : Long)
    case ConjMv => (rk : Long) * (rk : Long)
    case InvMv => (rk : Long)
  }
  
  def allMoves(pres : Presentation): MoveType => List[Move] = {
    case ACStabMv => List(ACStab)
    case ACDeStabMv => 
      if (pres.ACstabilized) List(ACDeStab) else List()
    case RtMultMv => 
      val n = pres.rank
      (for (j <-0 to n-1; k <- 0 to n-1 if j != k) yield RtMult(j, k)).toList
    case LftMultMv => 
      val n = pres.rank
      (for (j <-0 to n-1; k <- 0 to n-1 if j != k) yield LftMult(j, k)).toList
    case ConjMv => 
      val n = pres.rank
      (for (j <-0 to n-1; k <- 0 to n-1 if j != k) yield Conj(j, k)).toList
    case InvMv => 
      val n = pres.rank
      (for (j <-0 to n-1) yield Inv(j)).toList
  }
  
  
  
  trait Move extends (Presentation => Presentation){
    val mvType : MoveType
  }
  
  case object ACStab extends Move{
    def apply(p: Presentation) = p.ACstab
    
    val mvType = ACStabMv
  }
  
  case object ACDeStab extends Move{
    def apply(p: Presentation) = p.ACdestab
    
    val mvType = ACStabMv
  }
  
  case class RtMult(k : Int, l : Int) extends Move{
    def apply(p: Presentation) = p.rtmult(k, l)
    
    val mvType = ACStabMv
  }
  
  case class LftMult(k : Int, l : Int) extends Move{
    val mvType = ACStabMv
    
    def apply(p: Presentation) = p.lftmult(k, l)
  }
  
  case class Conj(k : Int, l : Int) extends Move{
    def apply(p: Presentation) = p.conj(k, l)
    
    val mvType = ACStabMv
  }
  
  case class Inv(k : Int) extends Move{
    def apply(p: Presentation) = p.inv(k)
    
    val mvType = ACStabMv
  }
  
  trait Chain{
    val head: Presentation
    
    def prob(d: DynDist) : Double
    
    val foot : Presentation
    
    val moveStack : List[Move]
  }
  
  case class AtomicChain(head : Presentation) extends Chain{
    // choose the head, then don't continue
    def prob(d: DynDist) = d(head) * (1 - d(PathContinue))
    
    val foot = head
    
    lazy val moveStack = List() 
  }
  
  case class RecChain(start: Chain, move: Move) extends Chain{
    val head = move(start.head)
    
    lazy val foot = start.foot
    
    lazy val moveStack = move :: start.moveStack
    /*
     * start.prob = probability of reaching start and not continuing.
     * multiply by the probability of continuing from start and making the given move.
     * The latter involves diving by the multiplicity of the move type.
     * We need to multiply by the probability of not continuing, but this is part of start.prob already.
     */
    def prob(d: DynDist) = start.prob(d) * d(move.mvType) * d(PathContinue) / multiplicity(head.rank)(move.mvType) 
  }
  
  object Chain{
      @annotation.tailrec def addMoves(start : Chain, moveStack : List[Move]) : Chain = moveStack match {
        case List() => start
        case x :: xs => addMoves(RecChain(start, x), xs)
      }
      
      /*
       * Subchains with the same head
       */
      @tailrec def subchains(chain: Chain, tail : List[Move] = List(), accum : Set[Chain] =Set()) : Set[Chain] =  chain match {
        case chn @ AtomicChain(head) => accum + addMoves(chn, tail)
        case chn @ RecChain(start, move) => subchains(start, tail :+ move, accum + addMoves(chn, tail))
      }
      
      def apply(foot: Presentation, moveStack : List[Move]) = addMoves(AtomicChain(foot), moveStack)
    
	  def offspring(chain : Chain) = for (mvTyp <- MoveTypeList; mv <- allMoves(chain.head)(mvTyp)) yield RecChain(chain, mv)
	  
	  def fullnextgen(chains: Set[Chain]) = (chains flatMap (offspring(_))) ++ chains
	  
	  def prunednextgen(chains : Set[Chain], prune : Chain => Boolean) = (chains filter (prune(_)) flatMap (offspring(_))) ++ chains
      
	  /*
	   * Back-propagating a weight on a chain recursively, but not its subchains with the same head to avoid multiple counting 
	   * mult : the weight (multiplier)
	   * accum : accumulater, initially empty
	   * This should be used with care because of overlapping chains, so multiple count.
	   * In particular, we backprop multiplying only by start.prob(d), and not the cumulative.
	   */
	  @annotation.tailrec def backprop(chain: Chain, mult : Double, d : DynDist, accum : DynDist) : DynDist = chain match {
	    case AtomicChain(head) => 
	      accum + (head, mult * (1 - d(PathContinue))) + (PathContinue, -mult * d(head))
	    case chn @ RecChain(start, move) =>
	      // Number of moves of the given move type for the presentation.
	      val multplcty = multiplicity(chain.head.rank)(move.mvType)
	      val newmult = mult * d(move.mvType) * d(PathContinue) / multplcty // to be passed on to the previous chain
	      val mvTypWeight = start.prob(d) *  d(PathContinue) / multplcty
	      val acContWt = start.prob(d) * d(move.mvType) / multplcty
	      val newaccum = accum + 
	      					(move.mvType, mvTypWeight) + 
	      					(PathContinue, acContWt) 
	      backprop(start, newmult, d, newaccum)
	  }
  }
  
  def dstbn(chains : Seq[Chain], d : DynDist) = {
    val fdchains = FiniteDistribution(for (chn <- chains) yield Weighted(chn, chn.prob(d)))
    fdchains map ((chn : Chain) => chn.head)
  }
  
  def backpropdstbn(chains: Set[Chain], feedback : FiniteDistribution[Presentation], d : DynDist) = {
    val empty = FiniteDistribution[DynObj](Seq())
    val bcklist = chains map ((chn) => Chain.backprop(chn, feedback(chn.head), d, empty))
    (bcklist :\ empty)(_ ++ _)
  }
  
  def subchains(chain: Chain , accum: Set[Chain] = Set()) : Set[Chain] = chain match {
    case chn : AtomicChain => Set(chn)
  }
  
  
  def ChnFeedback(chains: Set[Chain], d : DynDist, presCntn : Double, wrdCntn : Double) = {
    val dstbnpmf = dstbn(chains.toSeq, d).pmf
    val rawpmf = for (Weighted(pres, prob) <- dstbnpmf) yield (Weighted(pres,presentationWeight(pres, presCntn, wrdCntn) / prob))
    val pmftot = (rawpmf map (_.weight)).sum
    val pmf = rawpmf map ((x) => Weighted(x.elem, x.weight - (pmftot/rawpmf.length)))
    FiniteDistribution(pmf)
  }
  
  def dstbnflow(chains: Set[Chain], d : DynDist, presCntn : Double, wrdCntn : Double , epsilon: Double) = {
    val feedback = ChnFeedback(chains: Set[Chain], d : DynDist, presCntn : Double, wrdCntn : Double) * epsilon
    val shift : DynDist = backpropdstbn(chains, feedback, d)
    (d ++ shift).normalized
  }
  
  @annotation.tailrec def chainEvolvCutoff(chains: Set[Chain], d : DynDist , cutoff: Double) : Set[Chain] ={
    val prune: Chain => Boolean = (chain) => chain.prob(d) > cutoff
    val nextgen = Chain.prunednextgen(chains, prune)
    if  (chains == nextgen) chains else chainEvolvCutoff(nextgen, d, cutoff)
  }
  
  /*
   * Presentations in the support of the evolving distribution
   */
  def presSupp(d : DynDist) = (d.pmf collect (WtdPresentation) map (_.elem)).toSet
  
  def initChains(d : DynDist) : Set[Chain] = presSupp(d) map (AtomicChain(_))  
  
  /*
   * Next step of the evolution of the distribution, assuming selection of sequences based on cutoff
   *
   */
  def dstbnFlowCutoff(d : DynDist, presCntn : Double, wrdCntn : Double , epsilon: Double, cutoff : Double) : DynDist = {
    val chainSet = chainEvolvCutoff(initChains(d), d, cutoff) flatMap (subchains(_)) 
    dstbnflow(chainSet, d, presCntn, wrdCntn, epsilon)
  }
}