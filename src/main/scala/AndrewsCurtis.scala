package provingGround

import provingGround.FreeGroups._
import provingGround.Collections._
import annotation._

// Play Json imports
import play.api.libs.json._

import play.api.Play.current

import play.api.libs.iteratee._
import play.api.libs.EventSource

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
  type DynDstbn = FiniteDistribution[DynObj]
  
  type Vert = Presentation
  
  
  val isPresentation : ACobject => Boolean = {
    case pres : Presentation => true
    case _ => false
  }
  
  val WtdPresentation : PartialFunction[Weighted[DynObj], Weighted[Presentation]] = {
    case Weighted(pres : Presentation, wt) => Weighted(pres, wt)
  }
  
  def FDVert(d : DynDstbn) : FiniteDistribution[Vert] = {
     val rawpmf = d.pmf collect WtdPresentation
     FiniteDistribution(rawpmf).normalized()
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
    case RtMultMv => (rk : Long) * ((rk : Long) - 1)
    case LftMultMv => (rk : Long) * ((rk : Long) - 1)
    case ConjMv => (rk : Long) * ((rk : Long) - 1)
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
  
  
  
  trait Move extends (Vert => Vert){
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

  /*
   * A chain of moves starting at foot and ending at head.
   */  
  trait Chain{

    val head: Vert
    
    def prob(d: DynDstbn) : Double
    
    val foot : Vert
    
    val moveStack : List[Move]
  }
  
  case class AtomicChain(head : Vert) extends Chain{
    // choose the head, then don't continue
    def prob(d: DynDstbn) = d(head) * (1 - d(PathContinue))
    
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
    def prob(d: DynDstbn) = start.prob(d) * d(move.mvType) * d(PathContinue) / multiplicity(head.rank)(move.mvType) 
  }
  
  object Chain{
      @tailrec def addMoves(start : Chain, moveStack : List[Move]) : Chain = moveStack match {
        case List() => start
        case xs => addMoves(RecChain(start, xs.last), xs.dropRight(1))
      }
      
      /*
       * Subchains with the same head. Recursively defined with tail being moves to be appended to each chain
       */
      @tailrec def subchains(chain: Chain, tail : List[Move] = List(), accum : Set[Chain] =Set()) : Set[Chain] =  chain match {
        case AtomicChain(head) => accum + addMoves(chain, tail)
        case RecChain(start, move) => subchains(start, tail :+ move, accum + addMoves(chain, tail))
      }
      
      def apply(foot: Vert, moveStack : List[Move]) = addMoves(AtomicChain(foot), moveStack)
    
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
	  @annotation.tailrec def backprop(chain: Chain, mult : Double, d : DynDstbn, accum : DynDstbn) : DynDstbn = chain match {
	    case AtomicChain(head) => 
	      accum + (head, mult * (1 - d(PathContinue))) + (PathContinue, -mult * d(head))
	    case chn @ RecChain(start, move) =>
	      // Number of moves of the given move type for the presentation.
	      val multplcty = multiplicity(chain.head.rank)(move.mvType)
	      val newmult = mult * d(move.mvType) * d(PathContinue) / multplcty // to be passed on to the previous chain
	      val mvTypWeight = start.prob(d) *  d(PathContinue) / multplcty
	      val cntnWt = start.prob(d) * d(move.mvType) / multplcty
	      val newaccum = accum + 
	      					(move.mvType, mult * mvTypWeight) + 
	      					(PathContinue, mult * cntnWt) 
	      backprop(start, newmult, d, newaccum)
	  }
  }
  
  /*
  * The distribution on presentations via head of chains induced by that on presentations, moves and continuation.
  */ 
  def dstbn(chains : Seq[Chain], d : DynDstbn) = {
    val fdchains = FiniteDistribution(for (chn <- chains) yield Weighted(chn, chn.prob(d)))
    (fdchains map ((chn : Chain) => chn.head)).flatten
  }
  

  /*
  * Back-propagate a feedback on distribution on presentations.
  */
  def backpropdstbn(chains: Set[Chain], feedback : FiniteDistribution[Vert], d : DynDstbn) = {
    val empty = FiniteDistribution[DynObj](Seq())
    val bcklist = chains map ((chn) => Chain.backprop(chn, feedback(chn.head), d, empty))
    ((bcklist :\ empty)(_ ++ _)).flatten
  }
  
  
  /*
   * The background weights in the Andrews-Curtis case
   */
  
  def ACbgkWt(presCntn : Double, wrdCntn : Double) : Vert => Double = presentationWeight(_, presCntn, wrdCntn)
  
  /*
   * Feedback for distribution on presentations, comparing probabilities from generating presentations by words
   * with generating from Andrews-Curtis moves. This has total zero. 
   * A high (raw) feedback is for a simple distribution needing a lot of moves. 
   */
  def dstbnFeedback(presdstbn : FiniteDistribution[Vert] , bgwt : Vert => Double) = {
    val dstbnpmf = presdstbn.pmf
    val fdbkrawpmf = for (Weighted(pres, prob) <- dstbnpmf) yield (Weighted(pres,bgwt(pres) / prob))
    val fdbkpmftot = (fdbkrawpmf map (_.weight)).sum
    val fdbkpmf = fdbkrawpmf map ((x) => Weighted(x.elem, x.weight - (fdbkpmftot/fdbkrawpmf.length)))
    FiniteDistribution(fdbkpmf)
  }
  
  /*
   * Change the initial distribution based on feedback, possibly pruning at threshold. This is the learning step.
   */
  def dstbnflow(chains: Set[Chain], d : DynDstbn, bgwt : Vert => Double , epsilon: Double, threshold: Double = 0) = {
    val presdstbn = dstbn(chains.toSeq, d)
    val feedback = dstbnFeedback(presdstbn, bgwt) * epsilon
    val shift : DynDstbn = backpropdstbn(chains, feedback, d)
    (d ++ shift).normalized(threshold)
  }
  
  /*
   * Generate chains based on a cutoff (which stops descendants).
   */
  @annotation.tailrec def chainGenCutoff(chains: Set[Chain], d : DynDstbn , cutoff: Double) : Set[Chain] ={
    val prune: Chain => Boolean = (chain) => chain.prob(d) > cutoff
    val nextgen = Chain.prunednextgen(chains, prune)
    if  (chains == nextgen) chains else chainGenCutoff(nextgen, d, cutoff)
  }
  
  /*
   * Presentations in the support of the evolving distribution.
   */
  def presSupp(d : DynDstbn) = (d.pmf collect (WtdPresentation) map (_.elem)).toSet
  
  /*
   * The initial chains, based on the dynamic distribution, from which chains are generated.
   * This is also useful for reporting interesting presentations.
   */
  def initChains(d : DynDstbn) : Set[Chain] = presSupp(d) map (AtomicChain(_))  
  
  /*
   * Next step of the evolution of the distribution, assuming selection of sequences based on cutoff
   *
   */
  def dstbnFlowCutoff(d : DynDstbn, bgwt : Vert => Double , epsilon: Double, cutoff : Double) : DynDstbn = {
    val chainSet = chainGenCutoff(initChains(d), d, cutoff) flatMap (Chain.subchains(_)) 
    dstbnflow(chainSet, d, bgwt, epsilon)
  }
  
  /*
   * flow for tuning, without adding vertices to the support. This is accomplished by not propogating on subchains
   */  
  def tuneFlowCutOff(d : DynDstbn, bgwt : Vert => Double , epsilon: Double, cutoff : Double) ={
    dstbnflow(chainGenCutoff(initChains(d), d, cutoff), d, bgwt, epsilon)
  }
  
  /*
   * The best chain for a presentation. We record these (perhaps in MongoDb) while forgetting other chains.
   */
  def bestChain(pres: Vert , chains: Set[Chain], d : DynDstbn) = chains filter (_.head == pres) maxBy (_.prob(d))
  
  def wtdPresJson(pw : Weighted[Presentation]) = {
      val listlist = pw.elem.rels map (_.ls)
      Json.obj("rank" -> pw.elem.rank, "words" -> listlist, "prob" -> pw.weight)
    }
  
  def dstbnJson(dstbn : FiniteDistribution[Presentation]) = Json.toJson(dstbn.pmf.toList map (wtdPresJson(_))) 
  
  def sendDstbn(dstbn : FiniteDistribution[Presentation]) = dstbnChannel.push(dstbnJson(dstbn))
  
  /*
   * sending out distributions on presentations to frontend : the dstbnout Enumerator is sent in response to an sse request
   * 
   */
  val (dstbnout, dstbnChannel) = Concurrent.broadcast[JsValue]

// Short Loop: Flow for a while, purge and report survivors
// Long loop : Repeat short loop
}
