package provingGround

import provingGround.FreeGroups._
import provingGround.RandomWords._
import provingGround.Collections._

object AndrewsCurtis{
  
  type ACD = FiniteDistribution[ACobject]
  
  val isPresentation : ACobject => Boolean = {
    case pres : Presentation => true
    case _ => false
  }
  
  val WtdPresentation : PartialFunction[Weighted[ACobject], Weighted[Presentation]] = {
    case Weighted(pres : Presentation, wt) => Weighted(pres, wt)
  }
  
  def FDpresentation(d : ACD) : FiniteDistribution[Presentation] = {
     val rawpmf = d.pmf collect WtdPresentation
     FiniteDistribution(rawpmf).normalized
  }
  
  case object ACcontinue extends ACobject
  
  trait MoveType extends ACobject
  
  case object ACStabMv extends MoveType
  case object ACDeStabMv extends MoveType
  case object RtMultMv extends MoveType
  case object LftMultMv extends MoveType
  case object ConjMv extends MoveType
  case object InvMv extends MoveType
  
  val MoveTypeList = List(ACStabMv, ACDeStabMv, RtMultMv, LftMultMv, ConjMv, InvMv)
  
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
      (for (j <-0 to n-1; k <- 0 to n-1) yield RtMult(j, k)).toList
    case LftMultMv => 
      val n = pres.rank
      (for (j <-0 to n-1; k <- 0 to n-1) yield LftMult(j, k)).toList
    case ConjMv => 
      val n = pres.rank
      (for (j <-0 to n-1; k <- 0 to n-1) yield Conj(j, k)).toList
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
  
  trait ACchain{
    val head: Presentation
    
    def prob(d: ACD) : Double
  }
  
  case class ACstart(head : Presentation) extends ACchain{
    def prob(d: ACD) = d(head) * (1 - d(ACcontinue))
  }
  
  case class ACextend(start: ACchain, move: Move) extends ACchain{
    val head = move(start.head)
    
    def prob(d: ACD) = start.prob(d) * d(move.mvType) * d(ACcontinue) / multiplicity(head.rank)(move.mvType) 
  }
  
  object ACchain{
	  def offspring(chain : ACchain) = for (mvTyp <- MoveTypeList; mv <- allMoves(chain.head)(mvTyp)) yield ACextend(chain, mv)
	  
	  def fullnextgen(chains: Seq[ACchain]) = (chains flatMap (offspring(_))) ++ chains
	  
	  def prunednextgen(chains : Seq[ACchain], prune : ACchain => Boolean) = (chains filter (prune(_)) flatMap (offspring(_))) ++ chains
      
	  
	  @annotation.tailrec def backprop(chain: ACchain, mult : Double, d : ACD, accum : ACD) : ACD = chain match {
	    case ACstart(head) => 
	      accum + (head, mult * (1 - d(ACcontinue))) + (ACcontinue, -mult * d(head))
	    case ACextend(start, move) =>
	      val newmult = mult * d(move.mvType) * d(ACcontinue) / (multiplicity(chain.head.rank)(move.mvType))
	      val mvTypWeight = start.prob(d) *  d(ACcontinue) / (multiplicity(chain.head.rank)(move.mvType))
	      val acContWt = start.prob(d) * d(move.mvType) / (multiplicity(chain.head.rank)(move.mvType))
	      val newaccum = accum + (move.mvType, mvTypWeight) + (ACcontinue, acContWt)
	      backprop(start, newmult, d, newaccum)
	  }
  }
  
  def dstbn(chains : Seq[ACchain], d : ACD) = {
    val fdchains = FiniteDistribution(for (chn <- chains) yield Weighted(chn, chn.prob(d)))
    fdchains map ((chn : ACchain) => chn.head)
  }
  
  def backpropdstbn(chains: Seq[ACchain], feedback : FiniteDistribution[Presentation], d : ACD) = {
    val empty = FiniteDistribution[ACobject](Seq())
    val bcklist = chains map ((chn) => ACchain.backprop(chn, feedback(chn.head), d, empty))
    (bcklist :\ empty)(_ ++ _)
  }
  
  
  
  def ACfeedback(chains: Seq[ACchain], d : ACD, presCntn : Double, wrdCntn : Double) = {
    val dstbnpmf = dstbn(chains, d).pmf
    val rawpmf = for (Weighted(pres, prob) <- dstbnpmf) yield (Weighted(pres,presentationWeight(pres, presCntn, wrdCntn) / prob))
    val pmftot = (rawpmf map (_.weight)).sum
    val pmf = rawpmf map ((x) => Weighted(x.elem, x.weight - (pmftot/rawpmf.length)))
    FiniteDistribution(pmf)
  }
  
  def distflow(chains: Seq[ACchain], d : ACD, presCntn : Double, wrdCntn : Double , epsilon: Double) = {
    val feedback = ACfeedback(chains: Seq[ACchain], d : ACD, presCntn : Double, wrdCntn : Double) * epsilon
    val shift : ACD = backpropdstbn(chains, feedback, d)
    (d ++ shift).normalized
  }
  
  @annotation.tailrec def chainEvolvCutoff(chains: Seq[ACchain], d : ACD , cutoff: Double) : Seq[ACchain] ={
    val prune: ACchain => Boolean = (chain) => chain.prob(d) > cutoff
    val nextgen = ACchain.prunednextgen(chains, prune)
    if  (chains.toSet == nextgen.toSet) chains else chainEvolvCutoff(nextgen, d, cutoff)
  }
  
  def initChains(d : ACD) = {
    d.pmf collect (WtdPresentation) map (_.elem) map (ACstart(_))
  }
  
  def distFlowCutoff(d : ACD, presCntn : Double, wrdCntn : Double , epsilon: Double, cutoff : Double) : ACD = {
     distflow(initChains(d), d, presCntn, wrdCntn, epsilon)
  }
}