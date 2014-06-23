package provingGround

import provingGround.FreeGroups._
import provingGround.Collections._
import annotation._

class MoveLearner[V, M <: (V => V)](movetypes: List[M], moves : (V, M) => Set[V]){
  
  def multiplicity(vert: V, movetype: M) = moves(vert, movetype).size
  
  case class DynDst(vrtdst: FiniteDistribution[V], edgdst : FiniteDistribution[M], cntn : Double){
    lazy val  norm = vrtdst.norm + edgdst.norm + cntn.abs
    
    def probV (v : V) = vrtdst(v)
    
    def probM (e : M) = edgdst(e)
    
    def ++ (that : DynDst) = DynDst(vrtdst ++ that.vrtdst, edgdst ++ that.edgdst, cntn + that.cntn)
    
    def addV (v : V, p : Double) = DynDst(vrtdst + (v, p), edgdst, cntn)
    
    def addM ( e : M, p : Double) = DynDst(vrtdst, edgdst + (e, p), cntn)
    
    def updtV (vd : FiniteDistribution[V]) = DynDst(vd, edgdst, cntn)
    
    def updtM (ed : FiniteDistribution[M]) = DynDst(vrtdst, ed, cntn)
    
    def updtC(c : Double) = DynDst(vrtdst, edgdst, c)
    
    def addC (p : Double) = DynDst(vrtdst, edgdst, cntn + p)
    
    def normalized(c : Double =0.0) = DynDst(vrtdst.normalized(c), edgdst.normalized(c), cntn)
    
    def flatten = DynDst(vrtdst.flatten, edgdst.flatten, cntn)
  }
  
  object DynDst{
    def empty = DynDst(FiniteDistribution(Seq()), FiniteDistribution(Seq()), 0)
  }
  
  trait Chain{

    val head: V
    
    def prob(d: DynDst) : Double
    
    def weightedHead(d: DynDst) = Weighted(head, prob(d))
    
//    val foot : V
    
//    val moveStack : List[Move]
  }
  
  case class AtomicChain(head: V) extends Chain{
    def prob(d: DynDst) = d.probV(head) * (1 - d.cntn)
  }
  
  case class RecChain(head: V, tail: Chain, mvtyp: M) extends Chain{
    lazy val mult = multiplicity(prev, mvtyp) 
    
    lazy val prev = tail.head
    
    def prob(d: DynDst) : Double = tail.prob(d) * d.edgdst(mvtyp) * d.cntn /mult    
  }
  
  def offspring(chain: Chain) = for (mvtyp <- movetypes; res <- moves(chain.head, mvtyp)) yield
		  				RecChain(res, chain, mvtyp) 
  
  def fullnextgen(chains: Set[Chain]) = (chains flatMap (offspring(_))) ++ chains
	  
  def prunednextgen(chains : Set[Chain], prune : Chain => Boolean) = (chains filter (prune(_)) flatMap (offspring(_))) ++ chains
  
  
  @tailrec final def propagateSteps(chains: Set[Chain], steps: Int) : Set[Chain] = 
    if (steps <1) chains else propagateSteps(fullnextgen(chains), steps -1)
  
  @tailrec final def propagate(chains: Set[Chain], prune : Chain => Boolean, 
      prev: Set[Chain] = Set.empty) : Set[Chain] = 
    if (prev == chains) chains else propagate(fullnextgen(chains), prune, chains)
  
  def finaldist(initdstbn: DynDst, chains: Set[Chain]) = {
      val dist = chains map (_.weightedHead(initdstbn))
      FiniteDistribution(dist.toSeq).normalized()
    }
  /*
   * Back-propagation along a single chain. Note that the error should already be scaled down. 
   */
  @tailrec private def backpropchain(initdstbn: DynDst, 
		  error: FiniteDistribution[V], accum: DynDst, chain: Chain) : DynDst =  chain match {
    case AtomicChain(head) => 
      val headWt = error(head) * (1 - initdstbn.cntn)
      val cntnWt = - error(head) * initdstbn.probV(head)
      DynDst.empty addV(head, headWt) addC(cntnWt)
    case rec@ RecChain(head, tail, mvtyp) =>
      val errorprop = error * (initdstbn.edgdst(mvtyp) * initdstbn.cntn /rec.mult)
      val headWt = error(head) * (1 - initdstbn.cntn)
      val cntnWt = error(head) * (tail.prob(initdstbn) * initdstbn.edgdst(mvtyp) /rec.mult - initdstbn.probV(head))
      val headdst = DynDst.empty addV(head, headWt) addC(cntnWt)
      backpropchain(initdstbn, errorprop, headdst, tail) 
  }
  
  def backprop(initdstbn: DynDst, error: FiniteDistribution[V], chains: Set[Chain]) = 
    (DynDst.empty /: (chains map (backpropchain(initdstbn, error, DynDst.empty, _ ))))(_ ++ _)
    
  type Feedback = FiniteDistribution[V] => FiniteDistribution[V]
  
  def learnstep(d: DynDst, epsilon: Double, chains: Set[Chain], cutoff: Double =0.0)(implicit correction : Feedback) = {
    val error = correction(finaldist(d, chains)) * epsilon
    (d ++ backprop(d, error, chains)).normalized(cutoff)
  }
  
  
  
  class LearningLoop(cutoff: Double, stableLevel : Double, stablSteps: Int, outerSteps: Int, epsilon: Double)(implicit feedback: Feedback){
    def spannedchains(d: DynDst) = {
      val startchains : Set[Chain] = d.vrtdst.support map (AtomicChain(_))
      val prune = (ch: Chain) => ch.prob(d) > cutoff
      propagate(startchains, prune)
    }
      
    @tailrec final def stablelearn(d: DynDst, steps: Int = stablSteps, isStable: Boolean = false) : DynDst = 
        if (steps < 1 ||  isStable) d else {
        	val nxt = learnstep(d, epsilon, spannedchains(d), cutoff)(feedback)
        	stablelearn(nxt, steps -1, nxt.norm < (stableLevel * epsilon))
        }
    
    @tailrec final def replearn(d: DynDst, steps: Int = outerSteps, isStable: Boolean = false): DynDst = 
      if (steps <1 || isStable) d else {
        val nxt = stablelearn(d)
        replearn(nxt, steps -1, nxt.norm < (stableLevel * epsilon))
      }
  }
    
}