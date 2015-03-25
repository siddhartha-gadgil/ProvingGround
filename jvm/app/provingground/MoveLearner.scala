package provingground

import provingground.Collections._
import annotation._

/**
 * Learning dynamical system where we only have moves, not combinations.
 * Example: Andrews-Curtis
 *
 * The dynamics is given by:
 *
 * Generating chains by making moves
 *
 * A parameter deciding whether to continue a chain.
 *
 * A move type chosen if a chain is to be continued.
 *
 * A move of the type chosen with equal probability - warning: avoid 0/0
 *
 *
 * A chain is a sequence of moves.
 *
 * We start with singleton chains and evolve according to the moves to get a distribution on chains.
 *
 * This gives a distribution on vertices as well
 *
 * This is used to determine feedback for learning.
 *
 *
 *
 * @param movetypes Move types
 *
 * @param moves Moves for a given object (vertex) and move type
 *
 * @tparam V vertices
 *
 * @tparam M move types (not moves)
 *
 */
class MoveLearner[V, M](movetypes: List[M], moves : (V, M) => Set[V]){

  /**
   * The number of moves given move type and vertex
   */
  def multiplicity(vert: V, movetype: M) = moves(vert, movetype).size

  /**
   * The state of the learning system.
   *
   * This may also represent a change of state, so the total may not be 1
   *
   * @param vrtdst A distribution on vertices
   *
   * @param edgdst A distribution on move types (edges)
   *
   * @param cntn The probability of continuing.
   *
   */
  case class DynDst(vrtdst: FiniteDistribution[V], edgdst : FiniteDistribution[M], cntn : Double){
    /**
     * The total probability, for distributions not yet normalized
     */
    lazy val  norm = vrtdst.norm + edgdst.norm + cntn.abs

    /**
     * probability of a vertex
     */
    def probV (v : V) = vrtdst(v)

    /**
     * probability of a move type
     */
    def probM (e : M) = edgdst(e)

    /**
     * Add a distribution, result not normalized unless total is zero for that.
     */
    def ++ (that : DynDst) = DynDst(vrtdst ++ that.vrtdst, edgdst ++ that.edgdst, cntn + that.cntn)

    /**
     *
     * Subtract a distribution
     */
    def -- (that : DynDst) = DynDst(vrtdst ++ (that.vrtdst * (-1)), edgdst ++ (that.edgdst * (-1)), cntn - that.cntn)

    /**
     * Add a vertex with weight or add to the probability of the vertex
     */
    def addV (v : V, p : Double) = DynDst(vrtdst + (v, p), edgdst, cntn)

    /**
     * Add a move type with weight or add to the probability of the move type.
     */
    def addM ( e : M, p : Double) = DynDst(vrtdst, edgdst + (e, p), cntn)

    /**
     * Add to the probability of continuing a chain.
     */
    def addC (p : Double) = DynDst(vrtdst, edgdst, cntn + p)

    /**
     * Update the vertex distribution
     */
    def updtV (vd : FiniteDistribution[V]) = DynDst(vd, edgdst, cntn)

    /**
     * Update the distribution on move types
     */
    def updtM (ed : FiniteDistribution[M]) = DynDst(vrtdst, ed, cntn)

    /**
     * Update the probability of continuation
     */
    def updtC(c : Double) = DynDst(vrtdst, edgdst, c)

    /**
     * Normalized distribution
     */
    def normalized(c : Double =0.0) =
    	DynDst(vrtdst.normalized(c), edgdst.normalized(c), cntn)


    /**
     * Flatten all distributions - group together equal vertices.
     */
    def flatten = DynDst(vrtdst.flatten, edgdst.flatten, cntn)

    def entropyView(lines: Int = 20) = MoveLearner.DynDstView(vrtdst.entropyView(lines),
        edgdst.entropyView(lines), cntn)
  }

  /**
   * companion to state of dynamical system
   */
  object DynDst{
    /**
     * Just for no change, should not use as state.
     */
    def empty = DynDst(FiniteDistribution(Seq()), FiniteDistribution(Seq()), 0)
  }

  /**
   * A chain, i.e. a sequence of vertices related by moves.
   */
  trait Chain{
	/**
	 * The final vertex.
	 */
    val head: V

    /**
     * The probability of the chain.
     */
    def prob(d: DynDst) : Double

    /**
     * The head (final vertex) with probability of the chain.
     */
    def weightedHead(d: DynDst) = Weighted(head, prob(d))

  }

  /**
   * An atomic chain
   */
  case class AtomicChain(head: V) extends Chain{
    /**
     * Probability of generating the atomic chain. Namely, we choose head, then terminate.
     */
    def prob(d: DynDst) = d.probV(head) * (1 - d.cntn)
  }

  /**
   * A recursive chain, making a move from a previous chain
   *
   * @param head head of the new chain. Not determined by move type and previous chain
   *
   * @param tail tail of the new chain
   *
   * @param mvtyp move type for extending the chain
   */
  case class RecChain(head: V, tail: Chain, mvtyp: M) extends Chain{
    /**
     * number of moves of the given type starting with head of tail
     */
    lazy val mult = multiplicity(prev, mvtyp)

    /**
     * head of the tail.
     */
    lazy val prev = tail.head

    /**
     * probability of generating a chain:
     *
     * Note: the chain must terminate, but this is already the probability of termination of the previous chain.
     *
     */
    def prob(d: DynDst) : Double = tail.prob(d) * d.edgdst(mvtyp) * d.cntn /mult
  }

  /**
   * sub-chains of a chain, with the same head but starting at different stages.
   * Recursively defined.
   *
   *
   * @param chn chain for which we find sub-chains
   *
   * @param accum accumulator, should contain chains with given head
   *
   * @param gives chains with initial head from those with the head of the recursively passed chain.
   */
  	@tailrec final def subChains(chn: Chain, accum: Set[Chain]= Set(), append: Chain => Chain = (c) => c) : Set[Chain] = chn match {
  	  case chain: AtomicChain => accum + (append(chain))
  	  case RecChain(head, tail, mvTyp) =>
  	    val newAppend = (c: Chain) => RecChain(head, append(c), mvTyp) : Chain
  	    val newAccum = accum + append(chn)
  	    subChains(tail, newAccum, newAppend)
  	}

  /**
   * offspring of a chain, applying all possible moves to its head.
   */
  def offspring(chain: Chain) = for (mvtyp <- movetypes; res <- moves(chain.head, mvtyp)) yield
		  				RecChain(res, chain, mvtyp)
  /**
   * offspring of chains as well as the chains themselves.
   */
  def fullnextgen(chains: Set[Chain]) = (chains flatMap (offspring(_))) ++ chains

  /**
   * offspring of chains which are accepted by prune together with the chains.
   * Note that chains failing prune are not removed but only not propagatted.
   *
   * @param chains initial chains
   *
   * @param prune boolean function determining chains allowed to propagate.
   */
  def prunednextgen(chains : Set[Chain], prune : Chain => Boolean) = (chains filter (prune(_)) flatMap (offspring(_))) ++ chains

  /**
   * forward propagate the dynamics
   *
   * @param chains initial set of chains
   *
   * @param steps number of steps
   */
  @tailrec final def propagateSteps(chains: Set[Chain], steps: Int) : Set[Chain] =
    if (steps <1) chains else propagateSteps(fullnextgen(chains), steps -1)

    /**
     * forward propagate the dynamics while pruning until stable.
     *
     * @param chains initial set of chains
     *
     * @param prune boolean function for pruning.
     */
  @tailrec final def propagate(chains: Set[Chain], prune : Chain => Boolean,
      prev: Set[Chain] = Set.empty) : Set[Chain] =
    if (prev == chains) chains else propagate(prunednextgen(chains, prune), prune, chains)

    /**
     * The distribution on vertices as heads of chains.
     *
     * @param dstbn dynamical distribution.
     *
     * @param chains support for which probability is calculated and mapped to heads.
     */
  def finaldist(dstbn: DynDst, chains: Set[Chain]) = {
      val dist = chains map (_.weightedHead(dstbn))
      FiniteDistribution(dist.toSeq).normalized()
    }

  /**
   * Recursively back-propagate along a single chain.
   * For an atomic chain, the probability of choosing the head is changed, as is the probability of continuing.
   * If chain is not atomic, the error propagates to the tail of the chain.
   *
   *
   * @param initdstbn - the initial distribution
   *
   * @param error - the error to recursively propagate.
   *
   * @param accum accumulator
   *
   * @param chain along which to back-propagate.
   *
   * @return change in distribution
   */
  @tailrec final def backpropchain(initdstbn: DynDst,
		  error: Double, accum: DynDst = DynDst.empty, chain: Chain) : DynDst =  chain match {
    case AtomicChain(head) =>
      val headWt = error * (1 - initdstbn.cntn)
      val cntnWt = - error * initdstbn.probV(head)
      accum addV(head, headWt) addC(cntnWt)
    case rec@ RecChain(head, tail, mvtyp) =>
      val errorprop = error * (initdstbn.edgdst(mvtyp) * initdstbn.cntn /rec.mult)
      val mvWt = error * (tail.prob(initdstbn) * initdstbn.cntn /rec.mult)
      val newAccum  = accum addM (mvtyp, mvWt)
//      val headWt = error * (1 - initdstbn.cntn)
//      val cntnWt = error * (tail.prob(initdstbn) * initdstbn.edgdst(mvtyp) /rec.mult - initdstbn.probV(head))
//      val headdst = accum addV(head, headWt) addC(cntnWt)
      backpropchain(initdstbn, errorprop, newAccum, tail)
  }

  /**
   * total back-propagation over a collection of chains
   */
  def backprop(initdstbn: DynDst, error: FiniteDistribution[V], chains: Set[Chain]) =
    (DynDst.empty /: (chains map ((chain) => backpropchain(initdstbn, error(chain.head), DynDst.empty, chain ))))(_ ++ _)

    /**
     * feedback for learning, generally given by an implicit object.
     * can be constructed using the feedback method on a finite distribution, comparing to a base that need not be finite.
     */
  type Feedback = FiniteDistribution[V] => FiniteDistribution[V]

  /**
   * change of dynamical distribution in a single learning step, not changing the chains.
   *
   * @param d initial distribution
   *
   * @param epsilon scale for shifting
   *
   * @param chains support for learning
   *
   * @param cutoff purge support below cutoff
   *
   * @param correction feedback given the final distribution.
   */
  def learnstep(d: DynDst, epsilon: Double, chains: Set[Chain], cutoff: Double =0.0)(implicit correction : Feedback) = {
    val error = correction(finaldist(d, chains)) * epsilon
    (d ++ backprop(d, error, chains)).normalized(cutoff)
  }


  /**
   * loop for learning, with an inner loop changing only the distribution and
   * an outer one where chains are recomputed with the new purge.
   *
   * @param cutoff cutoff for learning step
   */
  case class LearningLoop(cutoffExp: Double, stableLevelExp : Double, innerSteps: Int, outerSteps: Int, epsilonExp: Double)(feedback: Feedback){
    /**
     * chains generated by support of d under propagation with pruning up to stabilization.
     */
    val cutoff = math.pow(2.0, -cutoffExp)

    val epsilon = math.pow(2.0, -epsilonExp)

    val stableLevel = math.pow(2.0, -stableLevelExp)

    def spannedchains(d: DynDst) = {
      val startchains : Set[Chain] = d.vrtdst.support map (AtomicChain(_))
      val prune = (ch: Chain) => ch.prob(d) > cutoff
      propagate(startchains, prune) flatMap (subChains(_))
    }

    /**
     * learning without changing the collection of chains used (the support).
     * terminates when the isStable condition occurs or when the given number of steps are run.
     * Recursively, the isStable condition is that the new distribution is close to the old one.
     * This is the inner loop.
     */
    @tailrec final def innerlearn(d: DynDst, steps: Int = innerSteps, isStable: Boolean = false, chains: Set[Chain]) : DynDst =
        if (steps < 1 ||  isStable) d else {
        	val nxt = learnstep(d, epsilon, chains, cutoff)(feedback)
        	innerlearn(nxt, steps -1, (d -- nxt).norm < (stableLevel * epsilon), chains)
        }

    /**
     * learning with changing the collection of chains used, using innerlearn.
     * terminates when the isStable condition occurs or when the given number of steps are run.
     * Recursively, the isStable condition is that the new distribution is close to the old one.
     * This is the outer loop.
     */
    @tailrec final def outerlearn(d: DynDst, steps: Int = outerSteps, isStable: Boolean = false): DynDst =
      if (steps <1 || isStable) d else {
        val nxt = innerlearn(d, innerSteps, false, spannedchains(d))
        outerlearn(nxt, steps -1, (d -- nxt).norm < (stableLevel * epsilon))
      }

  }




}

object MoveLearner{
    case class DynDstView(vrtdst: List[Weighted[String]],
        edgdst : List[Weighted[String]], cntn : Double)
}
