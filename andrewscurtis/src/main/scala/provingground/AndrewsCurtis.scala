package provingground.andrewscurtis

import FreeGroups._

import provingground._

import learning._

//import FiniteDistribution._
import annotation._
//import play.api.libs.json._
//import play.api.libs.iteratee._
//import play.api.libs.concurrent._
//import akka.actor._
//import play.api.libs.concurrent.Execution.Implicits._
//import scala.concurrent._
//import play.api.libs.json.Json.toJsFieldJsValueWrapper

object AndrewsCurtis {

  /*
   * The vertices (e.g. presentations) and edgetype/movetype for the dynamics. We assume there are no combinations here.
   * Other generic objects also extend this.
   */
  //  type DynObj = ACobject

  /*
   * Distribution on dynobjects - this is what evolves during learning.
   */
  //  type DynDstbn = FiniteDistribution[DynObj]

  type DynDstbn = DynDst[Presentation, ACMoveType]

  case class DynDst[V, E](vrtdst: FiniteDistribution[V],
                          edgdst: FiniteDistribution[E],
                          cntn: Double) {
    def probV(v: V): Double = vrtdst(v)

    def probE(e: E): Double = edgdst(e)

    def ++(that: DynDst[V, E]): DynDst[V, E] =
      DynDst(vrtdst ++ that.vrtdst, edgdst ++ that.edgdst, cntn + that.cntn)

    def addV(v: V, p: Double): DynDst[V, E] = DynDst(vrtdst .+ (v, p), edgdst, cntn)

    def addE(e: E, p: Double): DynDst[V, E] = DynDst(vrtdst, edgdst .+ (e, p), cntn)

    def updtV(vd: FiniteDistribution[V]): DynDst[V, E] = DynDst(vd, edgdst, cntn)

    def updtE(ed: FiniteDistribution[E]): DynDst[V, E] = DynDst(vrtdst, ed, cntn)

    def updtC(c: Double): DynDst[V, E] = DynDst(vrtdst, edgdst, c)

    def addC(p: Double): DynDst[V, E] = DynDst(vrtdst, edgdst, cntn + p)

    def normalized(c: Double): DynDst[V, E] =
      DynDst(vrtdst.normalized(c), edgdst.normalized(c), cntn)

    def flatten: DynDst[V, E] = DynDst(vrtdst.flatten, edgdst.flatten, cntn)
  }

  object DynDst {
    def empty[V, E]: DynDst[V, E] =
      DynDst(FiniteDistribution.empty[V], FiniteDistribution.empty[E], 0)
  }

  type Vert = Presentation

  //  val isPresentation : ACobject => Boolean = {
  //    case pres : Presentation => true
  //    case _ => false
  //  }

  //  val WtdPresentation : PartialFunction[Weighted[DynObj], Weighted[Presentation]] = {
  //    case Weighted(pres : Presentation, wt) => Weighted(pres, wt)
  //  }

  //  def FDVert(d : DynDstbn) : FiniteDistribution[Vert] = {
  //     val rawpmf = d.pmf collect WtdPresentation
  //     FiniteDistribution(rawpmf).normalized()
  //  }

  /*
   * Extend a path by another move. Need to do this differently while abstracting.
   */
  //  case object PathContinue extends DynObj

  /*
   * The type of moves
   */
  type MoveType = ACMoveType

  /*
   * Andrews-Curtis moves
   */
  sealed trait ACMoveType

  object ACMoveType {
    case object ACStabMv   extends ACMoveType
    case object ACDeStabMv extends ACMoveType
    case object RtMultMv   extends ACMoveType
    case object LftMultMv  extends ACMoveType
    case object ConjMv     extends ACMoveType
    case object InvMv      extends ACMoveType

    def fromString(mv: String): ACMoveType =
      (MoveTypeList find (_.toString == mv)).get
  }

  import ACMoveType._
  val MoveTypeList: List[ACMoveType] =
    List(ACStabMv, ACDeStabMv, RtMultMv, LftMultMv, ConjMv, InvMv)

  /*
  implicit object ACMoveFreqFormat extends Format[FiniteDistribution[ACMoveType]]{
    def reads(js : JsValue) : JsResult[FiniteDistribution[ACMoveType]] = {
      val pmf = for ((x, y) <- js.as[Map[String, Double]]) yield Weighted(ACMoveType.fromString(x), y)
      JsSuccess(FiniteDistribution(pmf.toSet))
    }

/*
    def writes(fd : FiniteDistribution[ACMoveType]): JsValue = {
      Json.toJson((for (Weighted(x, y) <- fd.pmf) yield (x.toString, y)).toMap)
    }
   */
  }

   */

  /*
   * Multiplicity of moves of a given type given a presentation. In general this should be a function of the presentation.
   */
  def multiplicity(rk: Int): MoveType => Long = {
    case ACStabMv   => 1: Long
    case ACDeStabMv => (1: Long) // can be zero in practice
    case RtMultMv   => (rk.toLong: Long) * ((rk.toLong) - 1)
    case LftMultMv  => (rk.toLong) * ((rk.toLong) - 1)
    case ConjMv     => (rk.toLong) * ((rk.toLong) * 2 - 1)
    case InvMv      => (rk.toLong)
  }

  def allMoves(pres: Presentation): MoveType => List[Move] = {
    case ACStabMv => List(ACStab)
    case ACDeStabMv =>
      if (pres.acStabilized) List(ACDeStab) else List()
    case RtMultMv =>
      val n = pres.rank
      (for (j <- 0 until n; k <- 0 until n if j != k)
        yield RtMult(j, k)).toList
    case LftMultMv =>
      val n = pres.rank
      (for (j <- 0 until n; k <- 0 until n if j != k)
        yield LftMult(j, k)).toList
    case ConjMv =>
      val n = pres.rank
      (for (j <- 0 until n; k <- -n to n if k != 0) yield Conj(j, k)).toList
    case InvMv =>
      val n = pres.rank
      (for (j <- 0 until n) yield Inv(j)).toList
  }

  private val vrtdst = FiniteDistribution(Set(Weighted(nullpres, 1.0)))
  private val edgseq = for (mvtyp <- MoveTypeList)
    yield Weighted(mvtyp, 1.0 / MoveTypeList.length)
  private val edgdst = FiniteDistribution(edgseq.toSet)
  val baseDstbn      = DynDst(vrtdst, edgdst, 0.3)

  trait Move extends (Vert => Vert) {
    val mvType: MoveType
  }

  case object ACStab extends Move {
    def apply(p: Presentation): FreeGroups.Presentation = p.acStab

    val mvType: ACMoveType = ACStabMv
  }

  case object ACDeStab extends Move {
    def apply(p: Presentation): FreeGroups.Presentation = p.acDestabilized

    val mvType : ACMoveType = ACStabMv
  }

  case class RtMult(k: Int, l: Int) extends Move {
    def apply(p: Presentation): FreeGroups.Presentation = p.rtmult(k, l)

    val mvType : ACMoveType = ACStabMv
  }

  case class LftMult(k: Int, l: Int) extends Move {
    val mvType : ACMoveType = ACStabMv

    def apply(p: Presentation): FreeGroups.Presentation = p.lftmult(k, l)
  }

  case class Conj(k: Int, l: Int) extends Move {
    def apply(p: Presentation): FreeGroups.Presentation = p.conj(k, l)

    val mvType : ACMoveType= ACStabMv
  }

  case class Inv(k: Int) extends Move {
    def apply(p: Presentation): FreeGroups.Presentation = p.inv(k)

    val mvType : ACMoveType = ACStabMv
  }

  /*
   * A chain of moves starting at foot and ending at head.
   */
  trait Chain {

    val head: Vert

    def prob(d: DynDstbn): Double

    val foot: Vert

    val moveStack: List[Move]
  }

  case class AtomicChain(head: Vert) extends Chain {
    // choose the head, then don't continue
    def prob(d: DynDstbn): Double = d.probV(head) * (1 - d.cntn)

    val foot: Vert = head

    lazy val moveStack : List[Move] = List()
  }

  case class RecChain(start: Chain, move: Move) extends Chain {
    val head: Vert = move(start.head)

    lazy val foot: Vert = start.foot

    lazy val moveStack: List[Move] = move :: start.moveStack
    /*
     * start.prob = probability of reaching start and not continuing.
     * multiply by the probability of continuing from start and making the given move.
     * The latter involves diving by the multiplicity of the move type.
     * We need to multiply by the probability of not continuing, but this is part of start.prob already.
     */
    def prob(d: DynDstbn): Double =
      start.prob(d) * d.probE(move.mvType) * d.cntn / multiplicity(head.rank)(
        move.mvType)
  }

  object Chain {
    @tailrec def addMoves(start: Chain, moveStack: List[Move]): Chain =
      moveStack match {
        case List() => start
        case xs     => addMoves(RecChain(start, xs.last), xs.dropRight(1))
      }

    /*
     * Subchains with the same head. Recursively defined with tail being moves to be appended to each chain
     */
    @tailrec def subchains(chain: Chain,
                           tail: List[Move] = List(),
                           accum: Set[Chain] = Set()): Set[Chain] =
      chain match {
        case AtomicChain(head) => accum + addMoves(chain, tail)
        case RecChain(start, move) =>
          subchains(start, tail :+ move, accum + addMoves(chain, tail))
      }

    def apply(foot: Vert, moveStack: List[Move]): Chain =
      addMoves(AtomicChain(foot), moveStack)

    def offspring(chain: Chain): List[RecChain] =
      for (mvTyp <- MoveTypeList; mv <- allMoves(chain.head)(mvTyp))
        yield RecChain(chain, mv)

    def fullnextgen(chains: Set[Chain]): Set[Chain] =
      (chains flatMap (offspring(_))) ++ chains

    def prunednextgen(chains: Set[Chain], prune: Chain => Boolean): Set[Chain] =
      (chains filter (prune(_)) flatMap (offspring(_))) ++ chains

    /*
	   * Back-propagating a weight on a chain recursively, but not its subchains with the same head to avoid multiple counting
	   * mult : the weight (multiplier)
	   * accum : accumulater, initially empty
	   * This should be used with care because of overlapping chains, so multiple count.
	   * In particular, we backprop multiplying only by start.prob(d), and not the cumulative.
	   */
    @annotation.tailrec
    def backprop(chain: Chain,
                 mult: Double,
                 d: DynDstbn,
                 accum: DynDstbn): DynDstbn =
      chain match {
        case AtomicChain(head) =>
          accum.addV(head, mult * (1 - d.cntn)) addC (-mult * d.probV(head))
        case chn @ RecChain(start, move) =>
          // Number of moves of the given move type for the presentation.
          val multplcty: Long = multiplicity(chain.head.rank)(move.mvType)
          val newmult =
            mult * d.probE(move.mvType) * d.cntn / multplcty // to be passed on to the previous chain
          val mvTypWeight = start.prob(d) * d.cntn / multplcty
          val cntnWt      = start.prob(d) * d.probE(move.mvType) / multplcty
          val newaccum =
            accum.addE(move.mvType, mult * mvTypWeight) addC (mult * cntnWt)
          backprop(start, newmult, d, newaccum)
      }
  }

  /*
   * The distribution on presentations via head of chains induced by that on presentations, moves and continuation.
   */
  def dstbn(chains: Set[Chain], d: DynDstbn): FiniteDistribution[Vert] = {
    val fdchains = FiniteDistribution(
      for (chn <- chains) yield Weighted(chn, chn.prob(d)))
    (fdchains map ((chn: Chain) => chn.head)).flatten
  }

  /*
   * Back-propagate a feedback on distribution on presentations.
   */
  def backpropdstbn(chains: Set[Chain],
                    feedback: FiniteDistribution[Vert],
                    d: DynDstbn): DynDst[_root_.provingground.andrewscurtis.FreeGroups.Presentation, MoveType] = {
    //    val empty = FiniteDistribution[DynObj](Set())
    val empty = DynDst(FiniteDistribution.empty[Presentation],
                       FiniteDistribution.empty[MoveType],
                       0)
    val bcklist =
      chains map ((chn) => Chain.backprop(chn, feedback(chn.head), d, empty))
    (bcklist.foldRight(empty)(_ ++ _)).flatten
  }

  /*
   * The background weights in the Andrews-Curtis case
   */

  def ACbgWt(presCntn: Double, wrdCntn: Double): Vert => Double =
    presentationWeight(_, presCntn, wrdCntn)

  /*
   * Feedback for distribution on presentations, comparing probabilities from generating presentations by words
   * with generating from Andrews-Curtis moves. This has total zero.
   * A high (raw) feedback is for a simple distribution needing a lot of moves.
   */
  def dstbnFeedback(presdstbn: FiniteDistribution[Vert],
                    bgwt: Vert => Double): FiniteDistribution[Vert] = {
    val dstbnpmf = presdstbn.pmf
    val fdbkrawpmf = for (Weighted(pres, prob) <- dstbnpmf)
      yield (Weighted(pres, bgwt(pres) / prob))
    val fdbkpmftot = (fdbkrawpmf map (_.weight)).sum
    val fdbkpmf =
      fdbkrawpmf map
        ((x) => Weighted(x.elem, x.weight - (fdbkpmftot / fdbkrawpmf.size)))
    FiniteDistribution(fdbkpmf)
  }

  /*
   * Change the initial distribution based on feedback, possibly pruning at threshold. This is the learning step.
   */
  def dstbnflow(chains: Set[Chain],
                d: DynDstbn,
                bgwt: Vert => Double,
                epsilon: Double,
                threshold: Double = 0): DynDst[FreeGroups.Presentation, AndrewsCurtis.ACMoveType] = {
    val presdstbn       = dstbn(chains, d)
    val feedback        = dstbnFeedback(presdstbn, bgwt) * epsilon
    val shift: DynDstbn = backpropdstbn(chains, feedback, d)
    (d ++ shift).normalized(threshold)
  }

  /*
   * Generate chains based on a cutoff (which stops descendants).
   */
  @annotation.tailrec
  def chainGenCutoff(chains: Set[Chain],
                     d: DynDstbn,
                     cutoff: Double): Set[Chain] = {
    val prune: Chain => Boolean = (chain) => chain.prob(d) > cutoff
    val nextgen                 = Chain.prunednextgen(chains, prune)
    if (chains == nextgen) chains else chainGenCutoff(nextgen, d, cutoff)
  }

  /*
   * Presentations in the support of the evolving distribution.
   */
  def presSupp(d: DynDstbn): Set[_root_.provingground.andrewscurtis.FreeGroups.Presentation] = (d.vrtdst.pmf map (_.elem)).toSet

  /*
   * The initial chains, based on the dynamic distribution, from which chains are generated.
   * This is also useful for reporting interesting presentations.
   */
  def initChains(d: DynDstbn): Set[Chain] = presSupp(d) map (AtomicChain(_))

  /*
   * Next step of the evolution of the distribution, assuming selection of sequences based on cutoff
   *
   */
  def dstbnFlowCutoff(d: DynDstbn,
                      bgwt: Vert => Double,
                      epsilon: Double,
                      cutoff: Double): DynDstbn = {
    val chainSet =
      chainGenCutoff(initChains(d), d, cutoff) flatMap (Chain.subchains(_))
    dstbnflow(chainSet, d, bgwt, epsilon)
  }

  /*
   * flow for tuning, without adding vertices to the support. This is accomplished by not propogating on subchains
   */
  def tuneFlowCutoff(d: DynDstbn,
                     bgwt: Vert => Double,
                     epsilon: Double,
                     cutoff: Double): DynDst[FreeGroups.Presentation, AndrewsCurtis.ACMoveType] = {
    dstbnflow(chainGenCutoff(initChains(d), d, cutoff), d, bgwt, epsilon)
  }

  /*
   * The best chain for a presentation. We record these (perhaps in MongoDb) while forgetting other chains.
   */
  def bestChain(pres: Vert, chains: Set[Chain], d: DynDstbn): Chain =
    chains filter (_.head == pres) maxBy (_.prob(d))

  /*
  def wtdPresJson(pw : Weighted[Presentation]) = {
      val listlist = pw.elem.rels map (_.ls)
      Json.obj("rank" -> pw.elem.rank, "words" -> listlist, "prob" -> pw.weight)
    }

  def dstbnJson(dstbn : FiniteDistribution[Presentation]) = Json.toJson(dstbn.pmf.toList map (wtdPresJson(_)))

  implicit val dstbnToJson = Enumeratee.map((dstbn : FiniteDistribution[Presentation]) => dstbnJson(dstbn))

// Short Loop: Flow for a while, purge and report survivors
// Long loop : Repeat short loop
   */
  def initDstbn(pthCntn: Double) =
    DynDst(FiniteDistribution(Set(Weighted(nullpres, 1.0))),
           FiniteDistribution.empty[Presentation],
           pthCntn)

  case class ACparameters(epsilon: Double = 1.0 / 100.0,
                          cutoff: Double = 1.0 / 1000000.0,
                          purgeLevel: Double = 1.0 / 10000.0,
                          pthCntn: Double = 0.7,
                          presCntn: Double = 0.7,
                          wrdCntn: Double = 0.7,
                          tuneLoops: Int = 25,
                          growLoops: Int = 15) {

    def bgwt: Vert => Double = ACbgWt(presCntn, wrdCntn)

    def tuneflow: DynDstbn => DynDstbn =
      tuneFlowCutoff(_, bgwt, epsilon, cutoff)

    def dstbnflow: DynDstbn => DynDstbn =
      dstbnFlowCutoff(_, bgwt, epsilon, cutoff)

    def multituneflow: DynDstbn => DynDstbn =
      IterateDyn(_, tuneflow, tuneLoops)

    def growflow: DynDstbn => DynDstbn = (d) => dstbnflow(multituneflow(d))

    def multigrowflow: DynDstbn => DynDstbn =
      IterateDyn(_, growflow, growLoops)

    val flow: DynDstbn => DynDstbn = (d) =>
      multigrowflow(d).normalized(purgeLevel)
  }

  case class ACFlowData(params: ACparameters, dstbn: DynDstbn)
}
