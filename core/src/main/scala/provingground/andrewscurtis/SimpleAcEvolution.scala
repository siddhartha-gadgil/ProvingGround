package provingground.andrewscurtis

import provingground._

import FreeGroups._
import Moves._
import MoveGenerator._
import DiffStructure._
import FiniteDistribution._
import Collections._

import FiniteDistributionLearner._
import LinearStructure._

import upickle.default._


//import com.github.nscala_time.time.Imports._

/**
 * @author gadgil
 */
object SimpleAcEvolution {
  def toPresentation(rank: Int, fdV: FiniteDistribution[Moves]) = fdV map ((v: V) => Moves.actOnTriv(rank)(v).get)



  case class State(rank: Int, fdM: FiniteDistribution[AtomicMove], fdV: FiniteDistribution[Moves]) {
    def fdP = toPresentation(rank, fdV)

    def pair = (fdM, fdV)

    def pickle = {
      val pmfM = (for (Weighted(m, p) <- fdM.pmf) yield PickledWeighted(write(m), p)).toVector
      val pmfV = (for (Weighted(v, p) <- fdV.pmf) yield (PickledWeighted(write(v), p))).toVector
      PickledState(rank, pmfM, pmfV)
    }
  }

  case class PickledState(rank: Int, pmfM: Vector[PickledWeighted], pmfV: Vector[PickledWeighted]) {
    def unpickle = {
      val fdM = {
        val pmf = for (PickledWeighted(x, p) <- pmfM) yield Weighted(x, p)
        FiniteDistribution(pmf) map ((s: String) => read[AtomicMove](s))
      }

      val fdV = {
        val pmf = for (PickledWeighted(x, p) <- pmfV) yield Weighted(read[Moves](x), p)
        FiniteDistribution(pmf)
      }

      State(rank, fdM, fdV)
    }
  }

  val E = Weighted(Moves.empty, 1)

  lazy val eSet = FiniteDistributionSet(Set(E))

  lazy val eVec = FiniteDistribution(Vector(E))

  lazy val eParVec = FiniteDistributionParVec(Vector(E).par)

  def allMoves(rank: Int) = genAllMoves(rank, rank)

  def unifMoves(rank: Int) = FiniteDistribution.uniform(allMoves(rank))

  def extendedMovesList(rank: Int): List[AtomicMove] = {
    val id = List(Id)
    val inv = genAllInv(rank)
    val lftmult = genLftMult(rank)
    val rtmult = genRtMult(rank)
    val conj = genConj(rank, rank)
    val lftmultinv = genLftInvMult(rank)
    val rtmultinv = genRtInvMult(rank)
    val transp = genTranspose(rank)
    id ++ inv ++ lftmult ++ rtmult ++ conj ++ lftmultinv ++ rtmultinv ++ transp
  }
  
  def learnerMoves(rank: Int)  = {
    val inv = genAllInv(rank)
    val lftmult = genLftMult(rank)
    val rtmult = genRtMult(rank)
    val conj = genConj(rank, rank)
    val lftmultinv = genLftInvMult(rank)
    val rtmultinv = genRtInvMult(rank)
    val transp = genTranspose(rank)
    val moves = inv ++ lftmult ++ rtmult ++ conj ++ lftmultinv ++ rtmultinv ++ transp
    val moveWeight = 0.5 / (moves.size)
    val pmf =  Weighted(Id : AtomicMove, 0.5) :: (moves map (Weighted(_, moveWeight)))
    FiniteDistribution(pmf)
  }
  
  def extendedMoves(rank: Int) = FiniteDistribution.uniform(extendedMovesList(rank))
  
  def genLftInvMult(rank: Int): List[AtomicMove] = {
    (for (j <- 0 until rank; k <- 0 until rank if j != k) yield LftMultInv(j, k)).toList
  }
  
  def genRtInvMult(rank: Int): List[AtomicMove] = {
    (for (j <- 0 until rank; k <- 0 until rank if j != k) yield RtMultInv(j, k)).toList
  }
  
  def genTranspose(rank: Int): List[AtomicMove] = {
    (for (j <- 0 until rank; k <- 0 until rank if j != k) yield Transpose(j, k)).toList
  }

  case class Path(rank: Int, steps: Int,
    wordCntn: Double, size: Double, scale: Double,
    states: List[State],
    evolvedStates: List[State],
    id: String) {
    def pickle =
      PickledPath(rank, steps, wordCntn, size, scale, states map (_.pickle), evolvedStates map (_.pickle), id)

    def length = states.size

    lazy val evolution = {
      import DiffbleFunction._
      def iterateSampDiff(lst: List[M], iterations: Int = 5) =
        iterate(sampleV(size) andthen genExtendM(lst))(iterations)
      iterateSampDiff(allMoves(rank), steps)
    }

    val current = states.last

    val initV = current.fdV

    lazy val evolved = {
      val mv = evolution.func((unifMoves(rank), initV))
      State(rank, mv._1, mv._2)
    }

    lazy val imagePres = toPresentation(rank, evolved.fdV)

    lazy val presentationFeedback = imagePres.rawfeedback(FreeGroups.Presentation.weight(wordCntn))

    lazy val presFn = genPresentationMoveFn(rank)

    lazy val imageFeedbackV: FiniteDistribution[V] = presFn.grad(evolved.fdV)(presentationFeedback)

    lazy val imageFeedback = projectV.grad(evolved.pair)(imageFeedbackV)

    lazy val (feedbackM, feedbackV) = evolution.grad(current.pair)(imageFeedback)

    lazy val (nextM, nextV) = (current.fdM ++ (feedbackM * scale), current.fdV ++ (feedbackV * scale))

    lazy val nextState = State(rank, nextM, nextV)

    lazy val next = Path(rank: Int, steps: Int,
      wordCntn: Double, size: Double, scale: Double,
      states :+ nextState: List[State],
      evolvedStates :+ evolved: List[State], id)

    @annotation.tailrec
    final def quickrun(n: Int): Path = if (n < 1) this else { next.quickrun(n - 1) }

    @annotation.tailrec
    final def run(
      n: Int,
      callback: Path => Unit = (p) => {}
    ): Path =
      {
        callback(this)
        if (n < 1) this
        else next.run(n - 1, callback)
      }
  }

  object Path {
    def init(rank: Int, steps: Int,
      wordCntn: Double = 0.5, size: Double = 1000,
      scale: Double = 1.0, id: String ="ephemeral") = {
      val state = State(rank, unifMoves(rank), eVec)
      Path(rank, steps, wordCntn, size, scale, List(state), List(), id)
    }

  }

  case class PickledPath(rank: Int, steps: Int,
    wordCntn: Double, size: Double, scale: Double,
    states: List[PickledState],
    evolvedStates: List[PickledState], id: String) {
    def unpickle = Path(rank, steps, wordCntn, size, scale, states map (_.unpickle), evolvedStates map (_.unpickle), id)
  }
}
