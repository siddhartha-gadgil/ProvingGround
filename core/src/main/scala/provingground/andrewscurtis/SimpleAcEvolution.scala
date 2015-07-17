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

/**
 * @author gadgil
 */
object SimpleAcEvolution {
  def toPresentation(rank: Int, fdV : FiniteDistribution[Moves]) = fdV map ((v: V) => Moves.actOnTriv(rank)(v).get)
  
  case class State(rank: Int, fdM: FiniteDistribution[AtomicMove], fdV : FiniteDistribution[Moves]){
    def fdP = toPresentation(rank, fdV)

    def pair = (fdM, fdV)
    
    def pickle = {
      val pmfM = (for (Weighted(m, p) <- fdM.pmf) yield (m.toPlainString, p)).toList
      val pmfV = (for (Weighted(v, p) <- fdV.pmf) yield (v.moves.map(_.toString), p)).toList
      PickledState(rank, pmfM, pmfV)
    }    
  }

  case class PickledState(rank: Int, pmfM: List[(String, Double)], pmfV : List[(List[String], Double)]){
    def unpickle = {
      val fdM = {
        val pmf = for ((x, p) <- pmfM) yield Weighted(x, p)
        FiniteDistribution(pmf) map ((s: String) => AtomicMove.fromString(s).get)
      }

      val fdV = {
        val pmf = for ((x, p) <- pmfV) yield Weighted(Moves.fromString(x).get, p)
        FiniteDistribution(pmf)
      }

      State(rank, fdM, fdV)
    }    
  }

  val E = Weighted(Moves.empty, 1)

  lazy val eSet = FiniteDistributionSet(Set(E))

  lazy val eVec = FiniteDistributionVec(Vector(E))

  lazy val eParVec = FiniteDistributionParVec(Vector(E).par)

  def allMoves(rank: Int) = genAllMoves(rank, rank)

  def unifMoves(rank: Int) = FiniteDistribution.uniform(allMoves(rank))


  case class Path(rank: Int, steps: Int,
      wordCntn: Double, size: Double, scale: Double,
      states: List[State],
      evolvedStates: List[State]){
    def pickle = 
      PickledPath(rank, steps, wordCntn, size, scale, states map (_.pickle), evolvedStates map (_.pickle))
    
    lazy val evolution = {
      iterateDiff(allMoves(rank), steps)
    }  
    
    val current = states.last
    
    val initV = current.fdV
    
    lazy val evolved = {      
      val mv = evolution.func((unifMoves(rank), initV))
      State(rank, mv._1, mv._2)
    }
    
    lazy val imagePres = toPresentation(rank, evolved.fdV)
    
    lazy val presentationFeedback = imagePres.feedback(FreeGroups.Presentation.weight(wordCntn))
    
    lazy val presFn = genPresentationMoveFn(rank)
    
    lazy val imageFeedbackV : FiniteDistribution[V] = presFn.grad(evolved.fdV)(presentationFeedback)
    
    lazy val imageFeedback = projectV.grad(evolved.pair)(imageFeedbackV)
    
    lazy val (feedbackM, feedbackV) = evolution.grad(current.pair)(imageFeedback)
    
    lazy val (nextM, nextV) = (current.fdM ++ (feedbackM * scale), current.fdV ++ (feedbackV * scale))
    
    lazy val nextState = State(rank, nextM, sample(size)(nextV))
    
    lazy val next = Path(rank: Int, steps: Int,
      wordCntn: Double, size: Double, scale: Double,
      states :+ nextState: List[State],
      evolvedStates :+ evolved: List[State])
      
    @annotation.tailrec 
    final def quickrun(n: Int) : Path = if (n < 1) this else {next.quickrun(n -1)}
    
    final def run(n: Int, 
        callback: Path => Unit = (p) => {}) : Path = 
      if (n < 1) this 
      else {
        val result = next.run(n -1, callback)
        callback(result)
        result}
  }
  
  object Path{
    def init(rank: Int, steps: Int, 
        wordCntn: Double = 0.5, size: Double = 1000, 
        scale: Double = 0.1) ={
          val state = State(rank, unifMoves(rank), eVec)
          Path(rank, steps, wordCntn, size, scale, List(state), List())
    }
  }

  case class PickledPath(rank: Int, steps: Int,
      wordCntn: Double, size: Double, scale: Double,
      states: List[PickledState],
      evolvedStates: List[PickledState]){
    def unpickle = Path(rank, steps, wordCntn, size, scale, states map (_.unpickle), evolvedStates map (_.unpickle))
  }
}
