package provingground

import akka.actor._

import akka.pattern.ask

import Hub.system

import FDLooper._

object FDLooper {
  case class Continue(steps: Int,
                      strictness: Double = 1.0,
                      epsilon: Double = 1.0)

  case object RunnerStop
}

/**
  * Actor for carrying out loops in a learning dynamical system
  *
  * @tparam X state type
  * @param dyn evolution function
  * @param feedback Feedback, depending on _strictness_, _initial state_ and _final state_.
  * @param normalize normalization for total weight, merging equal terms etc.
  * @param init initial state
  * @param srcRef actor to whom snapshots are sent to save, send over sse etc.
  * @tparam P parameter type
  * @param param parameters for the system that can be changed while running.
  */
class FDLooper[X : LinearStructure, P](
    dyn: DiffbleFunction[X, X], //includes purging
    feedback: Double => X => X => X,
    normalize: X => X,
    init: X,
    srcRef: ActorRef,
    val param: P
)
    extends Actor {
  import LinearStructure._

  /**
    * loops completed so far
    */
  var loops = 0

  /**
    * current state (mutable)
    */
  var state = init

  /**
    * snapshot from state
    */
  def snapShot(x: X) = SnapShot(x, self.path.name, loops, param)

  /**
    * change to make for next step: evolve, compute feedback, pullback feedback
    */
  def shift(start: X, strictness: Double, steps: Int, epsilon: Double = 1.0) = {
    val dynLoop = DiffbleFunction.iterateDiffble(dyn, steps)
    val result = dynLoop(start)
    val imageChange = feedback(strictness)(start)(result)
    dynLoop.grad(init)(imageChange) |*| (epsilon)
  }

  import FDHub._

  def receive = {
    case Continue(steps, strictness, epsilon) => // another loop
      {
        val newState = normalize(
            state |+| (shift(state, strictness, steps, epsilon)))
        state = newState
        loops += 1
        srcRef ! snapShot(newState)
        sender ! Done(steps, strictness, epsilon)
      }
    case RunnerStop => //stop self after sending acknowledgement
      sender ! Stopping(self)
      println(s"actor ${self.path.name} stopping")
      context.stop(self)
  }
}

/**
  * Actor saving snapshots, typically materialized from an akka Source
  */
trait FDSrc[X, P] extends Actor {
  def save: SnapShot[X, P] => Unit

  def receive = {
    case SnapShot(x, name, loops, param) =>
      save(SnapShot(x.asInstanceOf[X], name, loops, param.asInstanceOf[P]))
  }
}
