package provingground

import akka.actor._

import akka.pattern.ask

import Hub.system

import FDactor._

class FDactor[X : LinearStructure, P](
    dyn: DiffbleFunction[X, X], //includes purging
    feedback: Double => X => X => X,
    normalize : X => X,
    init: X,
    srcRef: ActorRef,
    val param: P
    ) extends Actor{
  import LinearStructure._

  var loops = 0
  
  
  def snapShot(x: X) = SnapShot(x, self.path.name, loops, param)
  
  def shift(start: X, strictness: Double, steps: Int, epsilon : Double = 1.0) = {
    val dynLoop = DiffbleFunction.iterateDiffble(dyn, steps)
    val result = dynLoop(start)
    val imageChange = feedback(strictness)(start)(result)
    dynLoop.grad(init)(imageChange) |*| (epsilon)
  }

  var state = init

  import FDactor._



  def receive = {
    case Continue(steps, strictness, epsilon) =>
      {
        val newState = normalize(state |+| (shift(state, strictness, steps, epsilon)))
        state = newState
        loops += 1
        srcRef ! snapShot(newState)
        sender ! Done(steps, strictness, epsilon)
      }
  }
}

trait FDsrc[X, P] extends Actor{ 
  def save: SnapShot[X, P] => Unit
  
  def receive = {
    case SnapShot(x, name, loops, param) => save(SnapShot(x.asInstanceOf[X], name, loops, param.asInstanceOf[P]))
  }
}

object FDactor{
  case class Continue(steps: Int, strictness : Double = 1.0, epsilon: Double = 1.0)

  case class Done(steps: Int, strictness : Double = 1.0, epsilon: Double = 1.0)

  case class Start(runner: ActorRef, steps: Int, strictness : Double = 1.0, epsilon: Double = 1.0)

  case class StartAll(steps: Int, strictness : Double = 1.0, epsilon: Double = 1.0)

  case class State(running: Boolean, steps: Int, strictness : Double = 1.0, epsilon: Double = 1.0)

  case class SnapShot[X, P](state: X, name: String, loops: Int, param: P)
  
  case class SetParam(runner: ActorRef, steps: Int, strictness : Double = 1.0, epsilon: Double = 1.0)

  case class SetSteps(runner: ActorRef, steps: Int)

  case class SetStrictness(runner: ActorRef, steps: Double)

  case class SetEpsilon(runner: ActorRef, steps: Double)

  case class Pause(runner: ActorRef)

  case object StopAll

  case class Resume(runner: ActorRef)
}
