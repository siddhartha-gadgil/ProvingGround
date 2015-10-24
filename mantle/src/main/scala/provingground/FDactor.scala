package provingground

import akka.actor._

import Hub.system

class FDactor[X : LinearStructure](
    dyn: DiffbleFunction[X, X], //includes purging
    feedback: Double => X => X => X,
    init: X,
    save : X => Unit    
    ) extends Actor{
  import LinearStructure._
  
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
        val newState = state |+| (shift(state, strictness, steps, epsilon))
        state = newState
        save(state)
        sender ! Done(steps, strictness, epsilon)
      }
  }
}

object FDactor{
  case class Continue(steps: Int, strictness : Double = 1.0, epsilon: Double = 1.0)
  
  case class Done(steps: Int, strictness : Double = 1.0, epsilon: Double = 1.0)
  
  case class Start(runner: ActorRef, steps: Int, strictness : Double = 1.0, epsilon: Double = 1.0)
  
  case class StartAll(steps: Int, strictness : Double = 1.0, epsilon: Double = 1.0)
  
  case class State(running: Boolean, steps: Int, strictness : Double = 1.0, epsilon: Double = 1.0)
  
  case class SetParam(runner: ActorRef, steps: Int, strictness : Double = 1.0, epsilon: Double = 1.0)

  case class SetSteps(runner: ActorRef, steps: Int)
  
  case class SetStrictness(runner: ActorRef, steps: Double)
  
  case class SetEpsilon(runner: ActorRef, steps: Double)
  
  case class Pause(runner: ActorRef)
  
  case class Resume(runner: ActorRef)
}
