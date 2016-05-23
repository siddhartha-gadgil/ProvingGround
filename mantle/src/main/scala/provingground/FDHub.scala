package provingground

import akka.actor._
import akka.pattern.ask
import scala.concurrent.duration._
import akka.util.Timeout

import FDLooper._

import FDHub._

/**
  * Actor controlling (bu not creating) the various loopers for running;
  * maintains info on the state etc. of loopers,
  * keeps telling a running looper to continue.
  * Running loopers is separated into spawning them, in general from outside the actor system,
  * and starting them from a Hub.
  */
class FDHub extends Actor {
  var loopers: Map[ActorRef, State] = Map.empty

  def names = (for ((r, x) <- loopers) yield r.path.name).toList

  def states = (for ((r, x) <- loopers) yield (r.path.name -> x))

  def receive = {
    case Start(
        runner: ActorRef, steps: Int, strictness: Double, epsilon: Double) =>
      // start a looper
      {
        loopers = loopers + (runner -> State(true, steps, strictness, epsilon))
        println(runner)
        println(loopers)
        runner ! Continue(steps, strictness, epsilon)
      }

    case StopHub =>
      // send messages to all loopers to stop. When return messages come, stop hub.
      {
        for (runner <- loopers.keys) runner ! RunnerStop
//      loopers = Map.empty
      }

    case Stopping(ref) => // message that a looper has stopped.
      loopers = loopers filter (_._1 != ref)
      if (loopers.isEmpty) {
        println("All loopers stopped, hub stopping")
//        Hub.system.shutdown()
        context.stop(self)
      }

    case Done(_, _, _) => // message that loop is finished
      val state = loopers(sender)
      import state._
      if (running) sender ! Continue(steps, strictness, epsilon)
    case Pause(runner) =>
      val state = loopers(sender)
      import state._
      loopers + (runner -> State(false, steps, strictness, epsilon))
    case Resume(runner) =>
      val state = loopers(sender)
      import state._
      loopers + (runner -> State(true, steps, strictness, epsilon))
      runner ! Continue(steps, strictness, epsilon)

    case SetParam(runner, steps, strictness, epsilon) =>
      val running = loopers(runner).running
      loopers + (runner -> State(running, steps, strictness, epsilon))

    case SetSteps(runner, newSteps) =>
      val state = loopers(runner)
      import state._
      loopers + (runner -> State(running, newSteps, strictness, epsilon))

    case SetStrictness(runner, newStrictness) =>
      val state = loopers(runner)
      import state._
      loopers + (runner -> State(running, steps, newStrictness, epsilon))

    case SetEpsilon(runner, newEpsilon) =>
      val state = loopers(runner)
      import state._
      loopers + (runner -> State(running, steps, strictness, newEpsilon))

    case Loopers =>
      sender ! names

    case States =>
      sender ! states
  }
}

object FDHub {
  case object Loopers

  case object States

  case class Done(steps: Int, strictness: Double = 1.0, epsilon: Double = 1.0)

  case class Start(runner: ActorRef,
                   steps: Int,
                   strictness: Double = 1.0,
                   epsilon: Double = 1.0)

  case class StartAll(
      steps: Int, strictness: Double = 1.0, epsilon: Double = 1.0)

  case class State(running: Boolean,
                   steps: Int,
                   strictness: Double = 1.0,
                   epsilon: Double = 1.0)

  case class SetParam(runner: ActorRef,
                      steps: Int,
                      strictness: Double = 1.0,
                      epsilon: Double = 1.0)

  case class SetSteps(runner: ActorRef, steps: Int)

  case class SetStrictness(runner: ActorRef, steps: Double)

  case class SetEpsilon(runner: ActorRef, steps: Double)

  case class Pause(runner: ActorRef)

  case object StopHub

  case class Stopping(ref: ActorRef)

  case class Resume(runner: ActorRef)

  def props: Props = Props[FDHub]

  import Hub.system

  def startHub(name: String = "FiniteDistribution-Hub") =
    system.actorOf(props, name)

  def stopHub(implicit hub: ActorRef) = hub ! StopHub

  def stop(implicit hub: ActorRef) = {
    stopHub
  }

  implicit val timeout = Timeout(5.seconds)

  import system.dispatcher

  def loopers(implicit hub: ActorRef) =
    (hub ? Loopers).mapTo[List[String]]

  def states(implicit hub: ActorRef) =
    (hub ? States).mapTo[Map[String, State]]

  def start(runner: ActorRef,
            steps: Int = 3,
            strictness: Double = 1,
            epsilon: Double = 0.1)(implicit hub: ActorRef) =
    hub ! Start(
        runner: ActorRef, steps: Int, strictness: Double, epsilon: Double)

  def pause(runner: ActorRef)(implicit hub: ActorRef) = hub ! Pause(runner)

  def resume(runner: ActorRef)(implicit hub: ActorRef) = hub ! Resume(runner)

  def setParam(
      runner: ActorRef, steps: Int, strictness: Double, epsilon: Double)(
      implicit hub: ActorRef) =
    hub ! SetParam(
        runner: ActorRef, steps: Int, strictness: Double, epsilon: Double)

  def setSteps(runner: ActorRef, steps: Int)(implicit hub: ActorRef) =
    hub ! SetSteps(runner: ActorRef, steps: Int)

  def setStrictness(runner: ActorRef, strictness: Double)(
      implicit hub: ActorRef) =
    hub ! SetStrictness(runner: ActorRef, strictness)

  def setEpsilon(runner: ActorRef, epsilon: Double)(implicit hub: ActorRef) =
    hub ! SetEpsilon(runner: ActorRef, epsilon)
}
