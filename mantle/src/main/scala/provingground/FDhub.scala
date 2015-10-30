package provingground

import akka.actor._
import akka.pattern.ask
import scala.concurrent.duration._
import akka.util.Timeout

import FDactor._

import FDhub._



class FDhub extends Actor {
  var runners: Map[ActorRef, State] =Map.empty

  def names = (for ((r, x) <- runners) yield r.path.name).toList

  def receive = {
    case Start(runner: ActorRef, steps: Int, strictness : Double, epsilon: Double) =>
      {
        runners = runners + (runner -> State(true, steps, strictness, epsilon))
        println(runner)
        println(runners)
        runner ! Continue(steps, strictness, epsilon)
      }

    case StopAll =>{
      for (runner <- runners.keys) runner ! PoisonPill
      runners = Map.empty
    }


    case Done(_, _, _) => {
      println(runners)
      println(sender)
      val state = runners(sender)
      import state._
      if (running) sender ! Continue(steps, strictness, epsilon)
    }
    case Pause(runner) =>
      {
      val state = runners(sender)
      import state._
      runners + (runner -> State(false, steps, strictness, epsilon))
    }
    case Resume(runner) =>
      {
      val state = runners(sender)
      import state._
      runners + (runner -> State(true, steps, strictness, epsilon))
      runner ! Continue(steps, strictness, epsilon)
    }
    case SetParam(runner, steps, strictness, epsilon) =>{
      val running = runners(runner).running
      runners + (runner -> State(running, steps, strictness, epsilon))
    }

    case SetSteps(runner, newSteps) =>{
      val state = runners(runner)
      import state._
      runners + (runner -> State(running, newSteps, strictness, epsilon))
    }

    case SetStrictness(runner, newStrictness) =>{
      val state = runners(runner)
      import state._
      runners + (runner -> State(running, steps, newStrictness, epsilon))
    }

    case SetEpsilon(runner, newEpsilon) =>{
      val state = runners(runner)
      import state._
      runners + (runner -> State(running, steps, strictness, newEpsilon))
    }

    case Runners =>
      sender ! names
  }
}

object FDhub{
  case object Runners

  def props : Props = Props[FDhub]

  import Hub.system

  def startHub = system.actorOf(props)

  def stopRunners(implicit hub: ActorRef) = hub ! StopAll

  def stop(implicit hub: ActorRef) = {
    stopRunners
    hub ! PoisonPill
  }

  implicit val timeout = Timeout(5.seconds)

  import system.dispatcher

  def runners(implicit hub: ActorRef) =
    (hub ? Runners) map (_.asInstanceOf[List[String]])

  def start(
      runner: ActorRef, steps: Int = 3, strictness : Double = 1, epsilon: Double = 0.1
      )(implicit hub: ActorRef) =
        hub ! Start(
          runner: ActorRef, steps: Int, strictness : Double, epsilon: Double)


  def pause(runner: ActorRef)(implicit hub: ActorRef) = hub ! Pause(runner)

  def resume(runner: ActorRef)(implicit hub: ActorRef) = hub ! Resume(runner)

  def setParam(
      runner: ActorRef, steps: Int, strictness : Double, epsilon: Double
      )(implicit hub: ActorRef) =
        hub ! SetParam(
          runner: ActorRef, steps: Int, strictness : Double, epsilon: Double)

  def setSteps(
      runner: ActorRef, steps: Int
      )(implicit hub: ActorRef) =
        hub ! SetSteps(
          runner: ActorRef, steps: Int)

  def setStrictness(
      runner: ActorRef, strictness: Double
      )(implicit hub: ActorRef) =
        hub ! SetStrictness(
          runner: ActorRef, strictness)

  def setEpsilon(
      runner: ActorRef, epsilon: Double
      )(implicit hub: ActorRef) =
        hub ! SetEpsilon(
          runner: ActorRef, epsilon)
}
