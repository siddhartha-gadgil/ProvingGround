package provingground.learning

import provingground._
import HoTT._
import monix.eval._, monix.catnap._, monix.tail._

import IterantRunner._

import monix.execution.Scheduler.Implicits.global

import monix.execution.AsyncQueue

trait IterantRunner[S, A, M] {
  val evolve: S => Task[(A, S)]

  def onCommand(state: S, c: M): S // for now, incomplete tasks are cancelled

  def unsafeIterant(init: S): Iterant[Task, A] =
    Iterant.fromStateActionL(evolve)(Task.pure(init))

  val requestQueue = AsyncQueue.unbounded[Request[M]]()

  def pollTask: Task[Request[M]] = Task.deferFuture(requestQueue.poll())

  def halt() = requestQueue.offer(Halt())

  def command(c: M) = requestQueue.offer(Command(c))

  def spawn: State[S, M] => Task[(Output[A, M], State[S, M])] = {
    case Running(state, pt) =>
      val raceTask: Task[
        Either[((A, S), Fiber[Request[M]]), (Fiber[(A, S)], Request[M])]
      ] =
        Task.racePair(evolve(state), pt)
      raceTask map {
        case Left(((a, s), fr)) =>
          (Result(a), Running(s, fr.join))
        case Right((fas, req @ Command(c))) =>
          fas.cancel
          (Ack(req), Running(onCommand(state, c), pollTask))
        case Right((fas, Halt())) =>
          fas.cancel
          ((Ack(Halt()), Halted(state)))
      }
    case Halted(_) => Task.never
  }

  def iterant(init: Task[S]) : Iterant[Task, Output[A,M]] = {
      val initState : Task[State[S, M]] = init.map{s => Running(s, pollTask)}
      val base = Iterant.fromStateActionL(spawn)(initState)
      base.takeWhile(running)
  }


}

object IterantRunner {
  sealed trait Request[M]

  case class Command[M](command: M) extends Request[M]

  case class Halt[M]() extends Request[M]

  sealed trait Output[A, M]

  case class Result[A, M](result: A) extends Output[A, M]

  case class Ack[A, M](req: Request[M]) extends Output[A, M]

  sealed trait State[S, M]

  case class Running[S, M](state: S, pollTask: Task[Request[M]])
      extends State[S, M]

  case class Halted[S, M](state: S) extends State[S, M]

  def running[S, M] : Output[S, M] => Boolean = {
      case Ack(Halt()) => false
      case _ => true
  }
}
