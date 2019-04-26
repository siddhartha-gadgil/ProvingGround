package provingground.learning

import provingground._
import HoTT._
import monix.eval._, monix.catnap._, monix.tail._

import IterantRunner._

import monix.execution.Scheduler.Implicits.global

trait IterantRunner[S, A, M]{
    val evolve: S => Task[(A, S)]
    
    def unsafeIterant(init: Task[S]) : Iterant[Task,A] = Iterant.fromStateActionL(evolve)(init)

    import monix.execution.AsyncQueue
    
    val requestQueue = AsyncQueue.unbounded[Request[M]]()

    def pollTask = Task.deferFuture(requestQueue.poll())

    def halt() = requestQueue.offer(Halt())

    def command(c: M) = requestQueue.offer(Command(c))
}

object IterantRunner{
    sealed trait Request[M]

    case class Command[M](command : M) extends Request[M]

    case class Halt[M]() extends Request[M]
}