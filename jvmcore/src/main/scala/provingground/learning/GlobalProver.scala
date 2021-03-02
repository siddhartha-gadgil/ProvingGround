package provingground.learning


import provingground._
import HoTT._
import monix.eval.Task

import scala.concurrent._






import scala.util.Success

@deprecated("use composite prover", "now")
sealed trait GlobalProver[R] {
  val result: Task[R]

  def sharpen(scale: Double): GlobalProver[R] = lpModify(lp => lp.sharpen(scale))

  def scaleLimit(scale: Double): GlobalProver[R] = lpModify(lp => lp.scaleLimit(scale))

  def addVar(term: Term, weight: Double) = lpModify(_.addVar(term, weight))

  def lpModify(fn: LocalProverStep => LocalProverStep): GlobalProver[R]
}

@deprecated("use composite prover", "now")
object GlobalProver {
  case class Elementary[R](
      lp: LocalProverStep,
      getResult: LocalProverStep => Task[R]
  ) extends GlobalProver[R] {
    val result = getResult(lp)

    def lpModify(fn: LocalProverStep => LocalProverStep): GlobalProver[R] =
      Elementary(fn(lp), getResult)
  }

  case class BothOf[R](
      first: GlobalProver[R],
      second: GlobalProver[R],
      combine: (R, R) => R
  ) extends GlobalProver[R] {
    val result: Task[R] =
      for {
        r1 <- first.result
        r2 <- second.result
      } yield combine(r1, r2)

    def lpModify(fn: LocalProverStep => LocalProverStep): GlobalProver[R] =
      BothOf(first.lpModify(fn), second.lpModify(fn), combine)
  }

  case class OneOf[R](
      first: GlobalProver[R],
      second: GlobalProver[R],
      firstNegated: GlobalProver[R],
      secondNegated: GlobalProver[R],
      isSuccess: R => Boolean
  ) extends GlobalProver[R] {

    def lpModify(fn: LocalProverStep => LocalProverStep): GlobalProver[R] =
      OneOf(
        first.lpModify(fn),
        second.lpModify(fn),
        firstNegated.lpModify(fn),
        secondNegated.lpModify(fn),
        isSuccess
      )

    val result: Task[R] = first.result.flatMap { r =>
      if (isSuccess(r)) Task.now(r) else second.result
    }
  }

  case class Xor[R](
      first: GlobalProver[R],
      second: GlobalProver[R],
      isSuccess: R => Boolean
  ) extends GlobalProver[Either[R, R]] {

    def lpModify(fn: LocalProverStep => LocalProverStep): Xor[R] =
      Xor(
        first.lpModify(fn),
        second.lpModify(fn),
        isSuccess
      )

    val result: Task[Either[R, R]] = first.result.flatMap { r =>
      if (isSuccess(r)) Task.now(Right(r)) else second.result.map(Left(_))
    }.memoize

    val success: Task[Boolean] = result.map(_.isRight)
  }

  def sequenceResult[R](provers: Vector[Xor[R]]): Task[Option[R]] =
    Task.now(provers).flatMap {
      case Vector() => Task.now(None)
      case head +: tail =>
        head.result.materialize flatMap {
          case Success(Right(res)) => Task.now(Some(res))
          case _    => sequenceResult(tail)
        }
    }

  case class AllOf[R](provers: Vector[GlobalProver[R]], combine: Vector[R] => R)
      extends GlobalProver[R] {
    def lpModify(fn: LocalProverStep => LocalProverStep): GlobalProver[R] =
      AllOf(provers.map(_.lpModify(fn)), combine)
    lazy val result = Task.parSequence(provers.map(_.result)).map(combine)
  }

  case class AnyOf[R](provers: Vector[Xor[R]])
      extends GlobalProver[Either[R, R]] {
    def lpModify(fn: LocalProverStep => LocalProverStep): GlobalProver[Either[R, R]] =
      AnyOf(provers.map(_.lpModify(fn)))

    def quickResult: Task[Either[R, R]] =
      Task.raceMany(provers.map(_.result)).memoize

    val optResult: Task[Option[R]] = sequenceResult(provers)

    // Note: this is just the default implementation
    lazy val result = {
      for {
        opt     <- optResult
        default <- quickResult
      } yield opt.map(Right(_)).getOrElse(default)
    }.memoize
  }

  case class SomeOf[R](provers: Vector[GlobalProver[R]], isSuccess: R => Boolean) extends GlobalProver[Vector[R]] {
    def lpModify(fn: LocalProverStep => LocalProverStep): GlobalProver[Vector[R]] =
      SomeOf(provers.map(_.lpModify(fn)), isSuccess)

    // Note: this is just the default implementation
    lazy val result : Task[Vector[R]] =  
      Task.parSequence(
        provers.map(p => p.result.materialize.map(_.toOption))
        ).map(v => v.flatten.filter(isSuccess))
  }

  type Result = (TermState, Set[EquationNode])

  def chomper(lp: LocalProver, typ: Typ[Term], goalWeight: Double) : Task[GlobalProver[Either[Result, Result]]] =
    skolemize(typ) match {
        case tp => 
            val lpFirst = lp.addGoal(tp, goalWeight)
            val lpSecond = lp.addGoal(tp, goalWeight)
            def result(lp: LocalProverStep) = 
                for {
                    eqs <- lp.equationNodes
                    fs <- lp.nextState
                } yield (fs, eqs)
            def isSuccess : Result => Boolean = {
                case (ns, _) => ns.successes.nonEmpty
            }
             Task(Xor(Elementary(lpFirst, result(_)), Elementary(lpSecond, result(_)), isSuccess))
    }
}
