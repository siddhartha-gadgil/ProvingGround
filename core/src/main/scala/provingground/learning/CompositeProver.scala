package provingground.learning

import provingground._
import HoTT._
import monix.eval.Task

import cats._, cats.implicits._

class CompositeProver[D: Semigroup] {
  sealed trait Result {
    val data: D

    def flip: Result

    def addData(d: D): Result
  }

  case class Proved(data: D) extends Result {
    val flip = Contradicted(data)

    def addData(d: D): Result = Proved(d |+| data)
  }

  case class Contradicted(data: D) extends Result {
    val flip = Proved(data)

    def addData(d: D): Result = Contradicted(d |+| data)
  }

  case class Unknown(data: D) extends Result {
    val flip = this

    def addData(d: D): Result = Unknown(d |+| data)
  }

  sealed trait Prover {
    val result: Task[Result]

    def sharpen(scale: Double): Prover = lpModify(lp => lp.sharpen(scale))

    def scaleLimit(scale: Double): Prover = lpModify(lp => lp.scaleLimit(scale))

    def addVar(term: Term, weight: Double) = lpModify(_.addVar(term, weight))

    def lpModify(fn: LocalProverStep => LocalProverStep): Prover
  }

  case class Elementary(
      lp: LocalProverStep,
      getData: LocalProverStep => Task[D],
      isSuccess: D => Boolean
  ) extends Prover {
    val result =
      getData(lp).map { d =>
        if (isSuccess(d)) Proved(d) else Unknown(d)
      }

    def lpModify(fn: LocalProverStep => LocalProverStep): Prover =
      Elementary(fn(lp), getData, isSuccess)
  }

  case class BothOf(
      first: Prover,
      second: Prover
  ) extends Prover {
    val result = first.result.flatMap {
      case Proved(data)       => second.result.map(_.addData(data))
      case Contradicted(data) => Task(Contradicted(data))
      case Unknown(data)      => second.result.map(_.addData(data))
    }

    def lpModify(fn: LocalProverStep => LocalProverStep): Prover =
      BothOf(first.lpModify(fn), second.lpModify(fn))
  }

  case class OneOf(
      first: Prover,
      second: Prover
  ) extends Prover {
    val result = first.result.flatMap {
      case Proved(data)       => Task(Proved(data))
      case Contradicted(data) => second.result.map(_.addData(data))
      case Unknown(data)      => second.result.map(_.addData(data))
    }

    def lpModify(fn: LocalProverStep => LocalProverStep): Prover =
      OneOf(first.lpModify(fn), second.lpModify(fn))
  }

  case class Xor(hyp: Prover, contra: Prover) extends Prover {
    def lpModify(fn: LocalProverStep => LocalProverStep): Xor =
      Xor(hyp.lpModify(fn), contra.lpModify(fn))

    val result: Task[Result] = 
        hyp.result.flatMap{
            case Proved(data) => Task(Proved(data))
            case Contradicted(data) => Task(Contradicted(data))
            case Unknown(data) => contra.result.map{
                case Proved(data) => Contradicted(data)
                case Contradicted(data) => Proved(data)
                case Unknown(data) => Unknown(data)
            }
        }
  }

  def sequenceResult(provers: Vector[Prover], accum: Option[D]) : Task[Result] = provers match {
      case Vector() => Task(Unknown(accum.get)) // error if an empty vector is the argument initially
      case head +: tail => head.result.flatMap{
        case Proved(data) => Task(Proved(data))
        case Contradicted(data) => Task(Contradicted(data))
        case Unknown(data) =>
            val d = accum.map(ad => data |+| ad).getOrElse(data) 
            sequenceResult(tail, accum).map{
                case Proved(data) => Contradicted(data)
                case Contradicted(data) => Proved(data)
                case Unknown(data) => Unknown(data)
            }
      }
  }

  case class AnyOf(provers: Vector[Xor]) extends Prover{
      def lpModify(fn: LocalProverStep => LocalProverStep): Prover = AnyOf(provers.map(_.lpModify(fn)))

      val result: Task[Result] = sequenceResult(provers, None)
  }

}
