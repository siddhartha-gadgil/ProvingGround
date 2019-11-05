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

  case class Proved(prover: Prover, data: D) extends Result {
    val flip = Contradicted(prover, data)

    def addData(d: D): Result = Proved(prover, d |+| data)
  }

  case class Contradicted(prover: Prover, data: D) extends Result {
    val flip = Proved(prover, data)

    def addData(d: D): Result = Contradicted(prover, d |+| data)
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
    lazy val result =
      getData(lp).map { d =>
        if (isSuccess(d)) Proved(this, d) else Unknown(d)
      }

    def lpModify(fn: LocalProverStep => LocalProverStep): Prover =
      Elementary(fn(lp), getData, isSuccess)
  }

  case class BothOf(
      first: Prover,
      second: Prover
  ) extends Prover {
    val result = first.result.flatMap {
      case Proved(_, data)         => second.result.map(_.addData(data))
      case Contradicted(res, data) => Task(Contradicted(res, data))
      case Unknown(data)           => second.result.map(_.addData(data))
    }

    def lpModify(fn: LocalProverStep => LocalProverStep): Prover =
      BothOf(first.lpModify(fn), second.lpModify(fn))
  }

  case class OneOf(
      first: Prover,
      second: Prover
  ) extends Prover {
    val result = first.result.flatMap {
      case Proved(res, data)     => Task(Proved(res, data))
      case Contradicted(_, data) => second.result.map(_.addData(data))
      case Unknown(data)         => second.result.map(_.addData(data))
    }

    def lpModify(fn: LocalProverStep => LocalProverStep): Prover =
      OneOf(first.lpModify(fn), second.lpModify(fn))
  }

  case class Xor(hyp: Prover, contra: Prover) extends Prover {
    def lpModify(fn: LocalProverStep => LocalProverStep): Xor =
      Xor(hyp.lpModify(fn), contra.lpModify(fn))

    val result: Task[Result] =
      hyp.result.flatMap {
        case Proved(_, data)       => Task(Proved(hyp, data))
        case Contradicted(_, data) => Task(Contradicted(hyp, data))
        case Unknown(data) =>
          contra.result.map {
            case Proved(_, data)       => Contradicted(hyp, data)
            case Contradicted(_, data) => Proved(hyp, data)
            case Unknown(data)         => Unknown(data)
          }
      }
  }

  def sequenceResult(provers: Vector[Prover], accum: Option[D]): Task[Result] =
    provers match {
      case Vector() =>
        Task(Unknown(accum.get)) // error if an empty vector is the argument initially
      case head +: tail =>
        head.result.flatMap {
          case Proved(res, data)       => Task(Proved(res, data))
          case Contradicted(res, data) => Task(Contradicted(res, data))
          case Unknown(data) =>
            val d = accum.map(ad => data |+| ad).getOrElse(data)
            sequenceResult(tail, accum)
        }
    }

  case class AnyOf(provers: Vector[Xor]) extends Prover {
    def lpModify(fn: LocalProverStep => LocalProverStep): Prover =
      AnyOf(provers.map(_.lpModify(fn)))

    val result: Task[Result] = sequenceResult(provers, None)
  }

  case class SomeOf(provers: Vector[Prover]) extends Prover {
    def lpModify(fn: LocalProverStep => LocalProverStep): Prover =
      SomeOf(provers.map(_.lpModify(fn)))

    val result: Task[Result] = sequenceResult(provers, None)
  }

  def isProved(result: Result, prover: Prover): Boolean =
    result match {
      case Proved(p, data) if prover == p => true
      case _ =>
        prover match {
          case AnyOf(provers) => provers.exists(p => isProved(result, p))
          case BothOf(first, second) =>
            isProved(result, first) && isProved(result, second)
          case Elementary(lp, getData, isSuccess) => false
          case SomeOf(provers)                    => provers.exists(p => isProved(result, p))
          case Xor(hyp, contra) =>
            isProved(result, hyp) || isContradicted(result, contra)
          case OneOf(first, second) =>
            isProved(result, first) || isProved(result, second)
        }
    }

  def isContradicted(result: Result, prover: Prover): Boolean =
    result match {
      case Contradicted(p, data) if prover == p => true
      case _ =>
        prover match {
          case AnyOf(provers) => false
          case BothOf(first, second) =>
            isContradicted(result, first) || isContradicted(result, second)
          case Elementary(lp, getData, isSuccess) => false
          case SomeOf(provers)                    => false
          case Xor(hyp, contra) =>
            isProved(result, contra) || isContradicted(result, hyp)
          case OneOf(first, second) =>
            isContradicted(result, first) && isContradicted(result, second)
        }
    }

  /**
    * Purge components of the result that have been contradicted.
    * */
  def purge(result: Result, prover: Prover): Option[Prover] =
    if (isContradicted(result, prover)) None
    else
      prover match {
        case AnyOf(provers) =>
          val ps = provers.filterNot(p => isContradicted(result, p))
          if (ps.nonEmpty) Some(AnyOf(ps)) else None
        case BothOf(first, second) =>
          val c1 = isContradicted(result, first)
          val c2 = isContradicted(result, second)
          if (c1 || c2) None else Some(BothOf(first, second))
        case el @ Elementary(lp, getData, isSuccess) => Some(el)
        case SomeOf(provers) =>
          val ps = provers.filterNot(p => isContradicted(result, p))
          if (ps.nonEmpty) Some(SomeOf(ps)) else None
        case x @ Xor(hyp, contra) =>
          val c1 = isContradicted(result, hyp)
          val c2 = isProved(result, contra)
          if (c1 || c2) None else Some(x)
        case OneOf(first, second) =>
          val c1 = isContradicted(result, first)
          val c2 = isContradicted(result, second)
          (c1, c2) match {
            case (true, true)   => None
            case (true, false)  => Some(second)
            case (false, true)  => Some(first)
            case (false, false) => Some(BothOf(first, second))
          }
      }

}

object TermData {
  type TermResult = (TermState, Set[EquationNode])

  implicit val sg: Semigroup[TermResult] = new Semigroup[TermResult] {
    def combine(x: TermResult, y: TermResult): TermResult =
      (x._1 ++ y._1, x._2 union y._2)
  }

  def termData(lp: LocalProverStep) =
    Task.parZip2(lp.nextState, lp.equationNodes)

  def termSuccess(typ: Typ[Term]): TermResult => Boolean = {
    case (ts, _) => ts.terms.support.exists(_.typ == typ)
  }

}

import TermData._

object TermProver extends CompositeProver[TermResult] {
  def xorProver(
      lp: LocalProverStep,
      typ: Typ[Term],
      instances: Typ[Term] => Task[Vector[Weighted[Term]]],
      varWeight: Double
  ): Task[Xor] =
    for {
      p1 <- typProver(lp, typ, instances, varWeight)
      p2 <- typProver(lp, negate(typ), instances, varWeight)
    } yield Xor(p1, p2)

  def getProver(lp: LocalProver, typ: Typ[Term], flatten: Double = 2) =
    typProver(
      lp,
      typ,
      tp =>
        lp.varDist(TermGeneratorNodes.termsWithTyp(tp))
          .map(_.pmf.map {
            case Weighted(x, p) => Weighted(x, math.pow(p, 1.0 / flatten))
          }),
      lp.tg.varWeight
    )

  def typProver(
      lp: LocalProverStep,
      typ: Typ[Term],
      instances: Typ[Term] => Task[Vector[Weighted[Term]]],
      varWeight: Double
  ): Task[Prover] =
    skolemize(typ) match {
      case pt: ProdTyp[u, v] =>
        for {
          p1 <- typProver(lp, pt.first, instances, varWeight)
          p2 <- typProver(lp, pt.second, instances, varWeight)
        } yield BothOf(p1, p2)
      case pt: PlusTyp[u, v] =>
        for {
          p1 <- xorProver(lp, pt.first, instances, varWeight)
          p2 <- xorProver(lp, pt.second, instances, varWeight)
        } yield OneOf(p1, p2)
      case pd: PiDefn[u, v] =>
        for {
          p1 <- xorProver(lp, negate(pd.domain), instances, varWeight)
          p2 <- typProver(
            lp.addVar(pd.variable, varWeight),
            pd.value,
            instances,
            varWeight
          )
        } yield OneOf(p1, p2)
      case ft: FuncTyp[u, v] =>
        val x = ft.dom.Var
        for {
          p1 <- xorProver(lp, negate(ft.dom), instances, varWeight)
          p2 <- typProver(
            lp.addVar(x, varWeight),
            ft.codom,
            instances,
            varWeight
          )
        } yield OneOf(p1, p2)
      case st: SigmaTyp[u, v] =>
        val l = funcToLambdaFixed(st.fib)
        val proversT = instances(st.fibers.dom).flatMap { wxs =>
          Task.gather(wxs.map {
            case Weighted(x, p) =>
              xorProver(
                lp.scaleLimit(p).sharpen(p),
                st.fibers(x.asInstanceOf[u]),
                instances,
                varWeight
              )
          })
        }
        proversT.map(AnyOf(_))

      case tp =>
        Task(Elementary(lp.addGoal(tp, 0.5), termData, termSuccess(tp)))

    }

}
