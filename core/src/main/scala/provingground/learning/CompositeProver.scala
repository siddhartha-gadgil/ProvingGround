package provingground.learning

import provingground._
import HoTT._
import monix.eval.Task

import cats._, cats.implicits._

class CompositeProver[D: Monoid] {
  val empty = implicitly[Monoid[D]].empty

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

  case class Unknown(data: D, partials: Set[Result]) extends Result {
    val flip = this

    def addData(d: D): Result = Unknown(d |+| data, partials)
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
        if (isSuccess(d)) Proved(this, d) else Unknown(d, Set())
      }

    def lpModify(fn: LocalProverStep => LocalProverStep): Prover =
      Elementary(fn(lp), getData, isSuccess)
  }

  case class BothOf(
      first: Prover,
      second: Prover
  ) extends Prover {
    val result = Task.parMap2(first.result, second.result) {
      case (Proved(_, d1), Proved(_, d2))     => Proved(this, d1 |+| d2)
      case (Contradicted(_, d1), r)           => Contradicted(this, d1 |+| r.data)
      case (r, Contradicted(_, d1))           => Contradicted(this, d1 |+| r.data)
      case (Unknown(d1, p1), Unknown(d2, p2)) => Unknown(d1 |+| d2, p1 union p2)
      case (Unknown(d1, p1), r)               => Unknown(d1 |+| r.data, p1 + r)
      case (r, Unknown(d1, p1))               => Unknown(d1 |+| r.data, p1 + r)
    }

    def lpModify(fn: LocalProverStep => LocalProverStep): Prover =
      BothOf(first.lpModify(fn), second.lpModify(fn))
  }

  case class OneOf(
      first: Prover,
      second: Prover
  ) extends Prover {

    val result = Task.parMap2(first.result, second.result) {
      case (Contradicted(_, d1), Contradicted(_, d2)) =>
        Contradicted(this, d1 |+| d2)
      case (Proved(_, d1), r)                 => Proved(this, d1 |+| r.data)
      case (r, Proved(_, d1))                 => Proved(this, d1 |+| r.data)
      case (Unknown(d1, p1), Unknown(d2, p2)) => Unknown(d1 |+| d2, p1 union p2)
      case (Unknown(d1, p1), r)               => Unknown(d1 |+| r.data, p1 + r)
      case (r, Unknown(d1, p1))               => Unknown(d1 |+| r.data, p1 + r)
    }

    def lpModify(fn: LocalProverStep => LocalProverStep): Prover =
      OneOf(first.lpModify(fn), second.lpModify(fn))
  }

  case class Xor(hyp: Prover, contra: Prover) extends Prover {
    def lpModify(fn: LocalProverStep => LocalProverStep): Xor =
      Xor(hyp.lpModify(fn), contra.lpModify(fn))

    val result: Task[Result] =
      Task.parMap2(hyp.result, contra.result) {
        case (Proved(_, d1), r)       => Proved(this, d1 |+| r.data)
        case (Contradicted(_, d1), r) => Contradicted(this, d1 |+| r.data)
        case (r, Contradicted(_, d1)) => Proved(this, d1 |+| r.data)
        case (r, Proved(_, d1))       => Contradicted(this, d1 |+| r.data)
        case (Unknown(d1, p1), Unknown(d2, p2)) =>
          Unknown(d1 |+| d2, p1 union p2)
      }
  }

  def sequenceResult(
      provers: Vector[Prover],
      accum: D,
      partials: Set[Result]
  ): Task[Result] =
    provers match {
      case Vector() =>
        Task(Unknown(accum, partials)) // error if an empty vector is the argument initially
      case head +: tail =>
        head.result.flatMap {
          case Proved(res, data)       => Task(Proved(res, data))
          case Contradicted(res, data) => Task(Contradicted(res, data))
          case Unknown(data, ps) =>
            val d = accum |+| data
            sequenceResult(tail, d, partials union ps)
        }
    }

  case class AnyOf(provers: Vector[Xor]) extends Prover {
    def lpModify(fn: LocalProverStep => LocalProverStep): Prover =
      AnyOf(provers.map(_.lpModify(fn)))

    val result: Task[Result] = sequenceResult(provers, empty, Set())
  }

  // Don's use this, use the `proveSome` method instead
  case class SomeOf(provers: Vector[Prover]) extends Prover {
    def lpModify(fn: LocalProverStep => LocalProverStep): Prover =
      SomeOf(provers.map(_.lpModify(fn)))

    val result: Task[Result] = sequenceResult(provers, empty, Set())
  }

  def proveSome(
      provers: Vector[Prover],
      results: Vector[Result],
      data: D,
      useData: D => LocalProverStep => LocalProverStep
  ): Task[(Vector[Result], D)] =
    provers match {
      case Vector()     => Task.now((results, data))
      case head +: tail =>
        val prover = head.lpModify(useData(data)) 
        prover.result flatMap {
          res =>
            val newData = res.data |+| data 
            proveSome(tail, results :+ res, newData, useData)
        }
    }

  def consequences(result: Result): Set[Result] = result match {
    case res @ Proved(BothOf(first, second), d) =>
      consequences(Proved(first, d))
        .union(consequences(Proved(second, d))) + res
    case res @ Contradicted(OneOf(first, second), d) =>
      consequences(Contradicted(first, d))
        .union(consequences(Contradicted(second, d))) + res
    case res @ Proved(Xor(first, second), d) =>
      consequences(Proved(first, d))
        .union(consequences(Contradicted(second, d))) + res
    case res @ Contradicted(Xor(first, second), d) =>
      consequences(Contradicted(first, d))
        .union(consequences(Proved(second, d))) + res
    case res => Set(res)
  }

  def provedProvers(results: Set[Result]) : Set[Prover] = 
    results collect{
      case Proved(prover, data) => prover 
    }

  def contradictedProvers(results: Set[Result]) : Set[Prover] = 
    results collect{
      case Contradicted(prover, data) => prover 
    }

  def isProved(results: Set[Result], prover: Prover): Boolean =
    provedProvers(results).contains(prover) ||
        {prover match {
          case AnyOf(provers) => provers.exists(p => isProved(results, p))
          case BothOf(first, second) =>
            isProved(results, first) && isProved(results, second)
          case Elementary(lp, getData, isSuccess) => false
          case SomeOf(provers)                    => provers.exists(p => isProved(results, p))
          case Xor(hyp, contra) =>
            isProved(results, hyp) || isContradicted(results, contra)
          case OneOf(first, second) =>
            isProved(results, first) || isProved(results, second)
        }
      }

  def isContradicted(results: Set[Result], prover: Prover): Boolean =
    contradictedProvers(results).contains(prover) ||
        {prover match {
          case AnyOf(provers) => false
          case BothOf(first, second) =>
            isContradicted(results, first) || isContradicted(results, second)
          case Elementary(lp, getData, isSuccess) => false
          case SomeOf(provers)                    => false
          case Xor(hyp, contra) =>
            isProved(results, contra) || isContradicted(results, hyp)
          case OneOf(first, second) =>
            isContradicted(results, first) && isContradicted(results, second)
        }}

  /**
    * Purge components of the result that have been contradicted.
    * */
  def purge(results: Set[Result], prover: Prover): Option[Prover] =
    if (isContradicted(results, prover)) None
    else
      prover match {
        case AnyOf(provers) =>
          val ps = provers.filterNot(p => isContradicted(results, p))
          if (ps.nonEmpty) Some(AnyOf(ps)) else None
        case BothOf(first, second) =>
          val c1 = isContradicted(results, first)
          val c2 = isContradicted(results, second)
          if (c1 || c2) None else Some(BothOf(first, second))
        case el @ Elementary(lp, getData, isSuccess) => Some(el)
        case SomeOf(provers) =>
          val ps = provers.filterNot(p => isContradicted(results, p))
          if (ps.nonEmpty) Some(SomeOf(ps)) else None
        case x @ Xor(hyp, contra) =>
          val c1 = isContradicted(results, hyp)
          val c2 = isProved(results, contra)
          if (c1 || c2) None else Some(x)
        case OneOf(first, second) =>
          val c1 = isContradicted(results, first)
          val c2 = isContradicted(results, second)
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

  implicit val sg: Monoid[TermResult] = new Monoid[TermResult] {
    def combine(x: TermResult, y: TermResult): TermResult =
      (x._1 ++ y._1, x._2 union y._2)

    val empty = (TermState.zero, Set())
  }

  def termData(lp: LocalProverStep) =
    Task.parZip2(lp.nextState, lp.equationNodes)

  def termSuccess(typ: Typ[Term]): TermResult => Boolean = {
    case (ts, _) => ts.terms.support.exists(_.typ == typ)
  }

}

import TermData._

object TermProver extends CompositeProver[TermResult] {
  val useData :TermResult => LocalProverStep => LocalProverStep = {case (ts, _) => lp => lp.addLookup(ts.terms.support)}

  def upgradeProver(prover: Prover, result: Result) : Option[Prover] = {
    val results = consequences(result)
    purge(results, prover).map(p => p.lpModify(useData(result.data)))
  }

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
