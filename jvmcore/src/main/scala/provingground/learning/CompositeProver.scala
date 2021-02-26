package provingground.learning

import provingground._
import HoTT._
import monix.eval.Task

import cats._, cats.implicits._
import scala.util.Failure
import scala.util.Success
import cats.instances.`package`.parallel
import scala.collection.mutable

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

    def setParallel(parallel: Boolean): Prover

    val successTerms: Task[Set[Term]]
  }

  case class Elementary(
      lp: LocalProverStep,
      getData: LocalProverStep => Task[D],
      isSuccess: D => Task[Boolean]
  ) extends Prover {
    lazy val result =
      getData(lp).flatMap { d =>
        isSuccess(d).map {
          case true  => Proved(this, d)
          case false => Unknown(d, Set())
        }
      }

    def lpModify(fn: LocalProverStep => LocalProverStep): Prover =
      Elementary(fn(lp), getData, isSuccess)

    def setParallel(parallel: Boolean): Prover = this

    val successTerms: Task[Set[HoTT.Term]] = lp.nextState.map(_.successTerms)
  }

  case class BothOf(
      first: Prover,
      second: Prover,
      zipProofs: (Term, Term) => Option[Term]
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
      BothOf(first.lpModify(fn), second.lpModify(fn), zipProofs)

    def setParallel(parallel: Boolean): Prover =
      BothOf(
        first.setParallel(parallel),
        second.setParallel(parallel),
        zipProofs
      )

    val successTerms: Task[Set[HoTT.Term]] =
      for {
        s1 <- first.successTerms
        s2 <- second.successTerms
      } yield
        for {
          x <- s1
          y <- s2
          z <- zipProofs(x, y)
        } yield z
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

    def setParallel(parallel: Boolean): Prover =
      OneOf(first.setParallel(parallel), second.setParallel(parallel))

    val successTerms: Task[Set[HoTT.Term]] = for {
      s1 <- first.successTerms
      s2 <- second.successTerms
    } yield s1 union s2
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

    def setParallel(parallel: Boolean): Prover =
      Xor(hyp.setParallel(parallel), contra.setParallel(parallel))

    val successTerms: Task[Set[HoTT.Term]] = hyp.successTerms

  }

  case class MapProof(prover: Prover, proofMap: Term => Option[Term])
      extends Prover {
    def lpModify(fn: LocalProverStep => LocalProverStep): Prover =
      MapProof(prover.lpModify(fn), proofMap)

    val result: Task[Result] = prover.result

    val successTerms: Task[Set[HoTT.Term]] =
      prover.successTerms.map(s => s.map(proofMap).flatten)

    def setParallel(parallel: Boolean): Prover =
      MapProof(prover.setParallel(parallel), proofMap)
  }

  def sequenceResult(
      provers: Vector[Prover],
      accum: D,
      partials: Set[Result]
  ): Task[Result] =
    provers match {
      case Vector() =>
        Task(Unknown(accum, partials))
      case head +: tail =>
        head.result.flatMap {
          case Proved(res, data)       => Task(Proved(res, data))
          case Contradicted(res, data) => Task(Contradicted(res, data))
          case Unknown(data, ps) =>
            val d = accum |+| data
            sequenceResult(tail, d, partials union ps)
        }
    }

  case class AnyOf(provers: Vector[Prover], parallel: Boolean) extends Prover {
    def lpModify(fn: LocalProverStep => LocalProverStep): Prover =
      AnyOf(provers.map(_.lpModify(fn)), parallel)

    val result: Task[Result] =
      if (parallel)
        Task.parSequence(provers.map(_.result)).map(mergeResults(_)).memoize
      else sequenceResult(provers, empty, Set()).memoize

    def setParallel(pl: Boolean): Prover =
      AnyOf(provers.map(_.setParallel(pl)), pl)

    val successTerms: Task[Set[HoTT.Term]] =
      Task
        .parSequence(provers.toSet.map((p: Prover) => p.successTerms))
        .map(_.flatten)
        .memoize
  }

  def combineResults(x: Result, y: Result) = (x, y) match {
    case (Proved(r1, d1), Proved(r2, d2)) =>
      Proved(BothOf(r1, r2, { case (x: Term, y: Term) => None }), d1 |+| d2)
    case (Proved(r1, d1), r2)               => Proved(r1, d1 |+| r2.data)
    case (r2, Proved(r1, d1))               => Proved(r1, d1 |+| r2.data)
    case (Unknown(d1, p1), Unknown(d2, p2)) => Unknown(d1 |+| d2, p1 union (p2))
    case (Unknown(d1, p1), r2)              => Unknown(d1 |+| r2.data, p1)
    case (r1, Unknown(d2, p2))              => Unknown(r1.data |+| d2, p2)
    case (r1, r2)                           => Unknown(r1.data |+| r2.data, Set())
  }

  def mergeResults(res: Iterable[Result]) =
    res.fold(Unknown(empty, Set()))(combineResults(_, _))

  case class SomeOf(provers: Vector[Prover]) extends Prover {
    def lpModify(fn: LocalProverStep => LocalProverStep): Prover =
      SomeOf(provers.map(_.lpModify(fn)))

    val result: Task[Result] =
      Task.parSequence(provers.map(_.result)).map(mergeResults(_)).memoize

    def setParallel(pl: Boolean): Prover =
      SomeOf(provers.map(_.setParallel(pl)))

    val successTerms: Task[Set[HoTT.Term]] =
      Task
        .parSequence(provers.toSet.map((p: Prover) => p.successTerms))
        .map(_.flatten)
        .memoize
  }

  def proveSome(
      provers: Vector[Prover],
      results: Vector[Result],
      data: D,
      useData: D => LocalProverStep => LocalProverStep
  ): Task[(Vector[Result], D)] =
    provers match {
      case Vector() => Task.now((results, data))
      case head +: tail =>
        val prover = head.lpModify(useData(data))
        prover.result flatMap { res =>
          val newData = res.data |+| data
          proveSome(tail, results :+ res, newData, useData)
        }
    }

  def consequences(result: Result): Set[Result] = result match {
    case res @ Proved(BothOf(first, second, zp), d) =>
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

  def provedProvers(results: Set[Result]): Set[Prover] =
    results collect {
      case Proved(prover, data) => prover
    }

  def contradictedProvers(results: Set[Result]): Set[Prover] =
    results collect {
      case Contradicted(prover, data) => prover
    }

  def isProved(results: Set[Result], prover: Prover): Boolean =
    provedProvers(results).contains(prover) || {
      prover match {
        case AnyOf(provers, _) => provers.exists(p => isProved(results, p))
        case BothOf(first, second, _) =>
          isProved(results, first) && isProved(results, second)
        case Elementary(lp, getData, isSuccess) => false
        case SomeOf(provers)                    => provers.exists(p => isProved(results, p))
        case Xor(hyp, contra) =>
          isProved(results, hyp) || isContradicted(results, contra)
        case OneOf(first, second) =>
          isProved(results, first) || isProved(results, second)
        case MapProof(prover, proofMap) => isProved(results, prover)
      }
    }

  def isContradicted(results: Set[Result], prover: Prover): Boolean =
    contradictedProvers(results).contains(prover) || {
      prover match {
        case AnyOf(provers, _) => false
        case BothOf(first, second, _) =>
          isContradicted(results, first) || isContradicted(results, second)
        case Elementary(lp, getData, isSuccess) => false
        case SomeOf(provers)                    => false
        case Xor(hyp, contra) =>
          isProved(results, contra) || isContradicted(results, hyp)
        case OneOf(first, second) =>
          isContradicted(results, first) && isContradicted(results, second)
        case MapProof(prover, proofMap) => isContradicted(results, prover)
      }
    }

  /**
    * Purge components of the result that have been contradicted.
    * */
  def purge(results: Set[Result], prover: Prover): Option[Prover] =
    if (isContradicted(results, prover)) None
    else
      prover match {
        case AnyOf(provers, p) =>
          val ps = provers.filterNot(p => isContradicted(results, p))
          if (ps.nonEmpty) Some(AnyOf(ps, p)) else None
        case BothOf(first, second, zp) =>
          val c1 = isContradicted(results, first)
          val c2 = isContradicted(results, second)
          if (c1 || c2) None else Some(BothOf(first, second, zp))
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
            case (true, true)  => None
            case (true, false) => Some(second)
            case (false, true) => Some(first)
            case (false, false) =>
              Some(BothOf(first, second, { case (t1: Term, t2: Term) => None }))
          }
        case MapProof(prover, proofMap) =>
          if (isContradicted(results, prover)) None
          else Some(MapProof(prover, proofMap))
      }

}

object TermData {
  type TermResult = (TermState, Set[EquationNode])

  implicit val sg: Monoid[TermResult] = new Monoid[TermResult] {
    def combine(x: TermResult, y: TermResult): TermResult =
      (x._1 ++ y._1, x._2 union y._2)

    val empty = (TermState.zero, Set())
  }

  def termData(lp: LocalProverStep): Task[TermResult] =
    for {
      ns <- lp.nextState
      ev <- lp.expressionEval
      ev1 = ExpressionEval.export(ev, lp.initState.vars)
    } yield
      (ns.export(lp.initState.vars), ev1.equations.flatMap(EquationOps.split(_)))

  def termSuccess(typ: Typ[Term]): TermResult => Task[Boolean] = {
    case (ts, _) => Task(ts.terms.support.exists(_.typ == typ))
  }

  import TermRandomVars.expressionMapVars

  def isleNormalizeDirect(eq: EquationNode, varWeight: Double = 0.3): EquationNode = {
    TermRandomVars.isleNormalize(eq, varWeight)
  }

  val isleNormalizeMemo : mutable.Map[EquationNode, EquationNode] = mutable.Map()

  def isleNormalize(eq: EquationNode) : EquationNode = 
    isleNormalizeMemo.getOrElse(eq ,
    {
      val result = TermRandomVars.isleNormalize(eq)
      isleNormalizeMemo.update(eq, result)
      result
    })

}

import TermData._

object TermProver extends CompositeProver[TermResult] {
  val useData: TermResult => LocalProverStep => LocalProverStep = {
    case (ts, _) => lp => lp.addLookup(ts.terms.support)
  }

  def upgradeProver(prover: Prover, result: Result): Option[Prover] = {
    val results = consequences(result)
    purge(results, prover).map(p => p.lpModify(useData(result.data)))
  }

  def xorProver(
      lp: LocalProverStep,
      typ: Typ[Term],
      instances: Typ[Term] => Task[Vector[Weighted[Term]]],
      varWeight: Double,
      parallel: Boolean
  ): Task[Xor] =
    for {
      p1 <- typProver(lp, typ, instances, varWeight, parallel)
      p2 <- typProver(lp, negate(typ), instances, varWeight, parallel)
    } yield Xor(p1, p2)

  def getProver(
      lp: LocalProver,
      typ: Typ[Term],
      flatten: Double = 2,
      parallel: Boolean = false
  ) =
    typProver(
      lp,
      typ,
      tp =>
        lp.varDist(TermRandomVars.termsWithTyp(tp))
          .map(_.pmf.map {
            case Weighted(x, p) => Weighted(x, math.pow(p, 1.0 / flatten))
          }),
      lp.tg.varWeight,
      parallel
    )

  def typProver(
      lp: LocalProverStep,
      typ: Typ[Term],
      instances: Typ[Term] => Task[Vector[Weighted[Term]]],
      varWeight: Double,
      parallel: Boolean
  ): Task[Prover] = {
    val sk = skolemize(typ)
    val proverT = sk match {
      case pt: ProdTyp[u, v] =>
        def zp(x: Term, y: Term) =
          if (x.typ == pt.first && y.typ == pt.second) Some(PairTerm(x, y))
          else None
        for {
          p1 <- typProver(lp, pt.first, instances, varWeight, parallel)
          p2 <- typProver(lp, pt.second, instances, varWeight, parallel)
        } yield BothOf(p1, p2, zp)
      case pt: PlusTyp[u, v] =>
        def map1(t: Term): Option[Term] =
          if (t.typ == pt.first) Some(pt.i(t.asInstanceOf[u])) else None
        def map2(t: Term): Option[Term] =
          if (t.typ == pt.second) Some(pt.j(t.asInstanceOf[v])) else None
        for {
          p1 <- xorProver(lp, pt.first, instances, varWeight, parallel)
          p2 <- xorProver(lp, pt.second, instances, varWeight, parallel)
        } yield OneOf(MapProof(p1, map1), MapProof(p2, map2))
      case pd: PiDefn[u, v] =>
        def viaZero(t: Term): Option[Term] =
          if (t.typ == pd.domain ->: Zero)
            Some(
              pd.variable :-> vacuous(pd.value)(
                fold(negateContra(pd.domain))(pd.variable, t)
              )
            )
          else None
        for {
          p1 <- xorProver(lp, negate(pd.domain), instances, varWeight, parallel)
          p2 <- typProver(
            lp.addVar(pd.variable, varWeight),
            pd.value,
            instances,
            varWeight,
            parallel
          )
        } yield OneOf(MapProof(p1, viaZero), p2)
      case ft: FuncTyp[u, v] =>
        val x = ft.dom.Var
        def viaZero(t: Term): Option[Term] =
          if (t.typ == negate(ft.dom))
            Some(x :-> vacuous(ft.codom)(fold(negateContra(ft.dom))(x, t)))
          else None
        for {
          p1 <- xorProver(lp, negate(ft.dom), instances, varWeight, parallel)
          p2 <- typProver(
            lp.addVar(x, varWeight),
            ft.codom,
            instances,
            varWeight,
            parallel
          )
        } yield OneOf(MapProof(p1, viaZero), p2)
      case st: SigmaTyp[u, v] =>
        val proversT = instances(st.fibers.dom).flatMap { wxs =>
          Task.parSequence(wxs.map {
            case Weighted(x, p) =>
              val target = st.fibers(x.asInstanceOf[u])
              def map(t: Term): Option[Term] =
                if (t.typ == target)
                  Some(st.paircons(x.asInstanceOf[u])(t.asInstanceOf[v]))
                else None
              xorProver(
                lp.scaleLimit(p).sharpen(p),
                target,
                instances,
                varWeight,
                parallel
              ).map(p => MapProof(p, map))
          })
        }
        proversT.map(AnyOf(_, parallel))

      case tp =>
        Task(Elementary(lp.addGoal(tp, 0.5), termData, termSuccess(tp)))

    }
    def m(t: Term) = if (t.typ == sk) Some(fromSkolemized(typ)(t)) else Some(t)
    if (typ == sk) proverT else proverT.map(prover => MapProof(prover, m))
  }

  def backwardProver(
      func: Term,
      lp: LocalProverStep,
      typ: Typ[Term],
      instances: Typ[Term] => Task[Vector[Weighted[Term]]],
      varWeight: Double,
      parallel: Boolean
  ): Task[Option[Prover]] = func.typ match {
    case ftp: FuncTyp[u, v] =>
      if (ftp.codom == typ) {
        def map(t: Term): Option[Term] =
          if (t.typ == ftp.domain) Some(fold(func)(t)) else None
        typProver(lp, ftp.dom, instances, varWeight, parallel)
          .map(MapProof(_, map))
          .map(Option(_))
      } else {
        val x = ftp.dom.Var
        def zip(a: Term, b: Term): Option[Term] =
          if (a.typ == ftp.dom && b.typ == ftp.codom) Some(b.replace(x, a))
          else None
        for {
          arg <- typProver(lp, ftp.dom, instances, varWeight, parallel)
          tailOpt <- backwardProver(
            fold(func)(x),
            lp,
            typ,
            instances,
            varWeight,
            parallel
          )
        } yield tailOpt.map(tail => BothOf(arg, tail, zip))
      }
    case pd: PiDefn[u, v] =>
      if (pd.value == typ)
        typProver(lp, pd.domain, instances, varWeight, parallel).map(Option(_))
      else {
        val proversT = instances(pd.domain).flatMap { wxs =>
          Task.parSequence(wxs.map {
            case Weighted(x, p) =>
              val target = pd.value.replace(pd.variable, x)
              backwardProver(target, lp, typ, instances, varWeight, parallel)
          })
        }
        proversT.map(
          ps =>
            if (ps.flatten.nonEmpty) Some(AnyOf(ps.flatten, parallel)) else None
        )
      }

    case _ => Task.now(None)
  }

  def bestResult(
      baseProver: Prover,
      accum: Option[Result],
      sharpness: Double,
      scale: Double,
      steps: Int
  ): Task[Option[Result]] =
    if (steps < 1) Task.now(accum)
    else {
      val prover: Prover = accum
        .flatMap(result => upgradeProver(baseProver, result))
        .getOrElse(baseProver)
        .sharpen(sharpness)
      prover.result.materialize.flatMap {
        case Failure(exception) => Task.now(accum)
        case Success(value) =>
          val result = accum.map(r => value.addData(r.data)).getOrElse(value)
          result match {
            case _: Proved       => Task.now(Some(result))
            case _: Contradicted => Task.now(Some(result))
            case _ =>
              bestResult(
                baseProver,
                Some(result),
                sharpness * scale,
                scale,
                steps - 1
              )
          }

      }
    }

}
