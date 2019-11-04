package provingground.learning

import provingground._
import HoTT._
import monix.eval.Task

import scala.concurrent._
import duration._

import TermRandomVars._

import GeneratorVariables._, Expression._

import EntropyAtomWeight._

import Utils.bestTask

import monix.tail.Iterant

import collection.mutable.ArrayBuffer
import _root_.shapeless.Succ
import scala.util.Success
import shapeless.FinSucc

object StrategicProvers {
  type Successes =
    Vector[(HoTT.Typ[HoTT.Term], Double, FiniteDistribution[HoTT.Term])]

  type SeekResult = (
      Successes,
      Set[EquationNode],
      Set[Term]
  )

  def formal(sc: Successes): Set[EquationNode] = {
    val termsGroups = sc.toSet
    val terms       = termsGroups.map(_._3).flatMap(_.support)
    terms.flatMap(t => DE.formalEquations(t))
  }

  val skolemize: Typ[Term] => Typ[Term] = {
    case pd: ProdTyp[u, v] => skolemize(pd.first) && skolemize(pd.second)
    case st: SigmaTyp[u, v] =>
      SigmaTyp(st.fib.variable :-> skolemize(st.fib.value))
    case pd: PiDefn[u, v] =>
      skolemize(pd.value) match {
        case pt: PlusTyp[x, y] =>
          PlusTyp(
            skolemize(PiDefn(pd.variable, pt.first)),
            skolemize(PiDefn(pd.variable, pt.second))
          )
        case st: SigmaTyp[x, y] =>
          val a    = pd.variable
          val b    = st.fib.variable
          val Q    = st.fib.value
          val beta = (pd.domain ->: st.fib.dom).Var
          SigmaTyp(beta :-> skolemize(PiDefn(a, Q.replace(b, beta))))
        case tp => PiDefn(pd.variable, tp)
      }
    case ft: FuncTyp[u, v] =>
      ft.codom match {
        case pt: ProdTyp[x, y] =>
          ProdTyp(
            skolemize(ft.dom ->: pt.first),
            skolemize(ft.dom ->: pt.second)
          )
        case st: SigmaTyp[x, y] =>
          val a    = ft.dom.Var
          val b    = st.fib.variable
          val Q    = st.fib.value
          val beta = (ft.domain ->: st.fib.dom).Var
          SigmaTyp(beta :-> skolemize(PiDefn(a, Q.replace(b, beta))))
        case typ: Typ[x] => ft.dom ->: typ
      }
    case typ => typ
  }

  var currentGoal: Option[Typ[Term]] = None

  var update: Unit => Unit = (_) => ()

  def seekGoal(
      lp: LocalProver,
      typ: Typ[Term],
      terms: Set[Term],
      scale: Double = 2,
      maxSteps: Int = 100
  ): Task[SeekResult] = {
    val base =
      if (lp.tg.solverW == 0) lp.addGoals(typ -> 0.5, negate(typ) -> 0.5)
      else lp.addGoals(typ -> 0.5, negate(typ) -> 0.5).addLookup(terms)
    currentGoal = Option(typ)
    update(())
    val tasks = (1 until (maxSteps)).toVector.map { n =>
      val prover = base.sharpen(math.pow(scale, n))
      val triple = for {
        sc  <- prover.successes
        eqs <- prover.equationNodes
        // termSet <- prover.expressionEval.map(_.finalTermSet)
        finalTerms <- prover.nextState.map(_.terms.support)
      } yield (sc, eqs union formal(sc), finalTerms)
      triple
    }

    bestTask[SeekResult](tasks, p => p._1.nonEmpty)
  }.map(_.getOrElse((Vector(), Set(), Set())))

  val successes: ArrayBuffer[Successes] = ArrayBuffer()

  val failures: ArrayBuffer[Typ[Term]] = ArrayBuffer()

  var termSet: Set[Term] = Set()

  var equationNodes: Set[EquationNode] = Set()

  def md =
    s"""## Goal chomping status
        |
        | * current goal : $currentGoal
        | * negated current goal: ${currentGoal.map(negate)}
        | * successes : ${successes.size}
        | * failures : ${failures.size}
        | * terms : ${termSet.size}
        | * equation-nodes: ${equationNodes.size}
        | * last success : ${successes.headOption}
        | 
        | ${failures.reverse.mkString("### Failures\n\n * ", "\n * ", "\n")}
        |""".stripMargin

  // chomps goals till failure or if none are left
  def goalChomper(
      lp: LocalProver,
      typs: Vector[Typ[Term]],
      accumSucc: Vector[Successes] = Vector(),
      accumEqs: Set[EquationNode] = Set(),
      accumTerms: Set[Term] = Set(),
      scale: Double = 2,
      maxSteps: Int = 100
  ): Task[
    (Vector[Successes], Set[EquationNode], Set[Term], Vector[Typ[Term]])
  ] =
    typs match {
      case Vector() => Task.now((accumSucc, accumEqs, accumTerms, Vector()))
      case typ +: ys =>
        seekGoal(lp, typ, Set(), scale, maxSteps).flatMap {
          case (ss, eqs, terms) =>
            if (ss.isEmpty)
              Task.now(
                (
                  accumSucc :+ ss,
                  accumEqs union eqs,
                  accumTerms union terms,
                  typ +: ys
                )
              )
            else {
              successes.append(ss)
              update(())
              goalChomper(
                lp,
                ys,
                accumSucc :+ ss,
                accumEqs union eqs,
                accumTerms union terms,
                scale,
                maxSteps
              )
            }
        }
    }

  def liberalChomper(
      lp: LocalProver,
      typs: Vector[Typ[Term]],
      accumSucc: Vector[Successes] = Vector(),
      accumFail: Vector[Typ[Term]] = Vector(),
      accumEqs: Set[EquationNode] = Set(),
      accumTerms: Set[Term] = Set(),
      scale: Double = 2,
      maxSteps: Int = 100
  ): Task[
    (
        Vector[Successes],
        Vector[Typ[Term]],
        Set[EquationNode],
        Set[Term],
        Vector[Typ[Term]]
    )
  ] =
    typs match {
      case Vector() =>
        Task.now((accumSucc, accumFail, accumEqs, accumTerms, Vector()))
      case typ +: ys =>
        seekGoal(lp, typ, accumTerms, scale, maxSteps).flatMap {
          case (ss, eqs, terms) =>
            equationNodes = equationNodes union (eqs)
            termSet = termSet union (terms)
            if (ss.isEmpty) {
              failures.append(typ)
              update(())
              liberalChomper(
                lp,
                ys,
                accumSucc,
                accumFail :+ typ,
                accumEqs union eqs,
                accumTerms union terms,
                scale,
                maxSteps
              )
            } else {
              successes.append(ss)
              update(())
              liberalChomper(
                lp,
                ys,
                accumSucc :+ ss,
                accumFail,
                accumEqs union eqs,
                accumTerms union terms,
                scale,
                maxSteps
              )
            }
        }
    }
}

sealed trait GlobalProver[R] {
  val result: Task[R]

  def sharpen(scale: Double): GlobalProver[R]

  def scaleLimit(scale: Double): GlobalProver[R]
}

object GlobalProver {
  case class Elementary[R](
      lp: LocalProverStep,
      getResult: LocalProverStep => Task[R]
  ) extends GlobalProver[R] {
    val result = getResult(lp)

    def sharpen(scale: Double): GlobalProver[R] =
      Elementary(lp.sharpen(scale), getResult)

    def scaleLimit(scale: Double): GlobalProver[R] =
      Elementary(lp.scaleLimit(scale), getResult)
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

    def scaleLimit(scale: Double): GlobalProver[R] =
      BothOf(first.scaleLimit(scale), second.scaleLimit(scale), combine)

    def sharpen(scale: Double): GlobalProver[R] =
      BothOf(first.sharpen(scale), second.sharpen(scale), combine)
  }

  case class OneOf[R](
      first: GlobalProver[R],
      second: GlobalProver[R],
      firstNegated: GlobalProver[R],
      secondNegated: GlobalProver[R],
      isSuccess: R => Boolean
  ) extends GlobalProver[R] {
    def scaleLimit(scale: Double): GlobalProver[R] =
      OneOf(
        first.scaleLimit(scale),
        second.scaleLimit(scale),
        firstNegated.scaleLimit(scale),
        secondNegated.scaleLimit(scale),
        isSuccess
      )

    def sharpen(scale: Double): GlobalProver[R] =
      OneOf(
        first.sharpen(scale),
        second.sharpen(scale),
        firstNegated.sharpen(scale),
        secondNegated.sharpen(scale),
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
    def scaleLimit(scale: Double): Xor[R] =
      Xor(
        first.scaleLimit(scale),
        second.scaleLimit(scale),
        isSuccess
      )

    def sharpen(scale: Double): Xor[R] =
      Xor(
        first.sharpen(scale),
        second.sharpen(scale),
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
    def sharpen(scale: Double): GlobalProver[R] =
      AllOf(provers.map(_.sharpen(scale)), combine)

    def scaleLimit(scale: Double): GlobalProver[R] =
      AllOf(provers.map(_.scaleLimit(scale)), combine)

    lazy val result = Task.gather(provers.map(_.result)).map(combine)
  }

  case class AnyOf[R](provers: Vector[Xor[R]])
      extends GlobalProver[Either[R, R]] {
    def scaleLimit(scale: Double): GlobalProver[Either[R, R]] =
      AnyOf(provers.map(_.scaleLimit(scale)))

    def sharpen(scale: Double): GlobalProver[Either[R, R]] =
      AnyOf(provers.map(_.sharpen(scale)))

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
    def sharpen(scale: Double): GlobalProver[Vector[R]] =
      SomeOf(provers.map(_.sharpen(scale)), isSuccess)

    def scaleLimit(scale: Double): GlobalProver[Vector[R]] =
      SomeOf(provers.map(_.scaleLimit(scale)), isSuccess)

    // Note: this is just the default implementation
    lazy val result : Task[Vector[R]] =  
      Task.gather(
        provers.map(p => p.result.materialize.map(_.toOption))
        ).map(v => v.flatten)
  }
}
