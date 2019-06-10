package provingground.interface
import provingground._, learning.{Sort => _, _}
import induction._

import monix.execution.Scheduler.Implicits.global
import monix.eval._
import translation.FansiShow._

import scala.collection.mutable.{ArrayBuffer, Map => mMap}
import HoTT.{Name => _, _}
import monix.execution.CancelableFuture

import math.max
import trepplein._

import LeanInterface._, LeanToTermMonix._, LeanParser._
import ujson.Arr

import scala.util.Try
import provingground.learning.TermGeneratorNodes

import TermRandomVars._, GeneratorVariables._, Expression._

class LeanParserEq(
    initMods: Seq[Modification],
    defTaskMap: Map[Name, Task[Term]] = Map(),
    indTaskMap: Map[Name, Task[TermIndMod]] = Map(),
    log: LeanParser.Logger = LeanParser.Logger.nop,
    tg: TermGeneratorNodes[TermState] = TermGeneratorNodes.Base
) extends LeanParser(
      initMods: Seq[Modification],
      defTaskMap: Map[Name, Task[Term]],
      indTaskMap: Map[Name, Task[TermIndMod]],
      log: LeanParser.Logger
    ) {
  def parseEq(
      exp: Expr,
      vars: Vector[Term] = Vector()
  ): Task[(Term, Set[EquationNode])] = {

    parseWork += exp
    log(ParseWork(exp))
    val resTask: Task[(Term, Set[EquationNode])] = exp match {
      case Const(name, _) =>
        getNamed(name)
          .orElse {
            defFromMod(name)
          }
          .getOrElse(
            Task.raiseError(UnParsedException(exp))
          )
          .map(_ -> ???)

      case Sort(Level.Zero) => Task.pure(Prop    -> Set())
      case Sort(_)          => Task.pure(Type    -> Set())
      case Var(n)           => Task.pure(vars(n) -> Set())
      case RecIterAp(name, args) =>
        pprint.log(s"Seeking RecIterAp $name, $args, $vars")
        pprint.log(s"${vars.headOption.map(isWitness)}")
        recApp(name, args, exp, vars).map(_ -> ???)

      case App(f, a) =>
        // pprint.log(s"Applying $f to $a")
        for {
          funcEqs <- parseEq(f, vars)
            .executeWithOptions(_.enableAutoCancelableRunLoops)
          argEqs <- parseEq(a, vars)
            .executeWithOptions(_.enableAutoCancelableRunLoops)
          res = fold(funcEqs._1)(argEqs._1)
          // _ = pprint.log(s"got result for $f($a)")
          eq = EquationNode(
            FinalVal(Elem(res, Terms)),
            Coeff(tg.applnNode, Terms) * FinalVal(
              Elem(funcEqs._1, termsWithTyp(funcEqs._1.typ))
            ) * FinalVal(Elem(argEqs._1, Terms))
          )
        } yield res -> (funcEqs._2.union(argEqs._2) + eq)
      case Lam(domain, body) =>
        // pprint.log(s"lambda $domain, $body")
        for {
          domTermEq <- parseEq(domain.ty, vars)
            .executeWithOptions(_.enableAutoCancelableRunLoops)
          domTyp <- Task.eval(toTyp(domTermEq._1))
          x = getNextVarName(vars, maxIndex(body)) :: domTyp
          valueEq <- parseEq(body, x +: vars)
            .executeWithOptions(_.enableAutoCancelableRunLoops)
        } yield {
          val isle    = tg.lambdaIsle(domTyp)
          val coeff   = Coeff(tg.lambdaIsle(domTyp), Terms)
          val eqs     = valueEq._2
          val isleEqs = eqs.map(_.mapVars((t) => InIsle(t, x, isle)))
          val initVarElems = eqs
            .flatMap { (eq) =>
              Expression.varVals(eq.rhs)
            }
            .collect {
              case InitialVal(Elem(el, rv)) => Elem(el, rv)
            }
          val isleIn: Set[EquationNode] =
            initVarElems.map { el =>
              EquationNode(
                InitialVal(InIsle(el, x, isle)),
                IsleScale(x, el) * InitialVal(el)
              )
            }
          val bridgeEq =
            EquationNode(
              FinalVal(Elem(valueEq._1, Terms)),
              coeff * FinalVal(
                InIsle(Elem(valueEq._1, isle.islandOutput(x)), x, isle)
              )
            )
          val allEqs = (isleEqs union isleIn) + bridgeEq
          valueEq._1 match {
            case FormalAppln(fn, arg) if arg == x && fn.indepOf(x) =>
              pprint.log(fn)
              fn -> allEqs
            case value =>
              Try(lambda(x)(value)).fold(err => {
                pprint.log(value)
                pprint.log(x)
                pprint.log(x.typ)
                throw LambdaFormException(x, value, err)
              }, res => res -> ???)
          }
        }
      case Pi(domain, body) =>
        // pprint.log(s"pi $domain, $body")
        for {
          domTermEq <- parseEq(domain.ty, vars)
            .executeWithOptions(_.enableAutoCancelableRunLoops)
          domTyp <- Task
            .eval(toTyp(domTermEq._1))
            .executeWithOptions(_.enableAutoCancelableRunLoops)
          x = getNextVarName(vars, maxIndex(body)) :: domTyp
          valueEq <- parseEq(body, x +: vars)
            .executeWithOptions(_.enableAutoCancelableRunLoops)
          cod <- Task.eval(toTyp(valueEq._1))
        } yield {
          val isle    = tg.piIsle(domTyp)
          val coeff   = Coeff(tg.lambdaIsle(domTyp), Terms)
          val eqs     = valueEq._2
          val isleEqs = eqs.map(_.mapVars((t) => InIsle(t, x, isle)))
          val initVarElems = eqs
            .flatMap { (eq) =>
              Expression.varVals(eq.rhs)
            }
            .collect {
              case InitialVal(Elem(el, rv)) => Elem(el, rv)
            }
          val isleIn: Set[EquationNode] =
            initVarElems.map { el =>
              EquationNode(
                InitialVal(InIsle(el, x, isle)),
                IsleScale(x, el) * InitialVal(el)
              )
            }
          val bridgeEq =
            EquationNode(
              FinalVal(Elem(valueEq._1, Terms)),
              coeff * FinalVal(
                InIsle(Elem(valueEq._1, isle.islandOutput(x)), x, isle)
              )
            )
          val allEqs = (isleEqs union isleIn) + bridgeEq

          if (LeanInterface.usesVar(body, 0)) piDefn(x)(cod) -> allEqs
          else (x.typ ->: cod)                               -> allEqs
        }
      case Let(domain, value, body) =>
        // pprint.log(s"let $domain, $value, $body")
        for {
          domTermEq <- parseEq(domain.ty, vars)
            .executeWithOptions(_.enableAutoCancelableRunLoops)
          domTyp <- Task.eval(toTyp(domTermEq._1))
          x = getNextVarName(vars, maxIndex(body)) :: domTyp
          valueTermEq <- parseEq(value, vars)
            .executeWithOptions(_.enableAutoCancelableRunLoops)
          bodyTermEq <- parseEq(body, x +: vars)
            .executeWithOptions(_.enableAutoCancelableRunLoops)
        } yield {
          def fn(
              v: GeneratorVariables.Variable[_]
          ): GeneratorVariables.Variable[_] =
            if (v == Elem(x, Terms)) Elem(valueTermEq._1, Terms) else v
          val expEqs = bodyTermEq._2.map(eq => eq.mapVars(fn))
          bodyTermEq._1.replace(x, valueTermEq._1) -> (expEqs union valueTermEq._2 union domTermEq._2)
        }
      case e => Task.raiseError(UnParsedException(e))
    }

    for {
      res <- resTask
      _ = {
        parseWork -= exp
        log(Parsed(exp))

      }
    } yield res

  }.onErrorRecoverWith {
    case pe: ParseException =>
      Task.raiseError(ParseException(pe.exps :+ exp, pe.vars, pe.error))
    case error: Exception =>
      Task.raiseError(ParseException(Vector(exp), vars, error))
  }
}
