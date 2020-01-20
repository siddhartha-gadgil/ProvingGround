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

import LeanInterface._, LeanParser._
import ujson.Arr

import scala.util.Try
import provingground.learning.TermGeneratorNodes

import TermRandomVars._, GeneratorVariables._, Expression._
import LeanParser._
import provingground.translation.FansiShow._

object LeanParserEq {
  def load(s: String = "basic"): LeanParserEq = {
    val name = s"$s.lean.export"
    val path = os.resource / name
    val in   = new java.io.ByteArrayInputStream(os.read.bytes(path))
    val mods = LeanInterface.getModsFromStream(in)
    new LeanParserEq(mods)
  }
}

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

  def funcFoldEqs(
      fn: Term,
      depth: Int,
      args: Vector[Term],
      result: Term
  ): Set[EquationNode] =
    if (depth < 1) Set()
    else {
      val coeff   = Coeff(tg.foldFuncNode(fn, depth))
      val x       = args.head
      val y       = fold(fn)(x)
      val tailVar = if (depth == 1) AtomVar(y) else FuncFoldVar(y, depth - 1)
      val eq = EquationNode(
        FinalVal(Elem(result, FuncFoldVar(fn, depth))),
        coeff * FinalVal(Elem(x, TermGeneratorNodes.termsWithTyp(x.typ))) * FinalVal(
          Elem(result, tailVar)
        )
      )
      funcFoldEqs(y, depth - 1, args.tail, result) + eq
    }

  def recAppEq(
      name: Name,
      args: Vector[Expr],
      exp: Expr,
      vars: Vector[Term]
  ): Task[(Term, Set[EquationNode])] =
    for {
      indMod <- getMemTermIndModEq(name, exp)
      (argsFmly, xs) = args.splitAt(indMod.numParams + 1)
      argsFmlyTermEq <- parseVecEq(argsFmly, vars).executeWithOptions(
        _.enableAutoCancelableRunLoops
      )
      recFnT = getRec(indMod, argsFmlyTermEq._1)
      vecEq <- parseVecEq(xs, vars).executeWithOptions(
        _.enableAutoCancelableRunLoops
      )
      vecInter = indMod.interleaveData(vecEq._1)
      recFn <- recFnT
      resT = Task(foldFuncLean(recFn, vecInter)).onErrorRecoverWith {
        case err: ApplnFailException =>
          throw RecFoldException(
            indMod,
            args,
            recFn,
            argsFmlyTermEq._1,
            vecInter,
            err
          )
      }
      res <- resT
      target = foldFuncLean(recFn, vecInter.take(indMod.intros.size)).typ
      ind <- getExstInduc(indMod, argsFmlyTermEq._1)
    } yield {
      val nodeOpt = tg.targetInducFuncsFolded(ind, target)
      val node    = nodeOpt.getOrElse(tg.targetInducNode(target))
      val coeff   = Coeff(node)
      val depth   = ind.intros.size
      val foldVar = if (depth == 0) AtomVar(res) else FuncFoldVar(recFn, depth)
      val baseEq =
        EquationNode(
          FinalVal(Elem(res, Terms)),
          coeff * FinalVal(Elem(target, Typs)) * FinalVal(Elem(res, foldVar))
        )
      val foldEqs = funcFoldEqs(recFn, depth, vecInter, res)
      res -> ((argsFmlyTermEq._2 union vecEq._2 union foldEqs) + baseEq)
    }

  def parseVecEq(
      vec: Vector[Expr],
      vars: Vector[Term]
  ): Task[(Vector[Term], Set[EquationNode])] =
    Task
      .sequence {
        vec.map(exp => parseEq(exp, vars))
      }
      .map(
        v => (v.map(_._1), v.map(_._2).fold(Set.empty[EquationNode])(_ union _))
      )
  // vec match {
  //   case Vector() => Task.pure(Vector() -> Set())
  //   case x +: ys =>
  //     for {
  //       head <- parseEq(x, vars).executeWithOptions(_.enableAutoCancelableRunLoops)
  //       tail <- parseVecEq(ys, vars).executeWithOptions(_.enableAutoCancelableRunLoops)
  //     } yield (head._1 +: tail._1) -> (head._2 union tail._2)
  // }

  val defnMapEq: mMap[Name, (Term, Set[EquationNode])] = mMap()

  def withDefnEq(name: Name, exp: Expr): Task[Unit] =
    for {
      termEq <- parseEq(exp, Vector())
        .executeWithOptions(_.enableAutoCancelableRunLoops)
      _ = {
        pprint.log(s"Defined $name"); log(Defined(name, termEq._1))
        defnMapEq += name -> termEq
      }
    } yield ()

  def indModFromModEq(name: Name): Option[Task[TermIndMod]] =
    findMod(name, mods.toVector).map { (mod) =>
      // pprint.log(s"Using ${mod.name}")
      for {
        _ <- withModEq(mod)
      } yield (termIndModMap(name))
    }

  def getMemTermIndModEq(name: Name, exp: Expr): Task[TermIndMod] =
    getTermIndMod(name)
      .orElse(indModFromModEq(name))
      .getOrElse(
        Task.raiseError(UnParsedException(exp))
      )

  def withAxiomEq(name: Name, ty: Expr): Task[Unit] =
    for {
      typEq <- parseEq(ty, Vector())
        .executeWithOptions(_.enableAutoCancelableRunLoops)
      term = (name.toString) :: toTyp(typEq._1)
      _ = {
        pprint.log(s"Defined $name"); log(Defined(name, term))
        defnMapEq += name -> (term, typEq._2)
      }
    } yield ()

  def withAxiomSeqEq(axs: Vector[(Name, Expr)]): Task[Unit] =
    axs match {
      case Vector() => Task(())
      case (name, ty) +: ys =>
        for {
          _ <- withAxiomEq(name, ty)
          _ <- withAxiomSeqEq(ys)
        } yield ()
    }

  def foldAxiomSeqEq(
      accum: (Vector[Term], Set[EquationNode]),
      axs: Vector[(Name, Expr)]
  ): Task[(Vector[Term], Set[EquationNode])] = axs match {
    case Vector() =>
      Task(accum)
    case (name, ty) +: ys =>
      pprint.log(s"seeking $name : ${ty.toString}")
      for {
        typEq <- parseEq(ty, Vector())
          .executeWithOptions(_.enableAutoCancelableRunLoops)
        // (typ, ltm1) = pr
        term = (name.toString) :: toTyp(typEq._1)
        _ = {
          pprint.log(s"Defined $name"); log(Defined(name, term))
          defnMapEq += name -> (term, typEq._2)
        }
        res <- foldAxiomSeqEq(((accum._1) :+ term, typEq._2 union accum._2), ys)
      } yield res
  }

  def withModEq(mod: Modification): Task[Unit] = mod match {
    case ind: IndMod =>
      val isPropn = isPropnFn(ind.ty)
      val name    = ind.name
      for {
        indTypTermEq <- parseEq(ind.ty, Vector())
          .executeWithOptions(_.enableAutoCancelableRunLoops)
        // (indTypTerm, ltm1) = pr
        (indTypTerm, indEqs) = indTypTermEq
        indTyp               = toTyp(indTypTerm)
        typF                 = name.toString :: indTyp
        _ = {
          pprint.log(s"Defined $name"); log(Defined(name, typF))
          defnMapEq += name -> (typF, indEqs)
        }
        _ = pprint.log(s"seeking intros: ${ind.intros.map(_.toString)}")
        _ = pprint.log(defnMapEq.get(name).toString)
        introsEq <- foldAxiomSeqEq(Vector() -> Set(), ind.intros)
        // (intros, withIntros) = introsPair
        typValuePair <- getValue(typF, ind.numParams, Vector())
        indMod = typValuePair match {
          case (_: Typ[Term], params) =>
            SimpleIndMod(ind.name, typF, introsEq._1, params.size, isPropn)
          case (_, params) =>
            IndexedIndMod(ind.name, typF, introsEq._1, params.size, isPropn)
        }
        _ = {
          log(DefinedInduc(name, indMod)); termIndModMap += ind.name -> indMod
        }
      } yield ()

    case ax: AxiomMod =>
      withAxiomEq(ax.name, ax.ty)
    case df: DefMod =>
      withDefnEq(df.name, df.value)
    case QuotMod =>
      import quotient._
      val axs = Vector(quot, quotLift, quotMk, quotInd).map { (ax) =>
        (ax.name, ax.ty)
      }
      withAxiomSeqEq(axs)
  }

  def defFromModEq(name: Name): Option[Task[(Term, Set[EquationNode])]] =
    findMod(name, mods.toVector).map { (mod) =>
      // pprint.log(s"Using ${mod.name}")
      for {
        _ <- withModEq(mod)
      } yield (defnMapEq(name))
    }

  def getNamedEq(name: Name): Option[Task[(HoTT.Term, Set[EquationNode])]] =
    defTaskMap
      .get(name)
      .map(tsk => tsk.map(term => (term, Set.empty[EquationNode])))
      .orElse(
        defnMapEq.get(name).map((t) => Task.pure(t))
      )

  def parseEq(
      exp: Expr,
      vars: Vector[Term] = Vector()
  ): Task[(Term, Set[EquationNode])] = {

    parseWork += exp
    log(ParseWork(exp))
    val resTask: Task[(Term, Set[EquationNode])] = exp match {
      case Const(name, _) =>
        pprint.log(s"seeking $name")
        getNamedEq(name)
          .orElse {
            defFromModEq(name)
          }
          .getOrElse(
            Task.raiseError(UnParsedException(exp))
          )

      case Sort(Level.Zero) => Task.pure(Prop    -> Set())
      case Sort(_)          => Task.pure(Type    -> Set())
      case Var(n)           => Task.pure(vars(n) -> Set())
      case RecIterAp(name, args) =>
        pprint.log(s"Seeking RecIterAp $name, $args, $vars")
        pprint.log(s"${vars.headOption.map(isWitness)}")
        recAppEq(name, args, exp, vars)

      case App(f, a) =>
        // pprint.log(s"Applying $f to $a")
        for {
          // pair <- Task.parZip2(
          //   parseEq(f, vars)
          //     .executeWithOptions(_.enableAutoCancelableRunLoops),
          //   parseEq(a, vars)
          //     .executeWithOptions(_.enableAutoCancelableRunLoops)
          // )
          // (funcEqs, argEqs) = pair
          funcEqs <- parseEq(f, vars)
            .executeWithOptions(_.enableAutoCancelableRunLoops)
          argEqs <- parseEq(a, vars)
            .executeWithOptions(_.enableAutoCancelableRunLoops)
          res = Try(applyFuncLean(funcEqs._1, argEqs._1))
            .getOrElse(
              throw new ApplnParseException(f, a, funcEqs._1, argEqs._1, vars)
            )
          // _ = pprint.log(s"got result for $f($a)")
          eq = EquationNode(
            FinalVal(Elem(res, Terms)),
            Coeff(tg.applnNode) * FinalVal(
              Elem(funcEqs._1, TermGeneratorNodes.termsWithTyp(funcEqs._1.typ))
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
          val coeff   = Coeff(tg.lambdaIsle(domTyp))
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
              val rhs =
                if (x == el.element) (IsleScale(x, el) * -1) + Literal(1)
                else IsleScale(x, el) * InitialVal(el)
              EquationNode(
                InitialVal(InIsle(el, x, isle)),
                rhs
              )
            }
          val bridgeEq =
            EquationNode(
              FinalVal(Elem(x :~> valueEq._1, Terms)),
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
              }, res => res -> allEqs)
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
          val coeff   = Coeff(tg.lambdaIsle(domTyp))
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
      Task.raiseError(ParseException(pe.expVars :+ (exp -> vars), pe.error))
    case error: Exception =>
      Task.raiseError(ParseException(Vector(exp -> vars), error))
  }

  def getEqTask(name: String) =
    parseEq(Const(Name(name.split("\\."): _*), Vector()))

  def getEqFut(name: String) =
    getEqTask(name).runToFuture

  def getEq(name: String) = getEqTask(name).runSyncUnsafe()

  def getEqTry(name: String) =
    getEqTask(name).materialize.runSyncUnsafe()

  def getEqError(name: String): Option[ParseException] =
    getEqTry(name).fold(
      err =>
        err match {
          case pe: ParseException => Some(pe)
          case _                  => None
        },
      _ => None
    )

}
