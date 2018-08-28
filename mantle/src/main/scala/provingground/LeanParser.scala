package provingground.interface
import provingground._
import induction._

import scala.concurrent._
import duration._
import monix.execution.Scheduler.Implicits.global
import monix.eval._
import monix.reactive._
import monix.tail._
import cats._
import translation.FansiShow._

import scala.collection.mutable.{ArrayBuffer, Map => mMap, Set => mSet}
import HoTT.{Name => _, _}
import monix.execution.CancelableFuture

import math.max
import trepplein._

import scala.meta

object LeanParser {
  case class ParseException(exps: Vector[Expr],
                            vars: Vector[Term],
                            error: Exception)
      extends Exception(error.toString) {
    def apl: Option[(Expr, Expr, ApplnFailException)] =
      (exps.head, error) match {
        case (App(f, x), er: ApplnFailException) => Some((f, x, er))
        case _                                   => None
      }
  }

  sealed trait Log

  case class Defined(name: Name, term: Term) extends Log

  case class DefinedInduc(name: Name, indMod: TermIndMod) extends Log

  case class ParseWork(expr: Expr) extends Log

  case class Parsed(expr: Expr) extends Log

  trait Logger extends (Log => Unit){logger =>
    def &&(that: Logger) = new Logger{
      def apply(l: Log): Unit = {logger(l) ; that(l)}
    }
  }

  object Logger {
    def apply(f: Log => Unit): Logger = (l: Log) => f(l)

    val nop: Logger = Logger((_) => ())

    def dispatch(send: String => Unit): Logger = Logger({
      case LeanParser.Defined(name, _)      => send(s"defined $name")
      case LeanParser.DefinedInduc(name, _) => send(s"defined inductive $name")
      case LeanParser.ParseWork(expr)    => send(s"started parsing $expr; current queue : ${LeanToTermMonix.parseWork.size}")
      case LeanParser.Parsed(expr)       => send(s"finished parsing $expr; current queue : ${LeanToTermMonix.parseWork.size}")
    })
  }

  def splitVec[A](sizes: Vector[Int],
                  vec: Vector[A]): (Vector[Vector[A]], Vector[A]) =
    sizes match {
      case Vector() => (Vector(), vec)
      case n +: ms =>
        val (head, tail)    = vec.splitAt(n)
        val (prev, residue) = splitVec(ms, tail)
        (head +: prev, residue)
    }

  @annotation.tailrec
  def shiftedName(n: Int, lastName: String = "'"): String =
    if (n == 0) lastName
    else shiftedName(n - 1, prefixedNextName(lastName))

  def getNextVarName(vecs: Vector[Term], n: Int): String = {
    val cleanVecs = vecs.filterNot(isWitness)
    val lastName =
      cleanVecs.headOption
        .collect { case sym: Symbolic => sym.name.toString }
        .getOrElse(shiftedName(n))
    val newName = prefixedNextName(lastName)
    if (vecs.headOption.exists(isWitness))
      pprint.log(s"$vecs\n$cleanVecs\n$lastName; $newName")
    newName
  }

  import upickle.default._, upickle.Js

  import translation._, TermJson._

  def jsDef(parser: LeanParser) = {
    val jsDefs = parser.defnMap.toVector.map {
      case (name, term) =>
        Js.Obj("name" -> Js.Str(name.toString), "term" -> termToJson(term).get)
    }
    Js.Arr(jsDefs: _*)
  }

  def jsTermIndMod(parser: LeanParser) = {
    val jsIndMods = parser.termIndModMap.toVector map {
      case (name, tim) =>
        Js.Obj(
          "name"       -> Js.Str(name.toString),
          "num-params" -> Js.Num(tim.numParams),
          "is-propn"   -> (if (tim.isPropn) Js.True else Js.False),
          "intros"     -> Js.Arr(tim.intros.map(termToJson(_).get): _*)
        )
    }
    Js.Arr(jsIndMods: _*)
  }

  def toJs(parser: LeanParser) =
    Js.Obj(
      "defns"   -> jsDef(parser),
      "indmods" -> jsTermIndMod(parser)
    )

  def apply(filename: String): LeanParser =
    new LeanParser(LeanInterface.getMods(filename))

  def applyFuncOptFold(ft: Task[Option[Term]],
                       v: Vector[Option[Term]]): Task[Option[Term]] =
    v match {
      case Vector() => ft
      case xo +: ys =>
        applyFuncOptFold(
          ft.map(
            (fo) => fo.flatMap((f) => xo.flatMap((x) => applyFuncOpt(f, x)))
          ),
          ys)
    }

  def applyFuncFold(ft: Task[Term], v: Vector[Term]): Task[Term] =
    v match {
      case Vector() => ft
      case x +: ys =>
        applyFuncFold(ft.map(
                        (f) => fold(f)(x)
                      ),
                      ys)
    }

}

class LeanParser(initMods: Seq[Modification],
                 defTaskMap: Map[Name, Task[Term]] = Map(),
                 indTaskMap: Map[Name, Task[TermIndMod]] = Map(),
                 log: LeanParser.Logger = LeanParser.Logger.nop) {

  val mods = ArrayBuffer(initMods : _*)

  def addMods(m: Seq[Modification]) : Unit =  mods ++= m

  import LeanParser._
  import LeanToTermMonix.{RecIterAp, getRec, isPropnFn, parseWork}

  val defnMap: mMap[Name, Term] = mMap()

  val termIndModMap: mMap[Name, TermIndMod] = mMap()

  def update() : Unit = {
    import library._
    defnMap ++= LeanMemo.defMap
    termIndModMap ++= LeanMemo.indMap
  }

  val parseMemo: mMap[(Expr, Vector[Term]), Term] = mMap()

  def getMemTermIndMod(name: Name, exp: Expr): Task[TermIndMod] =
    getTermIndMod(name)
      .orElse(indModFromMod(name))
      .getOrElse(
        Task.raiseError(UnParsedException(exp))
      )

  def getNamed(name: Name): Option[Task[Term]] =
    defTaskMap
      .get(name)
      .orElse(
        defnMap.get(name).map((t) => Task.pure(t))
      )

  def getTermIndMod(name: Name): Option[Task[TermIndMod]] =
    indTaskMap
      .get(name)
      .orElse(
        termIndModMap.get(name).map((t) => Task.pure(t))
      )

  def recApp(name: Name,
             args: Vector[Expr],
             exp: Expr,
             vars: Vector[Term]): Task[Term] =
    for {
      indMod <- getMemTermIndMod(name, exp)
      (argsFmly, xs) = args.splitAt(indMod.numParams + 1)
      argsFmlyTerm <- parseVec(argsFmly, vars).cancelable
      recFnT = getRec(indMod, argsFmlyTerm)
      _      = pprint.log(s"$vars")
      vec <- parseVec(xs, vars).cancelable
      _ = pprint.log(s"${vec.map(_.fansi)}")
      recFn <- recFnT
      res = fold(recFn)(vec: _*)
    } yield res

  def recAppSkips(name: Name,
                  args: Vector[Expr],
                  exp: Expr,
                  vars: Vector[Term]): Task[Option[Term]] =
    for {
      indMod <- getMemTermIndMod(name, exp)
      (argsFmly, xs) = args.splitAt(indMod.numParams + 1)
      argsFmlyTerm <- parseVec(argsFmly, vars).cancelable
      indOpt = indMod match {
        case sm: SimpleIndMod => Some(sm.getInd(argsFmlyTerm.init))
        case _                => None
      }
      resOpt: Option[Task[Option[Term]]] = for { // Option
        ind <- indOpt
        recFnT                = getRec(indMod, argsFmlyTerm)
        (recDataExpr, ys)     = xs.splitAt(ind.seqDom.numIntros)
        (recArgsVec, residue) = LeanParser.splitVec(ind.seqDom.introArgsVec, ys)
        indicesVec            = recDataExpr.map(LeanInterface.varsUsed)
        resOptTask: Task[Option[Term]] = for { // Task
          recData <- parseVec(recDataExpr, vars).cancelable
          recFn   <- recFnT
          withRecDataTask = Task(fold(recFn)(recData: _*)).cancelable
          optParsedAllTask = Task.sequence(recArgsVec.zip(indicesVec).map {
            case (vec, indices) => parseOptVec(vec.zipWithIndex, vars, indices).cancelable
          })
          optParsedAll <- optParsedAllTask
          withRecArgsOptTask = applyFuncOptFold(withRecDataTask.map(Some(_)),
                                                optParsedAll.flatten)
          withRecArgsOpt <- withRecArgsOptTask
          residueTerms   <- parseVec(residue, vars).cancelable
          foldOpt = withRecArgsOpt.map((f) =>
            applyFuncFold(Task.pure(f), residueTerms))
          fold <- Task.sequence(foldOpt.toVector)
        } yield fold.headOption
      } yield resOptTask // Option
      resFlat = Task.sequence(resOpt.toVector).map(_.headOption.flatten)
      tsk <- resFlat
    } yield tsk // Task

  def getTask(name: String): Task[Term] =
    parse(Const(Name(name.split("\\."): _*), Vector()))

  def get(name: String): CancelableFuture[Term] =
    getTask(name).runAsync

  def getIndTask(s: String): Task[TermIndMod] =
    {
      val name = Name(s.split("\\.") : _*)
      val exp = Const(name, Vector())
      getMemTermIndMod(name, exp)
    }

  def parse(exp: Expr, vars: Vector[Term] = Vector()): Task[Term] = {
    val memParsed = parseMemo.get(exp -> vars)
    // memParsed.foreach((t) => pprint.log(s"have a memo: $t"))
    memParsed.map(Task.pure(_)).getOrElse {
      parseWork += exp
      log(ParseWork(exp))

      // pprint.log(s"Parsing $exp")
      // pprint.log(s"$parseWork")
      val resTask: Task[Term] = exp match {
        case Const(name, _) =>
          // pprint.log(s"Seeking constant: $name")
          // pprint.log(s"${defnMap.get(name).map(_.fansi)}")
          getNamed(name)
            .orElse {
              // pprint.log(s"deffromMod $name")
              defFromMod(name)
            }
            .getOrElse(
              Task.raiseError(UnParsedException(exp))
            )
        case Sort(Level.Zero) => Task.pure(Prop)
        case Sort(_)          => Task.pure(Type)
        case Var(n)           => Task.pure(vars(n))
        case RecIterAp(name, args) =>
          pprint.log(s"Seeking RecIterAp $name, $args, $vars")
          pprint.log(s"${vars.headOption.map(isWitness)}")
          recApp(name, args, exp, vars)

        case App(f, a) =>
          // pprint.log(s"Applying $f to $a")
          for {
            func <- parse(f, vars).cancelable
            arg  <- parse(a, vars).cancelable
            res = fold(func)(arg)
            // _ = pprint.log(s"got result for $f($a)")
          } yield res
        case Lam(domain, body) =>
          // pprint.log(s"lambda $domain, $body")
          for {
            domTerm <- parse(domain.ty, vars).cancelable
            domTyp  <- Task.eval(toTyp(domTerm))
            x = getNextVarName(vars, maxIndex(body)) :: domTyp
            value <- parse(body, x +: vars).cancelable
          } yield
            value match {
              case FormalAppln(fn, arg) if arg == x && fn.indepOf(x) =>
                fn
              case y if domain.prettyName.toString == "_" => y
              case _                                      =>
                // if (value.typ.dependsOn(x)) LambdaTerm(x, value)
                // else LambdaFixed(x, value)
                lambda(x)(value)
            }
        case Pi(domain, body) =>
          // pprint.log(s"pi $domain, $body")
          for {
            domTerm <- parse(domain.ty, vars).cancelable
            domTyp  <- Task.eval(toTyp(domTerm)).cancelable
            x = getNextVarName(vars, maxIndex(body)) :: domTyp
            value <- parse(body, x +: vars).cancelable
            cod   <- Task.eval(toTyp(value))
          } yield
            if (LeanInterface.usesVar(body, 0)) PiDefn(x, cod)
            else x.typ ->: cod
        case Let(domain, value, body) =>
          // pprint.log(s"let $domain, $value, $body")
          for {
            domTerm <- parse(domain.ty, vars).cancelable
            domTyp  <- Task.eval(toTyp(domTerm))
            x = getNextVarName(vars, maxIndex(body)) :: domTyp
            valueTerm <- parse(value, vars).cancelable
            bodyTerm  <- parse(body, x +: vars).cancelable
          } yield bodyTerm.replace(x, valueTerm)
        case e => Task.raiseError(UnParsedException(e))
      }

      for {
        res <- resTask
        _ = {
          parseWork -= exp
          log(Parsed(exp))
          parseMemo += (exp, vars) -> res
          // pprint.log(s"parsed $exp")
          // if (isPropFmly(res.typ))
          //   pprint.log(
          //     s"\n\nWitness: ${res.fansi}\n\nprop: ${res.typ.fansi}\n\n")
        }
      } yield res
      // if (isPropFmly(res.typ)) "_" :: (res.typ) else res
    }
  }.onErrorRecoverWith {
    case pe: ParseException =>
      Task.raiseError(ParseException(pe.exps :+ exp, pe.vars, pe.error))
    case error: Exception =>
      Task.raiseError(ParseException(Vector(exp), vars, error))
  }

  def parseVec(vec: Vector[Expr], vars: Vector[Term]): Task[Vector[Term]] =
    vec match {
      case Vector() => Task.pure(Vector())
      case x +: ys =>
        for {
          head <- parse(x, vars).cancelable
          tail <- parseVec(ys, vars).cancelable
        } yield (head +: tail)
    }

  def parseOptVec(vec: Vector[(Expr, Int)],
                  vars: Vector[Term],
                  indices: Set[Int]): Task[Vector[Option[Term]]] =
    vec match {
      case Vector() => Task.pure(Vector())
      case (x, m) +: ys =>
        for {
          tail <- parseOptVec(ys, vars, indices).cancelable
          headOpt <- if (indices.contains(m)) parse(x, vars).cancelable.map(Option(_))
          else Task.pure(None)
        } yield (headOpt +: tail)
    }

  def getValue(t: Term,
               n: Int,
               accum: Vector[Term]): Task[(Term, Vector[Term])] =
    (t, n) match {
      case (x, 0) => Task.eval(x -> accum)
      case (l: LambdaLike[u, v], n) if n > 0 =>
        getValue(l.value, n - 1, accum :+ l.variable)
      case (fn: FuncLike[u, v], n) if n > 0 =>
        val x = fn.dom.Var
        getValue(fn(x), n - 1, accum :+ x)
      case _ => throw new Exception("getValue failed")
    }

  def withDefn(name: Name, exp: Expr): Task[Unit] =
    for {
      term <- parse(exp, Vector()).cancelable
      _ = {
        pprint.log(s"Defined $name"); log(Defined(name, term));
        defnMap += name -> term
      }
    } yield ()

  def withAxiom(name: Name, ty: Expr): Task[Unit] =
    for {
      typ <- parse(ty, Vector()).cancelable
      term = (name.toString) :: toTyp(typ)
      _ = {
        pprint.log(s"Defined $name"); log(Defined(name, term));
        defnMap += name -> term
      }
    } yield ()

  def withAxiomSeq(axs: Vector[(Name, Expr)]): Task[Unit] =
    axs match {
      case Vector() => Task(())
      case (name, ty) +: ys =>
        for {
          _ <- withAxiom(name, ty)
          _ <- withAxiomSeq(ys)
        } yield ()
    }

// Like withAxiomSeq but returns the axioms
  def foldAxiomSeq(accum: Vector[Term],
                   axs: Vector[(Name, Expr)]): Task[Vector[Term]] = axs match {
    case Vector() =>
      Task(accum)
    case (name, ty) +: ys =>
      for {
        typ <- parse(ty, Vector()).cancelable
        // (typ, ltm1) = pr
        term = (name.toString) :: toTyp(typ)
        _ = {
          pprint.log(s"Defined $name"); log(Defined(name, term));
          defnMap += name -> term
        }
        res <- foldAxiomSeq(term +: accum, ys)
      } yield res
  }

  def withMod(mod: Modification): Task[Unit] = mod match {
    case ind: IndMod =>
      val isPropn = isPropnFn(ind.ty)
      val name    = ind.name
      for {
        indTypTerm <- parse(ind.ty, Vector()).cancelable
        // (indTypTerm, ltm1) = pr
        indTyp = toTyp(indTypTerm)
        typF   = name.toString :: indTyp
        _ = {
          pprint.log(s"Defined $name"); log(Defined(name, typF));
          defnMap += name -> typF
        }
        intros <- foldAxiomSeq(Vector(), ind.intros).map(_.reverse)
        // (intros, withIntros) = introsPair
        typValuePair <- getValue(typF, ind.numParams, Vector())
        indMod = typValuePair match {
          case (typ: Typ[Term], params) =>
            SimpleIndMod(ind.name, typF, intros, params.size, isPropn)
          case (t, params) =>
            IndexedIndMod(ind.name, typF, intros, params.size, isPropn)
        }
        _ = { log(DefinedInduc(name, indMod)); termIndModMap += ind.name -> indMod }
      } yield ()

    case ax: AxiomMod =>
      withAxiom(ax.name, ax.ty)
    case df: DefMod =>
      withDefn(df.name, df.value)
    case QuotMod =>
      import quotient._
      val axs = Vector(quot, quotLift, quotMk, quotInd).map { (ax) =>
        (ax.name, ax.ty)
      }
      withAxiomSeq(axs)
  }

  val allIntros = mods.collect { case ind: IndMod => ind.intros }.flatten

  def findDefMod(name: Name) =
    mods.collect {
      case dm: DefMod if dm.name == name => dm
    }.headOption

  def findIndMod(name: Name) =
    mods.collect {
      case dm: IndMod if dm.name == name => dm
    }.headOption

  def findIntro(name: Name): Option[Expr] =
    allIntros.find(_._1 == name).map(_._2)

  def findRecChildren(name: Name): Option[Vector[Expr]] =
    name match {
      case Name.Str(prefix, "rec") =>
        findIndMod(prefix).map((ind) => ind.ty +: ind.intros.map(_._2))
      case _ => None
    }

  def findChildren(name: Name): Option[Vector[Expr]] =
    findDefMod(name)
      .map((dm) => Vector(dm.ty, dm.value))
      .orElse(findIndMod(name).map((ind) => Vector(ind.ty)))
      .orElse(findIntro(name).map((exp: Expr) => Vector(exp)))
      .orElse(findRecChildren(name))

  def maxIndex(exp: Expr): Int = exp match {
    case Sort(_)           => 0
    case Var(_)            => 0
    case App(f, x)         => max(maxIndex(f), maxIndex(x))
    case LocalConst(_, _)  => 0
    case Lam(domain, body) => max(maxIndex(domain.ty) + 1, maxIndex(body) + 1)
    case Pi(domain, body)  => max(maxIndex(domain.ty) + 1, maxIndex(body) + 1)
    case Let(domain, value, body) =>
      Vector(maxIndex(domain.ty), maxIndex(value), maxIndex(body) + 1).max
    case Const(name @ Name.Str(prefix, "rec"), _) =>
      findRecChildren(name)
        .map((v) => v.map(maxIndex).max * 2 + 1)
        .getOrElse(throw new Exception(s"could not find name $name"))
    case Const(name, _) =>
      findChildren(name)
        .map((v) => v.map(maxIndex).max)
        .getOrElse(throw new Exception(s"could not find name $name"))
  }

  def modNames(mod: Modification): Vector[Name] = mod match {
    case ind: IndMod =>
      ind.name +: (ind.intros.map(_._1))
    case QuotMod =>
      import quotient._
      Vector(quot, quotLift, quotMk, quotInd).map(_.name)
    case _ =>
      Vector(mod.name)
  }

  def findMod(name: Name, mods: Seq[Modification]): Option[Modification] =
    mods.find((mod) => modNames(mod).contains(name))

  def defFromMod(name: Name): Option[Task[Term]] =
    findMod(name, mods).map { (mod) =>
      // pprint.log(s"Using ${mod.name}")
      for {
        _ <- withMod(mod)
      } yield (defnMap(name))
    }

  def indModFromMod(name: Name): Option[Task[TermIndMod]] =
    findMod(name, mods).map { (mod) =>
      // pprint.log(s"Using ${mod.name}")
      for {
        _ <- withMod(mod)
      } yield (termIndModMap(name))
    }

  // code generation

  def defNames: ArrayBuffer[Name] = mods.collect { case df: DefMod => df.name }

  def allIndNames: ArrayBuffer[Name] = mods.collect { case ind: IndMod => ind.name }

  import translation.CodeGen

  def codeGen: CodeGen =
    CodeGen.objNames(defNames.map(_.toString), allIndNames.map(_.toString))

  def defnCode: ArrayBuffer[(Name, meta.Term)] =
    defNames.flatMap { (name) =>
      for {
        term <- defnMap.get(name)
        code <- codeGen(term)
      } yield (name, code)
    }

  def codeFromInd(ind: TermIndMod): meta.Term = {
    val p = getVariables(ind.numParams)(ind.typF).toVector
    val codeOpt =
      ind match {
        case mod: SimpleIndMod =>
          val seq =
            ConstructorSeqTL
              .getExst(toTyp(foldFunc(ind.typF, p)),
                       LeanToTermMonix.introsFold(ind, p))
              .value
          codeGen.consSeq(seq)
        case mod: IndexedIndMod =>
          val indSeq =
            TypFamilyExst
              .getIndexedConstructorSeq(foldFunc(ind.typF, p),
                                        LeanToTermMonix.introsFold(ind, p))
              .value
          codeGen.indexedConsSeqDom(indSeq)
      }
    import scala.meta._
    val cp = p.map(codeGen(_).get)
    cp.foldRight(codeOpt.get) { case (x, y) => q"Subst.Lambda($x, $y)" }
  }

}
