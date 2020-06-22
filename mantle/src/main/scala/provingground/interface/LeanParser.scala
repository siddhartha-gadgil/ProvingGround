package provingground.interface
import provingground._
import induction._

import monix.execution.Scheduler.Implicits.global
import monix.eval._
import translation.FansiShow._

import scala.collection.mutable.{ArrayBuffer, Map => mMap}
import HoTT.{Name => _, _}
import monix.execution.CancelableFuture

import math.max
import trepplein._

import LeanInterface._
import ujson.Arr

import scala.util.Try

import scala.collection.mutable.{Set => mSet}

case class RecFoldException(
    indMod: TermIndMod,
    argFmlyExps: Vector[Expr],
    recFn: Term,
    argsFmlyTerm: Vector[Term],
    vec: Vector[Term],
    fail: ApplnFailException
) extends IllegalArgumentException(
      s"Failure to fold recursive Function for ${indMod.name}, recursion function $recFn with error $fail"
    )

object LeanParser {
  val parseWork: mSet[Expr] = mSet()

  def load(s: String = "basic"): LeanParser = {
    val name = s"$s.lean.export"
    val path = os.resource / name
    val in   = new java.io.ByteArrayInputStream(os.read.bytes(path))
    val mods = provingground.interface.LeanInterface.getModsFromStream(in)
    new LeanParser(mods)
  }

  def proofLift: (Term, Term) => Task[Term] = {
    case (w: Typ[u], tp: Typ[v]) =>
      Task.eval { (w.Var) :-> tp } // should be in all cases
    case (w: FuncLike[u, v], tp: FuncLike[a, b]) if w.dom == tp.dom =>
      val x = w.dom.Var
      proofLift(w(x), tp(x.asInstanceOf[a]))
        .map((g: Term) => x :~> (g: Term))
    case _ => throw new Exception("could not lift proof")
  }

  def isPropnFn(e: Expr): Boolean = e match {
    case Pi(_, t) => isPropnFn(t)
    case Sort(l)  => l == Level.Zero
    case _        => false
  }

  def getValue(
    t: Term,
    n: Int,
    accum: Vector[Term]
): Task[(Term, Vector[Term])] =
  (t, n) match {
    case (x, 0) => Task.eval(x -> accum)
    case (l: LambdaLike[u, v], m) if m > 0 =>
      getValue(l.value, m - 1, accum :+ l.variable)
    case (fn: FuncLike[u, v], m) if m > 0 =>
      val x = fn.dom.Var
      getValue(fn(x), m - 1, accum :+ x)
    case _ => throw new Exception("getValue failed")
  }

  def introsFold(ind: TermIndMod, p: Vector[Term]): Vector[Term] =
    ind.intros.map((rule) => foldFuncLean(rule, p))

  def getRec(ind: TermIndMod, argsFmlyTerm: Vector[Term]): Task[Term] =
    ind match {
      case smp: SimpleIndMod =>
        getRecSimple(smp, Task.pure(argsFmlyTerm))
      case indInd: IndexedIndMod =>
        getRecIndexed(indInd, Task.pure(argsFmlyTerm))
    }

  def getExstInduc(
      ind: TermIndMod,
      argsFmlyTerm: Vector[Term]
  ): Task[ExstInducDefn] =
    ind match {
      case smp: SimpleIndMod =>
        getSimpleExstInduc(smp, Task.pure(argsFmlyTerm))
      case indInd: IndexedIndMod =>
        getIndexedExstInduc(indInd, Task.pure(argsFmlyTerm))
    }

  def getSimpleExstInduc(
      ind: SimpleIndMod,
      argsFmlyTerm: Task[Vector[Term]]
  ): Task[ExstInducDefn] =
    argsFmlyTerm.map { argsFmly =>
      val params = argsFmly.init
      val typ    = toTyp(foldFuncLean(ind.typF, params))
      val intros = introsFold(ind, params)
      val str0   = ExstInducStrucs.get(typ, introsFold(ind, params))
      val str = params.foldRight(str0) {
        case (x, s) => ExstInducStrucs.LambdaInduc(x, s)
      }
      val dfn = ExstInducDefn(typ, intros.toVector, str)
      dfn
    }

  def getRecSimple(
      ind: SimpleIndMod,
      argsFmlyTerm: Task[Vector[Term]]
  ): Task[Term] = {
    def getInd(p: Vector[Term]) =
      ConstructorSeqTL
        .getExst(toTyp(foldFuncLean(ind.typF, p)), introsFold(ind, p))
        .value
    val newParamsTask = argsFmlyTerm map (_.init)
    newParamsTask.flatMap { (newParams) =>
      val indNew =
        getInd(newParams)

      val fmlyOpt = argsFmlyTerm map (_.last)
      fmlyOpt map {
        case l: LambdaLike[u, v] =>
          l.value match {
            case tp: Typ[u] =>
              if (tp.dependsOn(l.variable))(indNew
                .inducE((l.variable: Term) :-> (tp: Typ[u])))
              else (indNew.recE(tp))
          }
        case fn: FuncLike[u, v] =>
          val x = fn.dom.Var
          val y = fn(x)
          y match {
            case tp: Typ[u] =>
              if (tp.dependsOn(x)) {
                (indNew.inducE((x: Term) :-> (tp: Typ[u])))
              } else (indNew.recE(tp))
          }
        // case pt: PiDefn[u, v] if ind.isPropn && pt.domain == indNew.typ =>
        //   indNew.inducE(lmbda(pt.variable: Term)(pt.value))
        case tp: Typ[u] if (ind.isPropn) =>
          // val x = tp.Var
          // if (tp.dependsOn(x)) {
          //   (indNew.inducE((x: Term) :-> (tp: Typ[u])))
          // } else
          (indNew.recE(tp))
      }

    }
  }

  def getIndexedExstInduc(
      ind: IndexedIndMod,
      argsFmlyTerm: Task[Vector[Term]]
  ): Task[ExstInducDefn] =
    argsFmlyTerm.map { argsFmly =>
      val params = argsFmly.init
      val typF   = foldFuncLean(ind.typF, params)
      val intros = introsFold(ind, params)
      val str0   = ExstInducStrucs.getIndexed(typF, introsFold(ind, params))
      val str = params.foldRight(str0) {
        case (x, s) => ExstInducStrucs.LambdaInduc(x, s)
      }
      val dfn = ExstInducDefn(ind.typF, intros.toVector, str)
      dfn
    }

  def getRecIndexed(
      ind: IndexedIndMod,
      argsFmlyTerm: Task[Vector[Term]]
  ): Task[Term] = {
    def getInd(p: Vector[Term]) =
      TypFamilyExst
        .getIndexedConstructorSeq(foldFuncLean(ind.typF, p), introsFold(ind, p))
        .value
    val newParamsTask = argsFmlyTerm map (_.init)
    newParamsTask.flatMap { (newParams) =>
      val indNew =
        getInd(newParams)
      val fmlOptRaw = argsFmlyTerm map (_.last)
      val fmlOpt =
        if (ind.isPropn)
          fmlOptRaw.flatMap((fib) => proofLift(indNew.W, fib))
        else
          fmlOptRaw
      val recOptTask =
        for {
          fml <- fmlOpt
          codOpt = indNew.family.constFinalCod(fml)
        } yield codOpt.map((cod) => indNew.recE(cod))
      val inducTask =
        fmlOpt.map((fib) => indNew.inducE(fib))
      for {
        recOpt <- recOptTask
        induc  <- inducTask
      } yield recOpt.getOrElse(induc)

    }
  }

  object RecIterAp {
    def unapply(exp: Expr): Option[(Name, Vector[Expr])] = exp match {
      case Const(Name.Str(prefix, "rec"), _) => Some((prefix, Vector()))
      case App(fn, x) =>
        unapply(fn).map { case (name, vec) => (name, vec :+ x) }
      case _ => None
    }
  }

  case class ParseException(
      expVars: Vector[(Expr, Vector[Term])],
      error: Exception
  ) extends Exception(error.toString) {
    val exps: Vector[Expr] = expVars.map(_._1)
    val vars: Vector[Term] = expVars.last._2
    def apl: Option[(Expr, Expr, ApplnParseException)] =
      (exps.head, error) match {
        case (App(f, x), er: ApplnParseException) => Some((f, x, er))
        case _                                   => None
      }

    def recfl: Option[RecFoldException] = error match {
      case e: RecFoldException => Some(e)
      case _                   => None
    }
  }

  class ApplnParseException(
      val fe: Expr,
      val ae: Expr,
      func: Term,
      arg: Term,
      val vars: Vector[Term]
  ) extends ApplnFailException(func, arg)

  case class LambdaFormException(variable: Term, value: Term, error: Throwable)
      extends Exception(error.getMessage)

  sealed trait Log

  case class Defined(name: Name, term: Term) extends Log

  case class DefinedInduc(name: Name, indMod: TermIndMod) extends Log

  case class ParseWork(expr: Expr) extends Log

  case class Parsed(expr: Expr) extends Log

  trait Logger extends (Log => Unit) { logger =>
    def &&(that: Logger): Logger = (l: Log) => {
      logger(l); that(l)
    }
  }

  object Logger {
    def apply(f: Log => Unit): Logger = (l: Log) => f(l)

    val nop: Logger = Logger((_) => ())

    def dispatch(send: String => Unit): Logger =
      Logger({
        case LeanParser.Defined(name, _) => send(s"defined $name")
        case LeanParser.DefinedInduc(name, _) =>
          send(s"defined inductive $name")
        case LeanParser.ParseWork(expr) =>
          send(s"started parsing $expr; current queue : ${parseWork.size}")
        case LeanParser.Parsed(expr) =>
          send(s"finished parsing $expr; current queue : ${parseWork.size}")
      })
  }

  def splitVec[A](
      sizes: Vector[Int],
      vec: Vector[A]
  ): (Vector[Vector[A]], Vector[A]) =
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

  import TermJson._

  def jsDef(parser: LeanParser): Arr = {
    val jsDefs = parser.defnMap.toVector.map {
      case (name, term) =>
        ujson.Obj(
          "name" -> ujson.Str(name.toString),
          "term" -> termToJson(term).get
        )
    }
    ujson.Arr(jsDefs: _*)
  }

  def jsTermIndMod(parser: LeanParser): Arr = {
    val jsIndMods = parser.termIndModMap.toVector map {
      case (name, tim) =>
        ujson.Obj(
          "name"       -> ujson.Str(name.toString),
          "num-params" -> ujson.Num(tim.numParams),
          "is-propn"   -> (if (tim.isPropn) ujson.True else ujson.False),
          "intros"     -> ujson.Arr(tim.intros.map(termToJson(_).get): _*)
        )
    }
    ujson.Arr(jsIndMods: _*)
  }

  def toJs(parser: LeanParser) =
    ujson.Obj(
      "defns"   -> jsDef(parser),
      "indmods" -> jsTermIndMod(parser)
    )

  def apply(filename: String): LeanParser =
    new LeanParser(LeanInterface.getMods(filename))

  def applyFuncOptFold(
      ft: Task[Option[Term]],
      v: Vector[Option[Term]]
  ): Task[Option[Term]] =
    v match {
      case Vector() => ft
      case xo +: ys =>
        applyFuncOptFold(
          ft.map(
            (fo) => fo.flatMap((f) => xo.flatMap((x) => applyFuncOpt(f, x)))
          ),
          ys
        )
    }

  def applyFuncFold(ft: Task[Term], v: Vector[Term]): Task[Term] =
    v match {
      case Vector() => ft
      case x +: ys =>
        applyFuncFold(
          ft.map(
            (f) => applyFuncLean(f, x)
          ),
          ys
        )
    }

}

class LeanParser(
    initMods: Seq[Modification],
    defTaskMap: Map[Name, Task[Term]] = Map(),
    indTaskMap: Map[Name, Task[TermIndMod]] = Map(),
    log: LeanParser.Logger = LeanParser.Logger.nop
) {

  val mods: ArrayBuffer[Modification] = ArrayBuffer(initMods: _*)

  def addMods(m: Seq[Modification]): Unit = mods ++= m

  import LeanParser._

  val defnMap: mMap[Name, Term] = mMap()

  val termIndModMap: mMap[Name, TermIndMod] = mMap()

  def update(): Unit = {
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

  def recApp(
      name: Name,
      args: Vector[Expr],
      exp: Expr,
      vars: Vector[Term]
  ): Task[Term] =
    for {
      indMod <- getMemTermIndMod(name, exp)
      (argsFmly, xs) = args.splitAt(indMod.numParams + 1)
      argsFmlyTerm <- parseVec(argsFmly, vars).executeWithOptions(
        _.enableAutoCancelableRunLoops
      )
      recFnT = getRec(indMod, argsFmlyTerm)
      vec <- parseVec(xs, vars).executeWithOptions(
        _.enableAutoCancelableRunLoops
      )
      vecInter = indMod.interleaveData(vec)
      recFn <- recFnT
      resT = Task(foldFuncLean(recFn, vecInter)).onErrorRecoverWith {
        case err: ApplnFailException =>
          throw RecFoldException(
            indMod,
            args,
            recFn,
            argsFmlyTerm,
            vecInter,
            err
          )
      }
      res <- resT
    } yield res


  def getTask(name: String): Task[Term] =
    parse(Const(Name(name.split("\\."): _*), Vector()))

  def getFut(name: String): CancelableFuture[Term] =
    getTask(name).runToFuture

  def get(name: String): Term = getTask(name).runSyncUnsafe()

  def getTry(name: String): Try[Term] =
    getTask(name).materialize.runSyncUnsafe()

  def getError(name: String) : Option[ParseException] = 
    getTry(name).fold(
      err => err match {
        case pe : ParseException => Some(pe)
        case _ => None
      },
      _ => None
    )

  def getIndTask(s: String): Task[TermIndMod] = {
    val name = Name(s.split("\\."): _*)
    val exp  = Const(name, Vector())
    getMemTermIndMod(name, exp)
  }

  def getInd(s: String) = getIndTask(s).runSyncUnsafe()

  def findDef(s: String): Option[DefMod] = {
    val name = trepplein.Name(s.split("\\."): _*)
    mods.find(_.name == name).collect { case df: DefMod => df }
  }

  def findInd(s: String): Option[IndMod] = {
    val name = trepplein.Name(s.split("\\."): _*)
    mods.find(_.name == name).collect { case df: IndMod => df }
  }

  def allNames(init: String = "") =
    mods
      .filter(_.name.toString.startsWith(init))
      .map(_.name.toString)

  def topNames(init: String = "") =
    allNames(init).map(s => s.split("\\.").headOption).flatten.distinct

  def parse(exp: Expr, vars: Vector[Term] = Vector()): Task[Term] = {
    val memParsed = parseMemo.get(exp -> vars)
    memParsed.map(Task.pure).getOrElse {
      parseWork += exp
      log(ParseWork(exp))
      val resTask: Task[Term] = exp match {
        case Const(name, _) =>
          getNamed(name)
            .orElse {
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
          for {
            func <- parse(f, vars)
              .executeWithOptions(_.enableAutoCancelableRunLoops)
            arg <- parse(a, vars)
              .executeWithOptions(_.enableAutoCancelableRunLoops)
            res = Try(applyFuncLean(func, arg))
              .getOrElse(throw new ApplnParseException(f, a, func, arg, vars))
          } yield res
        case Lam(domain, body) =>
          for {
            domTerm <- parse(domain.ty, vars)
              .executeWithOptions(_.enableAutoCancelableRunLoops)
            domTyp <- Task.eval(toTyp(domTerm))
            x = getNextVarName(vars, maxIndex(body)) :: domTyp
            value <- parse(body, x +: vars)
              .executeWithOptions(_.enableAutoCancelableRunLoops)
          } yield
            value match {
              case FormalAppln(fn, arg) if arg == x && fn.indepOf(x) =>
                pprint.log(fn)
                fn
              // case y if domain.prettyName.toString == "_" => y
              case _ =>
                // if (value.typ.dependsOn(x)) LambdaTerm(x, value)
                // else LambdaFixed(x, value)
                Try(lambda(x)(value)).fold(err => {
                  pprint.log(value)
                  pprint.log(x)
                  pprint.log(x.typ)
                  throw LambdaFormException(x, value, err)
                }, res => res)
            }
        case Pi(domain, body) =>
          for {
            domTerm <- parse(domain.ty, vars)
              .executeWithOptions(_.enableAutoCancelableRunLoops)
            domTyp <- Task
              .eval(toTyp(domTerm))
              .executeWithOptions(_.enableAutoCancelableRunLoops)
            x = getNextVarName(vars, maxIndex(body)) :: domTyp
            value <- parse(body, x +: vars)
              .executeWithOptions(_.enableAutoCancelableRunLoops)
            cod <- Task.eval(toTyp(value))
          } yield
            if (LeanInterface.usesVar(body, 0)) piDefn(x)(cod)
            else x.typ ->: cod
        case Let(domain, value, body) =>
          for {
            domTerm <- parse(domain.ty, vars)
              .executeWithOptions(_.enableAutoCancelableRunLoops)
            domTyp <- Task.eval(toTyp(domTerm))
            x = getNextVarName(vars, maxIndex(body)) :: domTyp
            valueTerm <- parse(value, vars)
              .executeWithOptions(_.enableAutoCancelableRunLoops)
            bodyTerm <- parse(body, x +: vars)
              .executeWithOptions(_.enableAutoCancelableRunLoops)
          } yield bodyTerm.replace(x, valueTerm)
        case e => Task.raiseError(UnParsedException(e))
      }

      for {
        res <- resTask
        _ = {
          parseWork -= exp
          log(Parsed(exp))
        }
      } yield res
      // if (isPropFmly(res.typ)) "_" :: (res.typ) else res
    }
  }.onErrorRecoverWith {
    case pe: ParseException =>
      Task.raiseError(ParseException(pe.expVars :+ (exp -> vars), pe.error))
    case error: Exception =>
      Task.raiseError(ParseException(Vector(exp -> vars), error))
  }

  def parseVec(vec: Vector[Expr], vars: Vector[Term]): Task[Vector[Term]] =
    vec match {
      case Vector() => Task.pure(Vector())
      case x +: ys =>
        for {
          head <- parse(x, vars).executeWithOptions(
            _.enableAutoCancelableRunLoops
          )
          tail <- parseVec(ys, vars).executeWithOptions(
            _.enableAutoCancelableRunLoops
          )
        } yield (head +: tail)
    }

  def parseOptVec(
      vec: Vector[(Expr, Int)],
      vars: Vector[Term],
      indices: Set[Int]
  ): Task[Vector[Option[Term]]] =
    vec match {
      case Vector() => Task.pure(Vector())
      case (x, m) +: ys =>
        for {
          tail <- parseOptVec(ys, vars, indices).executeWithOptions(
            _.enableAutoCancelableRunLoops
          )
          headOpt <- if (indices.contains(m))
            parse(x, vars)
              .executeWithOptions(_.enableAutoCancelableRunLoops)
              .map(Option(_))
          else Task.pure(None)
        } yield (headOpt +: tail)
    }


  def withDefn(name: Name, exp: Expr): Task[Unit] =
    for {
      term <- parse(exp, Vector())
        .executeWithOptions(_.enableAutoCancelableRunLoops)
      _ = {
        pprint.log(s"Defined $name"); log(Defined(name, term))
        defnMap += name -> term
      }
    } yield ()

  def withAxiom(name: Name, ty: Expr): Task[Unit] =
    for {
      typ <- parse(ty, Vector())
        .executeWithOptions(_.enableAutoCancelableRunLoops)
      term = (name.toString) :: toTyp(typ)
      _ = {
        pprint.log(s"Defined $name"); log(Defined(name, term))
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
  def foldAxiomSeq(
      accum: Vector[Term],
      axs: Vector[(Name, Expr)]
  ): Task[Vector[Term]] = axs match {
    case Vector() =>
      Task(accum)
    case (name, ty) +: ys =>
      for {
        typ <- parse(ty, Vector())
          .executeWithOptions(_.enableAutoCancelableRunLoops)
        // (typ, ltm1) = pr
        term = (name.toString) :: toTyp(typ)
        _ = {
          pprint.log(s"Defined $name"); log(Defined(name, term))
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
        indTypTerm <- parse(ind.ty, Vector())
          .executeWithOptions(_.enableAutoCancelableRunLoops)
        // (indTypTerm, ltm1) = pr
        indTyp = toTyp(indTypTerm)
        typF   = name.toString :: indTyp
        _ = {
          pprint.log(s"Defined $name"); log(Defined(name, typF))
          defnMap += name -> typF
        }
        intros <- foldAxiomSeq(Vector(), ind.intros).map(_.reverse)
        // (intros, withIntros) = introsPair
        typValuePair <- getValue(typF, ind.numParams, Vector())
        indMod = typValuePair match {
          case (_: Typ[Term], params) =>
            SimpleIndMod(ind.name, typF, intros, params.size, isPropn)
          case (_, params) =>
            IndexedIndMod(ind.name, typF, intros, params.size, isPropn)
        }
        _ = {
          log(DefinedInduc(name, indMod)); termIndModMap += ind.name -> indMod
        }
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

  val allIntros: ArrayBuffer[(Name, Expr)] = mods.collect {
    case ind: IndMod => ind.intros
  }.flatten

  def findDefMod(name: Name): Option[DefMod] =
    mods.collectFirst {
      case dm: DefMod if dm.name == name => dm
    }

  def findIndMod(name: Name): Option[IndMod] =
    mods collectFirst {
      case dm: IndMod if dm.name == name => dm
    }

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
    case Const(name @ Name.Str(_, "rec"), _) =>
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
    findMod(name, mods.toVector).map { (mod) =>
      // pprint.log(s"Using ${mod.name}")
      for {
        _ <- withMod(mod)
      } yield (defnMap(name))
    }

  def indModFromMod(name: Name): Option[Task[TermIndMod]] =
    findMod(name, mods.toVector).map { (mod) =>
      // pprint.log(s"Using ${mod.name}")
      for {
        _ <- withMod(mod)
      } yield (termIndModMap(name))
    }

  // code generation

  def defNames: ArrayBuffer[Name] = mods.collect { case df: DefMod => df.name }

  def allIndNames: ArrayBuffer[Name] = mods.collect {
    case ind: IndMod => ind.name
  }

  import translation.CodeGen

  def codeGen: CodeGen =
    CodeGen.objNames(defNames.map(_.toString).toVector, allIndNames.map(_.toString).toVector)

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
              .getExst(toTyp(foldFuncLean(mod.typF, p)), introsFold(mod, p))
              .value
          codeGen.consSeq(seq)
        case mod: IndexedIndMod =>
          val indSeq =
            TypFamilyExst
              .getIndexedConstructorSeq(
                foldFuncLean(mod.typF, p),
                introsFold(mod, p)
              )
              .value
          codeGen.indexedConsSeqDom(indSeq)
      }
    import scala.meta._
    val cp = p.map(codeGen(_).get)
    cp.foldRight(codeOpt.get) {
      case (x, y) =>
        meta.Term.Apply(
          meta.Term.Select(meta.Term.Name("Subst"), meta.Term.Name("Lambda")),
          List(x, y)
        )
      // q"Subst.Lambda($x, $y)"
    }
  }

}
