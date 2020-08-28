package provingground.interface
import provingground._
import induction._

import scala.concurrent._, duration._
//import monix.execution.Scheduler.Implicits.global
import monix.eval._
import monix.reactive._
import monix.tail._
//import cats._
import translation.FansiShow._
import scala.collection.mutable

import HoTT.{Name => _, _}

import trepplein._
import LeanInterface._, LeanParser._

@deprecated("use LeanParser", "buggy, avoid")
object LeanToTermMonix {


  def feedWit(t: Term): Option[Term] = t match {
    case f: FuncLike[u, v] if (isProp(f.dom)) =>
      pprint.log(s"feeding witness to ${t.fansi} of type ${t.typ.fansi}")
      Some(f("witness" :: f.dom))
    case _ => None
  }

  def witUnify(x: Term, typ: Typ[Term]): Option[Term] = (x, typ) match {
    case (y, t) if y.typ == t => Some(y)
    case (l: LambdaLike[u, v], pd: PiDefn[a, b]) if l.dom == pd.domain =>
      witUnify(l.value, pd.value.replace(pd.variable, l.variable))
        .map(lambda(l.variable)(_))
    case (l: LambdaLike[u, v], tp) if isProp(l.dom) =>
      pprint.log(
        s"unifying for ${l.fansi} with value type ${l.value.typ} to type ${tp.fansi}"
      )
      witUnify(l.value, tp)
    case (y, pd: PiDefn[u, v]) if isProp(pd.domain) =>
      pprint.log(
        s"unifying for ${pd.fansi} with value type ${pd.value.typ} to type $y"
      )
      witUnify(y, pd.value).map(lambda(pd.variable)(_))
    case (l: LambdaLike[u, v], tp) =>
      for {
        v <- feedWit(l.variable)
        ll = lambda(v)(l.value.replace(l.variable, v))
        res <- witUnify(ll, tp)
      } yield res
    case (y, t) =>
      None
  }

  def applyWitUnify(f: Term, x: Term): Option[Term] = f match {
    case fn: FuncLike[u, v] =>
      for {
        arg <- witUnify(x, fn.dom)
        res <- applyFuncWitOpt(fn, arg)
      } yield res
    case _ => None
  }

  var func: Term = _

  var arg: Term = _

  val applWork: mutable.Set[(Term, Term)] = mutable.Set()

  def applyFuncWitOpt(f: Term, x: Term): Option[Term] = {
    func = f
    arg = x
    applWork += (f -> x)

    val resTask = applyFuncOpt(f, x)
      .orElse(
        feedWit(f).flatMap(applyFuncWitOpt(_, x))
      )
      .orElse(
        feedWit(x).flatMap(applyFuncWitOpt(f, _))
      )
      .orElse(applyWitUnify(f, x))
      .orElse(
        if (isProp(x.typ)) {
          pprint.log(
            s"skipping application of ${x.fansi} to ${f.fansi} of typ ${f.typ.fansi}"
          )
          Some(f)
        } else None
      )
    for {
      res <- resTask
      _ = {
        applWork -= (f -> x)
      }
    } yield res
  }

  def applyFuncWit(f: Term, x: Term): Term =
    applyFuncWitOpt(f, x).getOrElse {
      throw new ApplnFailException(f, x)
    }

  def applyOptFuncWit(fo: Option[Term], xo: Option[Term]): Option[Term] =
    xo.flatMap { (x) =>
        fo.flatMap((f) => applyFuncWitOpt(f, x))
      }
      .orElse {
        fo.collect { case l: LambdaLike[u, v] => l.value }
      }

  def applyFuncWitFold(ft: Task[Term], v: Vector[Term]): Task[Term] =
    v match {
      case Vector() => ft
      case x +: ys =>
        applyFuncWitFold(
          ft.map(
            (f) => applyFuncWit(f, x)
          ),
          ys
        )
    }

  def applyFuncOptWitFold(
      ft: Task[Option[Term]],
      v: Vector[Option[Term]]
  ): Task[Option[Term]] =
    v match {
      case Vector() => ft
      case x +: ys =>
        applyFuncOptWitFold(
          ft.map(
            (f) => applyOptFuncWit(f, x)
          ),
          ys
        )
    }


  type TaskParser = (Expr, Vector[Term]) => Task[Term]

  val empty = LeanToTermMonix(Map(), Map())

  def fromMods(
      mods: Vector[Modification],
      init: LeanToTermMonix = empty
  ): Task[LeanToTermMonix] =
    mods.foldLeft(Task.pure(init)) {
      case (l, m) => l.flatMap(_.add(m))
    }

  def addChunk(
      mods: Vector[Modification],
      init: Task[LeanToTermMonix] = Task.eval(empty),
      limit: FiniteDuration = 3.minutes
  ): Task[LeanToTermMonix] =
    mods
      .foldLeft(init) {
        case (l, m) =>
          l.flatMap(_.add(m).timeout(limit).onErrorRecoverWith {
            case err =>
              l
          })
      }
      .memoize

  def observable(
      mods: Vector[Modification],
      init: LeanToTermMonix = empty,
      limit: FiniteDuration = 5.minutes,
      logErr: (Modification, Throwable) => Unit = (_, _) => {}
  ): Observable[LeanToTermMonix] =
    Observable.fromIterable(mods).flatScan[LeanToTermMonix](init) {
      case (l, m) =>
        Observable.fromTask(
          l.add(m).timeout(limit).onErrorRecoverWith {
            case err =>
              logErr(m, err)
              Task.eval(l)
          }
        )
    }

  def iterant(
      mods: Vector[Modification],
      init: LeanToTermMonix = empty,
      limit: FiniteDuration = 5.minutes,
      logErr: (Modification, Throwable) => Unit = (_, _) => {},
      recoverAll: Boolean = true
  ): Iterant[Task, LeanToTermMonix] =
    Iterant
      .fromIterable[Task, Modification](mods)
      .scanEval[LeanToTermMonix](Task.pure(init)) {
        case (l, m) =>
          l.add(m).timeout(limit).onErrorRecoverWith {
            case err if recoverAll =>
              logErr(m, err)
              Task.pure(l)
            case err: TimeoutException =>
              logErr(m, err)
              Task.pure(l)
            case err: UnParsedException =>
              logErr(m, err)
              Task.pure(l)

          }
      }

// internal parser
  def parse(
      exp: Expr,
      vars: Vector[Term],
      ltm: LeanToTermMonix,
      mods: Vector[Modification]
  ): Task[(Term, LeanToTermMonix)] = {
    parseWork += exp
    def getNamed(name: Name) =
      ltm.defnMap.get(name).map((t) => Task.pure(t -> ltm))
    def getTermIndMod(name: Name) =
      ltm.termIndModMap.get(name).map((t) => Task.pure(t -> ltm))
    pprint.log(s"$parseWork")
    val resTask: Task[(Term, LeanToTermMonix)] = exp match {
      case Const(name, _) =>
        getNamed(name)
          .orElse {
            defFromMod(name, ltm, mods)
          }
          .getOrElse(
            Task.raiseError(UnParsedException(exp))
          )
      case Sort(Level.Zero)      => Task.pure(Prop -> ltm)
      case Sort(_)               => Task.pure(Type -> ltm)
      case Var(n)                => Task.pure(vars(n) -> ltm)
      case RecIterAp(name, args) =>
        for {
          pair0 <- getTermIndMod(name)
            .orElse(indModFromMod(name, ltm, mods))
            .getOrElse(
              Task.raiseError(UnParsedException(exp))
            )
          (indMod, ltm0) = pair0
          (argsFmly, xs) = args.splitAt(indMod.numParams + 1)
          pair1 <- parseVec(argsFmly, vars, ltm0, mods)
          (argsFmlyTerm, ltm1) = pair1
          recFnT               = getRec(indMod, argsFmlyTerm)
          pair2 <- parseVec(xs, vars, ltm1, mods)
          (vec, ltm2) = pair2
          resTask     = applyFuncWitFold(recFnT, vec)
          res <- resTask
        } yield (res, ltm2)

      case App(f, a) =>
        for {
          p1 <- parse(f, vars, ltm, mods)
          (func, ltm1) = p1
          p2 <- parse(a, vars, ltm1, mods)
          (arg, ltm2) = p2
          res         = applyFuncWit(func, arg)
        } yield (res, ltm2)
      case Lam(domain, body) =>
        for {
          p1 <- parse(domain.ty, vars, ltm, mods)
          (domTerm, ltm1) = p1
          domTyp <- Task.eval(toTyp(domTerm))
          x = domTyp.Var
          p2 <- parse(body, x +: vars, ltm1, mods)
          (value, ltm2) = p2
        } yield
          value match {
            case FormalAppln(fn, argu) if argu == x && fn.indepOf(x) =>
              fn -> ltm2
            case y if domain.prettyName.toString == "_" => y -> ltm2
            case _ =>
              (lambda(x)(value), ltm2)
            // if (value.typ.dependsOn(x)) (LambdaTerm(x, value), ltm2)
            // else (LambdaFixed(x, value), ltm2)
          }
      case Pi(domain, body) =>
        for {
          p1 <- parse(domain.ty, vars, ltm, mods)
          (domTerm, ltm1) = p1
          domTyp <- Task.eval(toTyp(domTerm))
          x = domTyp.Var
          p2 <- parse(body, x +: vars, ltm1, mods)
          (value, ltm2) = p2
          cod <- Task.eval(toTyp(value))
          dep = cod.dependsOn(x)
        } yield if (dep) (PiDefn(x, cod), ltm2) else (x.typ ->: cod, ltm2)
      case Let(domain, value, body) =>
        for {
          p1 <- parse(domain.ty, vars, ltm, mods)
          (domTerm, ltm1) = p1
          domTyp <- Task.eval(toTyp(domTerm))
          x = domTyp.Var
          p2 <- parse(value, vars, ltm1, mods)
          (valueTerm, ltm2) = p2
          p3 <- parse(body, x +: vars, ltm2, mods)
          (bodyTerm, ltm3) = p3
        } yield (bodyTerm.replace(x, valueTerm), ltm3)
      case e => Task.raiseError(UnParsedException(e))
    }

    for {
      res <- resTask
      _ = {
        parseWork -= exp
      }
    } yield res
  }

  def parseVec(
      vec: Vector[Expr],
      vars: Vector[Term],
      ltm: LeanToTermMonix,
      mods: Vector[Modification]
  ): Task[(Vector[Term], LeanToTermMonix)] =
    vec match {
      case Vector() => Task.pure(Vector() -> ltm)
      case x +: ys =>
        for {
          p1 <- parse(x, vars, ltm, mods)
          (head, ltm1) = p1
          p2 <- parseVec(ys, vars, ltm1, mods)
          (tail, parse2) = p2
        } yield (head +: tail, parse2)
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

  def withDefn(
      name: Name,
      exp: Expr,
      ltm: LeanToTermMonix,
      mods: Vector[Modification]
  ): Task[LeanToTermMonix] =
    for {
      pr <- parse(exp, Vector(), ltm, mods)
      (term, ltm) = pr
      modified    = ltm.addDefnMap(name, term)
    } yield modified

  def withAxiom(
      name: Name,
      ty: Expr,
      ltm: LeanToTermMonix,
      mods: Vector[Modification]
  ): Task[LeanToTermMonix] =
    for {
      pr <- parse(ty, Vector(), ltm, mods)
      (typ, ltm) = pr
      term       = (name.toString) :: toTyp(typ)
      modified   = ltm.addDefnMap(name, term)
    } yield modified

  def withAxiomSeq(
      axs: Vector[(Name, Expr)],
      ltmT: Task[LeanToTermMonix],
      mods: Vector[Modification]
  ): Task[LeanToTermMonix] = axs match {
    case Vector() => ltmT
    case (name, ty) +: ys =>
      for {
        ltm <- ltmT
        ltmH = withAxiom(name, ty, ltm, mods)
        res <- withAxiomSeq(ys, ltmH, mods)
      } yield res
  }

  // Like withAxiomSeq but returns the axioms
  def foldAxiomSeq(
      accum: Vector[Term],
      axs: Vector[(Name, Expr)],
      ltmT: Task[LeanToTermMonix],
      mods: Vector[Modification]
  ): Task[(Vector[Term], LeanToTermMonix)] = axs match {
    case Vector() =>
      for { ltm <- ltmT } yield (accum, ltm)
    case (name, ty) +: ys =>
      for {
        ltm <- ltmT
        pr  <- parse(ty, Vector(), ltm, mods)
        (typ, ltm1) = pr
        term        = (name.toString) :: toTyp(typ)
        modified    = ltm1.addDefnMap(name, term)
        res <- foldAxiomSeq(term +: accum, ys, Task.pure(modified), mods)
      } yield res
  }

  def withMod(
      mod: Modification,
      ltm: LeanToTermMonix,
      mods: Vector[Modification]
  ): Task[LeanToTermMonix] = mod match {
    case ind: IndMod =>
      val isPropn = isPropnFn(ind.ty)
      val name    = ind.name
      for {
        pr <- parse(ind.ty, Vector(), ltm, mods)
        (indTypTerm, ltm1) = pr
        indTyp             = toTyp(indTypTerm)
        typF               = name.toString :: indTyp
        withTyp            = ltm1.addDefnMap(name, typF)
        introsPair <- foldAxiomSeq(
          Vector(),
          ind.intros,
          Task.pure(withTyp),
          mods
        )
        (intros, withIntros) = introsPair
        typValuePar <- getValue(typF, ind.numParams, Vector())
        indMod = typValuePar match {
          case (typ: Typ[Term], params) =>
            SimpleIndMod(ind.name, typF, intros, params.size, isPropn)
          case (t, params) =>
            IndexedIndMod(ind.name, typF, intros, params.size, isPropn)
        }
      } yield
        LeanToTermMonix(
          withIntros.defnMap,
          withIntros.termIndModMap + (ind.name -> indMod)
        )
    case ax: AxiomMod =>
      withAxiom(ax.name, ax.ty, ltm, mods)
    case df: DefMod =>
      withDefn(df.name, df.value, ltm, mods)
    case QuotMod =>
      import quotient._
      val axs = Vector(quot, quotLift, quotMk, quotInd).map { (ax) =>
        (ax.name, ax.ty)
      }
      withAxiomSeq(axs, Task.pure(ltm), mods)
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

  def findMod(name: Name, mods: Vector[Modification]): Option[Modification] =
    mods.find((mod) => modNames(mod).contains(name))

  def defFromMod(
      name: Name,
      ltm: LeanToTermMonix,
      mods: Vector[Modification]
  ): Option[Task[(Term, LeanToTermMonix)]] =
    findMod(name, mods).map { (mod) =>
      pprint.log(s"Using ${mod.name}")
      for {
        ltm1 <- withMod(mod, ltm, mods)
      } yield (ltm1.defnMap(name), ltm1)
    }

  def indModFromMod(
      name: Name,
      ltm: LeanToTermMonix,
      mods: Vector[Modification]
  ): Option[Task[(TermIndMod, LeanToTermMonix)]] =
    findMod(name, mods).map { (mod) =>
      for {
        ltm1 <- withMod(mod, ltm, mods)
      } yield (ltm1.termIndModMap(name), ltm1)
    }

}


@deprecated("use LeanParser", "buggy, avoid")
    case class LeanToTermMonix(
    defnMap: Map[Name, Term],
    termIndModMap: Map[Name, TermIndMod]
) { self =>
  import LeanToTermMonix._

  def defnOpt(exp: Expr): Option[Term] =
    exp match {
      case Const(name, _) => defnMap.get(name)
      case _              => None
    }

  object Predef {
    def unapply(exp: Expr): Option[Term] =
      (
        defnOpt(exp)
      )
  }

  def parse(exp: Expr, vars: Vector[Term]): Task[Term] =
    exp match {
      case Predef(t)        => Task.eval(t)
      case Sort(Level.Zero) => Task.eval(Prop)
      case Sort(_)          => Task.eval(Type)
      case Var(n)           => Task.eval(vars(n))
      case RecIterAp(name, args) =>
        val indMod         = termIndModMap(name)
        val (argsFmly, xs) = args.splitAt(indMod.numParams + 1)

        for {
          argsFmlyTerm <- parseVec(argsFmly, vars)
          recFnT = getRec(indMod, argsFmlyTerm)
          recFn <- recFnT
          vec   <- parseVec(xs, vars)
          resTask = applyFuncWitFold(recFnT, vec)
            .onErrorRecoverWith {
              case err: ApplnFailException =>
                throw RecFoldException(indMod, argsFmly, recFn, argsFmlyTerm, vec, err)
            }
          res <- resTask
        } yield res

      case App(f, a) =>
        Task
          .parZip2(
            Task
              .defer(parse(f, vars)),
            Task.defer(parse(a, vars))
          )
          .map { case (fn, x) => applyFuncWit(fn, x) }
      case Lam(domain, body) =>
        for {
          domTerm <- parse(domain.ty, vars)
          domTyp  <- Task.eval(toTyp(domTerm))
          x = domTyp.Var
          value <- parse(body, x +: vars)
        } yield
          value match {
            case FormalAppln(fn, argu) if argu == x && fn.indepOf(x) => fn
            case y if domain.prettyName.toString == "_"              => y
            case _ =>
              lambda(x)(value)
            // if (value.typ.dependsOn(x)) LambdaTerm(x, value)
            // else LambdaFixed(x, value)
          }
      case Pi(domain, body) =>
        for {
          domTerm <- parse(domain.ty, vars)
          domTyp  <- Task.eval(toTyp(domTerm))
          x = domTyp.Var
          value <- parse(body, x +: vars)
          cod   <- Task.eval(toTyp(value))
          dep = cod.dependsOn(x)
        } yield if (dep) PiDefn(x, cod) else x.typ ->: cod
      case Let(domain, value, body) =>
        for {
          domTerm <- parse(domain.ty, vars)
          domTyp  <- Task.eval(toTyp(domTerm))
          x = domTyp.Var
          valueTerm <- parse(value, vars)
          bodyTerm  <- parse(body, x +: vars)
        } yield bodyTerm.replace(x, valueTerm)
      case e => Task.raiseError(UnParsedException(e))
    }

  def parseTyp(x: Expr, vars: Vector[Term]): Task[Typ[Term]] =
    parse(x, vars).flatMap {
      case tp: Typ[_] => Task.eval(tp)
      case t =>
        Task.raiseError(NotTypeException(t))
    }

  def parseVec(vec: Vector[Expr], vars: Vector[Term]): Task[Vector[Term]] =
    Task.parSequence {
      vec.map(parse(_, vars))
    }

  def parseTypVec(
      vec: Vector[Expr],
      vars: Vector[Term]
  ): Task[Vector[Typ[Term]]] =
    Task.parSequence {
      vec.map(parseTyp(_, vars))
    }

  def parseSymVec(
      vec: Vector[(Name, Expr)],
      vars: Vector[Term]
  ): Task[Vector[(Name, Term)]] =
    Task.parSequence {
      for {
        (name, expr) <- vec
      } yield
        for {
          tp <- parseTyp(expr, vars)
        } yield (name, name.toString :: tp)
    }

  def parseSym(name: Name, ty: Expr, vars: Vector[Term]): Task[Term] =
    parseTyp(ty, vars).map(name.toString :: _)

  def parseVar(b: Binding, vars: Vector[Term]): Task[Term] =
    parseSym(b.prettyName, b.ty, vars)

  def addDefnMap(name: Name, term: Term): LeanToTermMonix =
    self.copy(defnMap = self.defnMap + (name -> term))

  def addDefnVal(name: Name, value: Expr, tp: Expr): Task[LeanToTermMonix] =
    parse(value, Vector())
      .map((t) => addDefnMap(name, t))

  def addAxiom(name: Name, ty: Expr): Task[LeanToTermMonix] =
    parseSym(name, ty, Vector())
      .map(addDefnMap(name, _))

  def addAxioms(axs: Vector[(Name, Expr)]): Task[LeanToTermMonix] = {
    val taskVec = axs.map {
      case (name, ty) => parseSym(name, ty, Vector()).map((t) => (name, t))
    }
    val mvec = Task.parSequence(taskVec)
    mvec.map((vec) => self.copy(defnMap = self.defnMap ++ vec))
  }

  def addAxiomSeq(axs: Vector[(Name, Expr)]): Task[LeanToTermMonix] =
    axs.foldLeft(Task.eval(self)) {
      case (p, (n, v)) => p.flatMap(_.addAxiom(n, v))
    }

  def addAxiomMod(ax: AxiomMod): Task[LeanToTermMonix] =
    addAxiom(ax.name, ax.ty)

  def addDefMod(df: DefMod): Task[LeanToTermMonix] =
    addDefnVal(df.name, df.value, df.ty)

  def addQuotMod: Task[LeanToTermMonix] = {
    import quotient._
    val axs = Vector(quot, quotLift, quotMk, quotInd).map { (ax) =>
      (ax.name, ax.ty)
    }
    addAxiomSeq(axs)
  }

  def addIndMod(ind: IndMod): Task[LeanToTermMonix] = {
    val name    = ind.name
    val isPropn = isPropnFn(ind.ty)
    for {
      inductiveTyp <- parseTyp(ind.ty, Vector())
      withTypDef = addDefnMap(ind.name, name.toString :: inductiveTyp)
      namedIntros <- withTypDef.parseSymVec(ind.intros, Vector())
      intros = namedIntros.map(_._2)
      withAxioms <- withTypDef.addAxioms(ind.intros)
      typF = name.toString :: inductiveTyp
      typValue <- getValue(typF, ind.numParams, Vector())
      indMod = typValue match {
        case (typ: Typ[Term], params) =>
          SimpleIndMod(ind.name, typF, intros, params.size, isPropn)
        case (t, params) =>
          IndexedIndMod(ind.name, typF, intros, params.size, isPropn)
      }
    } yield
      LeanToTermMonix(
        withTypDef.defnMap ++ namedIntros,
        termIndModMap + (ind.name -> indMod)
      )
  }

  def add(mod: Modification): Task[LeanToTermMonix] = mod match {
    case ind: IndMod  => addIndMod(ind)
    case ax: AxiomMod => addAxiomMod(ax)
    case df: DefMod   => addDefMod(df)
    case QuotMod      => addQuotMod
  }

}
