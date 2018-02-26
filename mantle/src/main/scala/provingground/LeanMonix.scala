package provingground.interface
import provingground._
import induction._

import scala.concurrent._, duration._
import monix.execution.Scheduler.Implicits.global
import monix.eval._
import monix.reactive._
import monix.tail._
import cats._
import translation.FansiShow._
import scala.collection.mutable.{Set => mSet, Map => mMap}

import HoTT.{Name => _, _}

import trepplein._

object LeanToTermMonix {
  def isPropnFn(e: Expr): Boolean = e match {
    case Pi(_, t) => isPropnFn(t)
    case Sort(l)  => l == Level.Zero
    case _        => false
  }

  def proofLift: (Term, Term) => Task[Term] = {
    case (w: Typ[u], pt: PiDefn[x, y]) if pt.domain == w =>
      Task.pure(LambdaFixed(pt.variable, pt.value))
    case (w: Typ[u], tp: Typ[v]) => Task.eval { (w.Var) :-> tp }
    case (w: FuncLike[u, v], tp: FuncLike[a, b]) if w.dom == tp.dom =>
      val x = w.dom.Var
      proofLift(w(x), tp(x.asInstanceOf[a]))
        .map((g: Term) => x :~> (g: Term))
    case _ => throw new Exception("could not lift proof")
  }

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
      pprint.log(s"unifying for ${l.fansi} with value type ${l.value.typ} to type ${tp.fansi}")
      witUnify(l.value, tp)
    case (y, pd: PiDefn[u, v]) if isProp(pd.domain) =>
      pprint.log(s"unifying for ${pd.fansi} with value type ${pd.value.typ} to type ${y}")
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

  val applWork: mSet[(Term, Term)] = mSet()

  val parseWork: mSet[Expr] = mSet()

  def applyFuncWitOpt(f: Term, x: Term): Option[Term] = {
    // pprint.log(s"Application: ${f.fansi}, ${x.fansi}")
    func = f
    arg = x
    applWork += (f -> x)
    // pprint.log(applWork.map {case (f, x) => (f.fansi, x.fansi)})

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
          pprint.log(s"skipping application of ${x.fansi} to ${f.fansi} of typ ${f.typ.fansi}")
          Some(f)} else None
      )
    for {
      res <- resTask
      _ = {
        applWork -= (f -> x)
        // pprint.log(s"completed application:$f($x)")
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
        applyFuncWitFold(ft.map(
                           (f) => applyFuncWit(f, x)
                         ),
                         ys)
    }

  def applyFuncOptWitFold(ft: Task[Option[Term]],
                          v: Vector[Option[Term]]): Task[Option[Term]] =
    v match {
      case Vector() => ft
      case x +: ys =>
        applyFuncOptWitFold(ft.map(
                              (f) => applyOptFuncWit(f, x)
                            ),
                            ys)
    }

  def introsFold(ind: TermIndMod, p: Vector[Term]) =
    ind.intros.map((rule) => foldFunc(rule, p))

  def getRec(ind: TermIndMod, argsFmlyTerm: Vector[Term]): Task[Term] =
    ind match {
      case smp: SimpleIndMod =>
        // pprint.log(s"Getting rec using simple IndMod ${ind.name}")
        getRecSimple(smp, Task.pure(argsFmlyTerm))
      case indInd: IndexedIndMod =>
        // pprint.log(s"Getting rec using indexed IndMod ${ind.name}")
        getRecIndexed(indInd, Task.pure(argsFmlyTerm))
    }

  def getRecSimple(ind: SimpleIndMod,
                   argsFmlyTerm: Task[Vector[Term]]): Task[Term] = {
    def getInd(p: Vector[Term]) =
      ConstructorSeqTL
        .getExst(toTyp(foldFunc(ind.typF, p)), introsFold(ind, p))
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
              if (tp.dependsOn(l.variable))(indNew.inducE(
                (l.variable: Term) :-> (tp: Typ[u])))
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
        case pt: PiDefn[u, v] if ind.isPropn && pt.domain == indNew.typ =>
          indNew.inducE(LambdaFixed(pt.variable, pt.value))
        case tp: Typ[u] if (ind.isPropn) =>
          val x = tp.Var
          if (tp.dependsOn(x)) {
            (indNew.inducE((x: Term) :-> (tp: Typ[u])))
          } else (indNew.recE(tp))
      }

    }
  }

  def getRecIndexed(ind: IndexedIndMod,
                    argsFmlyTerm: Task[Vector[Term]]): Task[Term] = {
    def getInd(p: Vector[Term]) =
      TypFamilyExst
        .getIndexedConstructorSeq(foldFunc(ind.typFP, p), introsFold(ind, p))
        .value
    val newParamsTask = argsFmlyTerm map (_.init)
    newParamsTask.flatMap { (newParams) =>
      val indNew =
        getInd(newParams)
      val fmlOptRaw = argsFmlyTerm map (_.last)
      val fmlOpt =
        if (ind.isPropn)
          fmlOptRaw.flatMap((fib) => proofLift(indNew.W, fib))
        else fmlOptRaw
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

  type TaskParser = (Expr, Vector[Term]) => Task[Term]

  val empty = LeanToTermMonix(Map(), Map())

  def fromMods(mods: Vector[Modification], init: LeanToTermMonix = empty) =
    mods.foldLeft(Task.pure(init)) {
      case (l, m) => l.flatMap(_.add(m))
    }

  def addChunk(mods: Vector[Modification],
               init: Task[LeanToTermMonix] = Task.eval(empty),
               limit: FiniteDuration = 3.minutes) =
    mods
      .foldLeft(init) {
        case (l, m) =>
          l.flatMap(_.add(m).timeout(limit).onErrorRecoverWith {
            case err =>
              l
          })
      }
      .memoize

  def observable(mods: Vector[Modification],
                 init: LeanToTermMonix = empty,
                 limit: FiniteDuration = 5.minutes,
                 logErr: (Modification, Throwable) => Unit = (_, _) => {}) =
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

  def iterant(mods: Vector[Modification],
              init: LeanToTermMonix = empty,
              limit: FiniteDuration = 5.minutes,
              logErr: (Modification, Throwable) => Unit = (_, _) => {},
              recoverAll: Boolean = true) =
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

  object RecIterAp {
    def unapply(exp: Expr): Option[(Name, Vector[Expr])] = exp match {
      case Const(Name.Str(prefix, "rec"), _) => Some((prefix, Vector()))
      case App(func, arg) =>
        unapply(func).map { case (name, vec) => (name, vec :+ arg) }
      case _ => None
    }
  }

// internal parser
  def parse(exp: Expr,
            vars: Vector[Term],
            ltm: LeanToTermMonix,
            mods: Vector[Modification]): Task[(Term, LeanToTermMonix)] = {
    parseWork += exp
    def getNamed(name: Name) =
      ltm.defnMap.get(name).map((t) => Task.pure(t -> ltm))
    def getTermIndMod(name: Name) =
      ltm.termIndModMap.get(name).map((t) => Task.pure(t -> ltm))
    // pprint.log(s"Parsing $exp")
    pprint.log(s"$parseWork")
    val resTask: Task[(Term, LeanToTermMonix)] = exp match {
      case Const(name, _) =>
        // pprint.log(s"Seeking constant: $name")
        // pprint.log(s"${ltm.defnMap.get(name).map(_.fansi)}")
        getNamed(name)
          .orElse {
            // pprint.log(s"deffromMod $name")
            defFromMod(name, ltm, mods)
          }
          .getOrElse(
            Task.raiseError(UnParsedException(exp))
          )
      case Sort(Level.Zero) => Task.pure(Prop    -> ltm)
      case Sort(_)          => Task.pure(Type    -> ltm)
      case Var(n)           => Task.pure(vars(n) -> ltm)
      case RecIterAp(name, args) =>
        // pprint.log(s"Seeking RecIterAp $name, $args")
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
        // pprint.log(s"Applying $f to $a")
        for {
          p1 <- parse(f, vars, ltm, mods)
          (func, ltm1) = p1
          p2 <- parse(a, vars, ltm1, mods)
          (arg, ltm2) = p2
          res         = applyFuncWit(func, arg)
          // _ = pprint.log(s"got result for $f($a)")
        } yield (res, ltm2)
      case Lam(domain, body) =>
        // pprint.log(s"lambda $domain, $body")
        for {
          p1 <- parse(domain.ty, vars, ltm, mods)
          (domTerm, ltm1) = p1
          domTyp <- Task.eval(toTyp(domTerm))
          x = domTyp.Var
          p2 <- parse(body, x +: vars, ltm1, mods)
          (value, ltm2) = p2
        } yield
          value match {
            case FormalAppln(fn, arg) if arg == x && fn.indepOf(x) =>
              fn -> ltm2
            case y if domain.prettyName.toString == "_" => y -> ltm2
            case _ =>
              if (value.typ.dependsOn(x)) (LambdaTerm(x, value), ltm2)
              else (LambdaFixed(x, value), ltm2)
          }
      case Pi(domain, body) =>
        // pprint.log(s"pi $domain, $body")
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
        // pprint.log(s"let $domain, $value, $body")
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
        // pprint.log(s"parsed $exp")
      }
    } yield res
  }

  def parseVec(
      vec: Vector[Expr],
      vars: Vector[Term],
      ltm: LeanToTermMonix,
      mods: Vector[Modification]): Task[(Vector[Term], LeanToTermMonix)] =
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

  def withDefn(name: Name,
               exp: Expr,
               ltm: LeanToTermMonix,
               mods: Vector[Modification]) =
    for {
      pr <- parse(exp, Vector(), ltm, mods)
      (term, ltm) = pr
      modified    = ltm.addDefnMap(name, term)
    } yield modified

  def withAxiom(name: Name,
                ty: Expr,
                ltm: LeanToTermMonix,
                mods: Vector[Modification]) =
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

  def withMod(mod: Modification,
              ltm: LeanToTermMonix,
              mods: Vector[Modification]): Task[LeanToTermMonix] = mod match {
    case ind: IndMod =>
      val isPropn = isPropnFn(ind.inductiveType.ty)
      val name    = ind.inductiveType.name
      for {
        pr <- parse(ind.inductiveType.ty, Vector(), ltm, mods)
        (indTypTerm, ltm1) = pr
        indTyp             = toTyp(indTypTerm)
        typF               = name.toString :: indTyp
        withTyp            = ltm1.addDefnMap(name, typF)
        introsPair <- foldAxiomSeq(Vector(),
                                   ind.intros,
                                   Task.pure(withTyp),
                                   mods)
        (intros, withIntros) = introsPair
        typValuePar <- getValue(typF, ind.numParams, Vector())
        indMod = typValuePar match {
          case (typ: Typ[Term], params) =>
            SimpleIndMod(ind.inductiveType.name,
                         typF,
                         intros,
                         params.size,
                         isPropn)
          case (t, params) =>
            IndexedIndMod(ind.inductiveType.name,
                          typF,
                          intros,
                          params.size,
                          isPropn)
        }
      } yield
        LeanToTermMonix(withIntros.defnMap,
                        withIntros.termIndModMap + (ind.name -> indMod))
    case ax: AxiomMod =>
      withAxiom(ax.name, ax.ax.ty, ltm, mods)
    case df: DefMod =>
      withDefn(df.name, df.defn.value, ltm, mods)
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
    case mod =>
      Vector(mod.name)
  }

  def findMod(name: Name, mods: Vector[Modification]) =
    mods.find((mod) => modNames(mod).contains(name))

  def defFromMod(
      name: Name,
      ltm: LeanToTermMonix,
      mods: Vector[Modification]): Option[Task[(Term, LeanToTermMonix)]] =
    findMod(name, mods).map { (mod) =>
      pprint.log(s"Using ${mod.name}")
      for {
        ltm1 <- withMod(mod, ltm, mods)
      } yield (ltm1.defnMap(name), ltm1)
    }

  def indModFromMod(
      name: Name,
      ltm: LeanToTermMonix,
      mods: Vector[Modification]): Option[Task[(TermIndMod, LeanToTermMonix)]] =
    findMod(name, mods).map { (mod) =>
      for {
        ltm1 <- withMod(mod, ltm, mods)
      } yield (ltm1.termIndModMap(name), ltm1)
    }

}

case class RecFoldException(indMod: TermIndMod,
                            recFn: Term,
                            argsFmlyTerm: Vector[Term],
                            vec: Vector[Term],
                            fail: ApplnFailException)
    extends IllegalArgumentException("Failure to fold recursive Function")

case class LeanToTermMonix(defnMap: Map[Name, Term],
                           termIndModMap: Map[Name, TermIndMod]) { self =>
  import LeanToTermMonix._

  def defnOpt(exp: Expr) =
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
                throw RecFoldException(indMod, recFn, argsFmlyTerm, vec, err)
            }
          res <- resTask
        } yield res

      case App(f, a) =>
        Task
          .defer(parse(f, vars))
          .zipMap(Task.defer(parse(a, vars)))(applyFuncWit)
      case Lam(domain, body) =>
        for {
          domTerm <- parse(domain.ty, vars)
          domTyp  <- Task.eval(toTyp(domTerm))
          x = domTyp.Var
          value <- parse(body, x +: vars)
        } yield
          value match {
            case FormalAppln(fn, arg) if arg == x && fn.indepOf(x) => fn
            case y if domain.prettyName.toString == "_"            => y
            case _ =>
              if (value.typ.dependsOn(x)) LambdaTerm(x, value)
              else LambdaFixed(x, value)
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
    Task.gather {
      vec.map(parse(_, vars))
    }

  def parseTypVec(vec: Vector[Expr],
                  vars: Vector[Term]): Task[Vector[Typ[Term]]] =
    Task.gather {
      vec.map(parseTyp(_, vars))
    }

  def parseSymVec(vec: Vector[(Name, Expr)],
                  vars: Vector[Term]): Task[Vector[(Name, Term)]] =
    Task.gather {
      for {
        (name, expr) <- vec
      } yield
        for {
          tp <- parseTyp(expr, vars)
        } yield (name, name.toString :: tp)
    }

  def parseSym(name: Name, ty: Expr, vars: Vector[Term]) =
    parseTyp(ty, vars).map(name.toString :: _)

  def parseVar(b: Binding, vars: Vector[Term]) =
    parseSym(b.prettyName, b.ty, vars)

  def addDefnMap(name: Name, term: Term) =
    self.copy(defnMap = self.defnMap + (name -> term))

  def addDefnVal(name: Name, value: Expr, tp: Expr) =
    parse(value, Vector())
      .map((t) => addDefnMap(name, t))

  def addAxiom(name: Name, ty: Expr) =
    parseSym(name, ty, Vector())
      .map(addDefnMap(name, _))

  def addAxioms(axs: Vector[(Name, Expr)]) = {
    val taskVec = axs.map {
      case (name, ty) => parseSym(name, ty, Vector()).map((t) => (name, t))
    }
    val mvec = Task.gather(taskVec)
    mvec.map((vec) => self.copy(defnMap = self.defnMap ++ vec))
  }

  def addAxiomSeq(axs: Vector[(Name, Expr)]): Task[LeanToTermMonix] =
    axs.foldLeft(Task.eval(self)) {
      case (p, (n, v)) => p.flatMap(_.addAxiom(n, v))
    }

  def addAxiomMod(ax: AxiomMod): Task[LeanToTermMonix] =
    addAxiom(ax.name, ax.ax.ty)

  def addDefMod(df: DefMod): Task[LeanToTermMonix] =
    addDefnVal(df.name, df.defn.value, df.defn.ty)

  def addQuotMod: Task[LeanToTermMonix] = {
    import quotient._
    val axs = Vector(quot, quotLift, quotMk, quotInd).map { (ax) =>
      (ax.name, ax.ty)
    }
    addAxiomSeq(axs)
  }

  def addIndMod(ind: IndMod): Task[LeanToTermMonix] = {
    val name    = ind.inductiveType.name
    val isPropn = isPropnFn(ind.inductiveType.ty)
    for {
      inductiveTyp <- parseTyp(ind.inductiveType.ty, Vector())
      withTypDef = addDefnMap(ind.name, name.toString :: inductiveTyp)
      namedIntros <- withTypDef.parseSymVec(ind.intros, Vector())
      intros = namedIntros.map(_._2)
      withAxioms <- withTypDef.addAxioms(ind.intros)
      typF = name.toString :: inductiveTyp
      typValue <- getValue(typF, ind.numParams, Vector())
      indMod = typValue match {
        case (typ: Typ[Term], params) =>
          SimpleIndMod(ind.inductiveType.name,
                       typF,
                       intros,
                       params.size,
                       isPropn)
        case (t, params) =>
          IndexedIndMod(ind.inductiveType.name,
                        typF,
                        intros,
                        params.size,
                        isPropn)
      }
    } yield
      LeanToTermMonix(withTypDef.defnMap ++ namedIntros,
                      termIndModMap + (ind.name -> indMod))
  }

  def add(mod: Modification): Task[LeanToTermMonix] = mod match {
    case ind: IndMod  => addIndMod(ind)
    case ax: AxiomMod => addAxiomMod(ax)
    case df: DefMod   => addDefMod(df)
    case QuotMod      => addQuotMod
  }

}
