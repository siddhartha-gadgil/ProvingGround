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

object LeanParser {
  def splitVec[A](sizes: Vector[Int], vec: Vector[A]): (Vector[Vector[A]], Vector[A]) = sizes match {
    case Vector() => (Vector(), vec)
    case n +: ms =>
      val (head, tail) = vec.splitAt(n)
      val (prev, residue) = splitVec(ms, tail)
      (head +: prev, residue)
  }
}

class LeanParser(mods: Vector[Modification]) {
  import LeanParser._
  import LeanToTermMonix.{
    RecIterAp,
    getRec,
    applyFuncWit,
    applyFuncWitFold,
    isPropnFn,
    parseWork
  }

  parseWork.size

  val defnMap: mMap[Name, Term] = mMap()

  val termIndModMap: mMap[Name, TermIndMod] = mMap()

  def parse(exp: Expr, vars: Vector[Term] = Vector()): Task[Term] = {
    parseWork += exp
    def getNamed(name: Name) =
      defnMap.get(name).map((t) => Task.pure(t))
    def getTermIndMod(name: Name) =
      termIndModMap.get(name).map((t) => Task.pure(t))
    // pprint.log(s"Parsing $exp")
    // pprint.log(s"$parseWork")
    val resTask: Task[Term] = exp match {
      case Const(name, _) =>
        pprint.log(s"Seeking constant: $name")
        pprint.log(s"${defnMap.get(name).map(_.fansi)}")
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
        pprint.log(s"Seeking RecIterAp $name, $args")
        for {
          indMod <- getTermIndMod(name)
            .orElse(indModFromMod(name))
            .getOrElse(
              Task.raiseError(UnParsedException(exp))
            )
          // (indMod, ltm0) = pair0
          (argsFmly, xs) = args.splitAt(indMod.numParams + 1)
          argsFmlyTerm <- parseVec(argsFmly, vars)
          // (argsFmlyTerm, ltm1) = pair1
          recFnT = getRec(indMod, argsFmlyTerm)
          vec <- parseVec(xs, vars)
          // (vec, ltm2) = pair2
          resTask = applyFuncWitFold(recFnT, vec)
          res <- resTask
        } yield res

      case App(f, a) =>
        // pprint.log(s"Applying $f to $a")
        for {
          func <- parse(f, vars)
          // (func, ltm1) = p1
          arg <- parse(a, vars)
          // (arg, ltm2) = p2
          res = applyFuncWit(func, arg)
          // _ = pprint.log(s"got result for $f($a)")
        } yield res
      case Lam(domain, body) =>
        pprint.log(s"lambda $domain, $body")
        for {
          domTerm <- parse(domain.ty, vars)
          // (domTerm, ltm1) = p1
          domTyp <- Task.eval(toTyp(domTerm))
          x = domTyp.Var
          value <- parse(body, x +: vars)
          // (value, ltm2) = p2
        } yield
          value match {
            case FormalAppln(fn, arg) if arg == x && fn.indepOf(x) =>
              fn
            case y if domain.prettyName.toString == "_" => y
            case _ =>
              if (!LeanInterface.usesVar(body, 0))
                LambdaFixed("_" :: domTyp, value)
              else if (value.typ.dependsOn(x)) LambdaTerm(x, value)
              else LambdaFixed(x, value)
          }
      case Pi(domain, body) =>
        pprint.log(s"pi $domain, $body")
        for {
          domTerm <- parse(domain.ty, vars)
          // (domTerm, ltm1) = p1
          domTyp <- Task.eval(toTyp(domTerm))
          x = domTyp.Var
          value <- parse(body, x +: vars)
          // (value, ltm2) = p2
          cod <- Task.eval(toTyp(value))
          // dep =  cod.dependsOn(x)
        } yield
          if (LeanInterface.usesVar(body, 0))(PiDefn(x, cod))
          else (x.typ ->: cod)
      case Let(domain, value, body) =>
        pprint.log(s"let $domain, $value, $body")
        for {
          domTerm <- parse(domain.ty, vars)
          // (domTerm, ltm1) = p1
          domTyp <- Task.eval(toTyp(domTerm))
          x = domTyp.Var
          valueTerm <- parse(value, vars)
          // (valueTerm, ltm2) = p2
          bodyTerm <- parse(body, x +: vars)
          // (bodyTerm, ltm3) = p3
        } yield (bodyTerm.replace(x, valueTerm))
      case e => Task.raiseError(UnParsedException(e))
    }

    for {
      res <- resTask
      _ = {
        parseWork -= exp
        pprint.log(s"parsed $exp")
        if (isPropFmly(res.typ))
          pprint.log(s"\n\nWitness: ${res.fansi}\n\nprop: ${res.typ.fansi}\n\n")
      }
    } yield if (isPropFmly(res.typ)) "_" :: (res.typ) else res
  }

  def parseVec(vec: Vector[Expr], vars: Vector[Term]): Task[Vector[Term]] =
    vec match {
      case Vector() => Task.pure(Vector())
      case x +: ys =>
        for {
          head <- parse(x, vars)
          // (head, ltm1) = p1
          tail <- parseVec(ys, vars)
          // (tail, parse2) = p2
        } yield (head +: tail)
    }

  def parseOptVec(vec: Vector[(Expr, Int)], vars: Vector[Term], indices: Set[Int]): Task[Vector[Option[Term]]] =
    vec match {
      case Vector() => Task.pure(Vector())
      case (x, m) +: ys =>
        for {
          tail <- parseOptVec(ys, vars, indices)
          headOpt <- if (indices.contains(m)) parse(x, vars).map(Option(_)) else Task.pure(None)
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
      term <- parse(exp, Vector())
      // (term, ltm) = pr
      _ = { defnMap += name -> term }
    } yield ()

  def withAxiom(name: Name, ty: Expr): Task[Unit] =
    for {
      typ <- parse(ty, Vector())
      // (typ, ltm) = pr
      term = (name.toString) :: toTyp(typ)
      _    = { defnMap += name -> term }
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
        typ <- parse(ty, Vector())
        // (typ, ltm1) = pr
        term = (name.toString) :: toTyp(typ)
        _    = { defnMap += name -> term }
        res <- foldAxiomSeq(term +: accum, ys)
      } yield res
  }

  def withMod(mod: Modification): Task[Unit] = mod match {
    case ind: IndMod =>
      val isPropn = isPropnFn(ind.inductiveType.ty)
      val name    = ind.inductiveType.name
      for {
        indTypTerm <- parse(ind.inductiveType.ty, Vector())
        // (indTypTerm, ltm1) = pr
        indTyp = toTyp(indTypTerm)
        typF   = name.toString :: indTyp
        _      = { defnMap += name -> typF }
        intros <- foldAxiomSeq(Vector(), ind.intros)
        // (intros, withIntros) = introsPair
        typValuePair <- getValue(typF, ind.numParams, Vector())
        indMod = typValuePair match {
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
        _ = { termIndModMap += ind.name -> indMod }
      } yield ()

    case ax: AxiomMod =>
      withAxiom(ax.name, ax.ax.ty)
    case df: DefMod =>
      withDefn(df.name, df.defn.value)
    case QuotMod =>
      import quotient._
      val axs = Vector(quot, quotLift, quotMk, quotInd).map { (ax) =>
        (ax.name, ax.ty)
      }
      withAxiomSeq(axs)
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

  def defFromMod(name: Name): Option[Task[Term]] =
    findMod(name, mods).map { (mod) =>
      pprint.log(s"Using ${mod.name}")
      for {
        _ <- withMod(mod)
      } yield (defnMap(name))
    }

  def indModFromMod(name: Name): Option[Task[TermIndMod]] =
    findMod(name, mods).map { (mod) =>
      pprint.log(s"Using ${mod.name}")
      for {
        _ <- withMod(mod)
      } yield (termIndModMap(name))
    }

}
