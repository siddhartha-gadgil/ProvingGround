package provingground.interface
import provingground._

import ammonite.ops._
import scala.util.Try

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global

import HoTT.{Name => _, _}

import translation.{TermLang => TL}
import trepplein._

case class LeanToTerm(defns: Expr => Option[Term], vars: Vector[Term]) {
  parser =>
  def parseTyped(exp: Expr, typOpt: Option[Typ[Term]] = None): Option[Term] =
    defns(exp).orElse(
      exp match {
        case App(x, y) =>
          val domOpt = typOpt.flatMap(TL.domTyp)
          for {
            arg  <- parseTyped(y, domOpt)
            func <- parseTyped(x) // cannot infer type
            res  <- TL.appln(func, arg)
          } yield res
        case Sort(_) => Some(Type)
        case Lam(domain, body) =>
          val xo = parseVar(domain)
          for {
            x   <- xo
            pb  <- addVar(x).parseTyped(body)
            res <- TL.lambda(x, pb)
          } yield res
        case Pi(domain, body) =>
          val xo = parseVar(domain)
          def getCodom(t: Term): Option[Typ[Term]] =
            typOpt match {
              case Some(fn: FuncLike[u, v]) if fn.dom == t.typ =>
                Some(fn.depcodom(t.asInstanceOf[u]))
              case _ => None
            }
          for {
            x   <- xo
            pb  <- addVar(x).parseTyped(body, getCodom(x))
            res <- TL.pi(x, pb)
          } yield res
        case Let(domain, value, body) =>
          val xo = parseVar(domain)
          for {
            x       <- xo
            pb      <- addVar(x).parseTyped(body, typOpt)
            valTerm <- parseTyped(body, parseTyp(domain.ty))
          } yield pb.replace(x, valTerm)
        case LocalConst(b, _, Some(tp)) => parseSym(b.prettyName, tp)
        case Var(n)                     => Some(vars(n))
        case c: Const =>
          println(s"Constant $c undefined")
          None
        case _ =>
          // println(s"failed to parse $exp")
          None
      }
    )

  def addVar(t: Term) = LeanToTerm(defns, t +: vars)

  def parseTyp(x: Expr) =
    parseTyped(x, Some(Type)).collect { case tp: Typ[_] => tp }

  def parseSym(name: Name, ty: Expr) =
    parseTyp(ty).map(name.toString :: _)

  def parseVar(b: Binding) = parseSym(b.prettyName, b.ty)

  /**
    * add several definitions given as an option valued function
    */
  def addDefns(dfn: Expr => Option[Term]) =
    LeanToTerm((exp) => dfn(exp) orElse defns(exp), vars)

  def mkAxiom(name: Name, ty: Expr): Expr => Option[Term] = {
    case Const(`name`, _) => parseSym(name, ty)
    case _                => None
  }

  def mkDef(name: Name, value: Expr): Expr => Option[Term] = {
    case Const(`name`, _) => parseTyped(value)
    case _                => None
  }

  def addAxiomMod(ax: AxiomMod) = addDefns(mkDef(ax.name, ax.ax.ty))

  def addDefMod(df: DefMod) = addDefns(mkDef(df.name, df.defn.value))

  def addQuotMod = {
    import quotient._
    val axs = Vector(quot, quotLift, quotMk, quotInd).map { (ax) =>
      mkAxiom(ax.name, ax.ty)
    }
    axs.foldLeft(parser)(_.addDefns(_))
  }

  def addIndModConsts(ind: IndMod) = {
    val axs = { (ind.name -> ind.inductiveType.ty) +: ind.intros }.map {
      case (n, t) => mkAxiom(n, t)
    }
    axs.foldLeft(parser)(_.addDefns(_))
  }
}
import induction._, shapeless.{Path => _, _}

import translation.TermLang.{appln, domTyp}

object LeanToTerm {
  def iterAp(name: Name, length: Int): Expr => Option[Vector[Expr]] = {
    case c @ Const(`name`, _) if length == 0 => Some(Vector(c))
    case App(f, z)                           => iterAp(name, length - 1)(f).map(_ :+ z)
    case _                                   => None
  }

  import ConstructorShape._

  /**
    * Data for recursively defining a type family corresponding to a constructor
    * this is the type family for an inductive definition
    */
  def typData[S <: HList, ConstructorType <: Term with Subs[ConstructorType]](
      pattern: ConstructorShape[S, Term, ConstructorType],
      data: Term,
      typ: Typ[Term]): Option[Term] =
    pattern match {
      case _: IdShape[Term] => Some(data.typ)
      case shape: CnstDepFuncConsShape[a, Term, c, d, e] =>
        val x = shape.tail.Var
        for {
          y   <- appln(data, x)
          rec <- typData(shape.headfibre(x), y, typ)
        } yield x :~> rec
      case shape: CnstFuncConsShape[a, Term, c, d, e] =>
        val x = shape.tail.Var
        for {
          y   <- appln(data, x)
          rec <- typData(shape.head, y, typ)
        } yield x :-> rec
      case shape: FuncConsShape[a, Term, c, d] =>
        val x = shape.tail(typ).Var
        val A = Type.Var
        for {
          fx  <- appln(data, x)
          dom <- domTyp(fx)
          y = dom.Var
          g   <- appln(fx, y)
          rec <- typData(shape.head, g, typ)
        } yield x :-> A :-> rec
    }

  def typDataVec[SS <: HList, Intros <: HList](
      seqDom: ConstructorSeqDom[SS, Term, Intros],
      data: Vector[Term],
      typ: Typ[Term],
      accum: Vector[Term] = Vector()): Option[Vector[Term]] =
    (seqDom, data) match {
      case (_: ConstructorSeqDom.Empty[_], Vector()) => Some(accum)
      case (cons: ConstructorSeqDom.Cons[a, b, Term, d, e], x +: ys) =>
        for {
          h <- typData(cons.pattern, x, typ)
          t <- typDataVec(cons.tail, ys, typ, accum :+ h)
        } yield t
      case _ => None
    }

}

class TermIndMod(name: Name,
                 inductiveTyp: Term,
                 intros: Vector[Term],
                 numParams: Int) {
  val recName = Name.Str(name, "rec")

  object recAp {
    def unapply(exp: Expr) = LeanToTerm.iterAp(name, intros.length)(exp)
  }
}

case class BaseTermIndMod(name: Name,
                          inductiveTyp: Typ[Term],
                          intros: Vector[Term])
    extends TermIndMod(name, inductiveTyp, intros, 0) {
  lazy val ind = ConstructorSeqTL.getExst(inductiveTyp, intros).value

  lazy val indDom = ind.seqDom

  val typRec = ind.recE(Type)

  def inducFamily(data: Vector[Term]) =
    LeanToTerm
      .typDataVec(indDom, data, inductiveTyp)
      .map((dat) =>
        (Option(typRec: Term) /: dat) {
          case (Some(f), a) => translation.TermLang.appln(f, a)
          case _            => None
      })
      .map(_.asInstanceOf[Func[Term, Typ[Term]]])

  // def inducFamily(data: Vector[Term]) =
  //   // ind.rec(Type)(LeanToTerm.typDataVec(indDom, data).get)
  //   LeanToTerm.typFamily(indDom, data)

  def recCod(data: Vector[Term])
    : Option[Typ[u] forSome { type u <: Term with Subs[u] }] = {
    val fmlyOpt = inducFamily(data)
    val x       = inductiveTyp.Var
    val y       = inductiveTyp.Var
    val C       = fmlyOpt.flatMap(appln(_, x))
    val CC      = fmlyOpt.flatMap(appln(_, y))
    C match {
      case Some(t: Typ[u]) if C == CC => Some(t)
      case _                          => None
    }
  }

  def recFunc(data: Vector[Term]) = recCod(data).map(ind.recE(_))

  def recDef(data: Vector[Term]): Option[Term] = {
    val init = recFunc(data)
    (init /: data) {
      case (Some(f), a) => translation.TermLang.appln(f, a)
      case _            => None
    }
  }

  def inducFunc(data: Vector[Term]) = inducFamily(data).map(ind.inducE(_))

  def inducDef(data: Vector[Term]): Option[Term] = {
    val init = inducFunc(data)
    (init /: data) {
      case (Some(f), a) => translation.TermLang.appln(f, a)
      case _            => None
    }
  }

  def recOrInduc(data: Vector[Term]) = recDef(data) orElse inducDef(data)
}

object LeanInterface {

  def consts(expr: Expr): Vector[Name] = expr match {
    case Const(name, _)      => Vector(name)
    case App(x, y)           => consts(x) ++ consts(y)
    case Var(_)              => Vector()
    case Sort(_)             => Vector()
    case Lam(b, x)           => consts(b.ty) ++ consts(x)
    case Pi(b, x)            => consts(b.ty) ++ consts(x)
    case Let(_, x, y)        => consts(x) ++ consts(y)
    case LocalConst(_, _, _) => Vector()
  }

  // crude implementation for exploring
  def subExpr(expr: Expr): Vector[Expr] = expr match {
    case App(x, y)    => App(x, y) +: subExpr(x) ++: subExpr(y)
    case Var(_)       => Vector()
    case Sort(_)      => Vector()
    case Lam(b, x)    => Lam(b, x) +: subExpr(x)
    case Pi(b, x)     => Pi(b, x) +: subExpr(x)
    case Let(b, x, y) => Let(b, x, y) +: subExpr(y)
    case e            => Vector(e)
  }

  def recApp: Expr => Boolean = {
    case exp @ App(Const(Name.Str(_, "rec"), _), _) => true
    case _                                          => false
  }

  @annotation.tailrec
  def defnNames(mods: Vector[Modification],
                accum: Vector[Name] = Vector()): Vector[Name] = mods match {
    case Vector() => accum
    case IndMod(ind, _, intros) +: tail =>
      defnNames(
        tail,
        ind.name +: Name.Str(ind.name, "rec") +: intros.map(_._1) ++: accum)
    case DefMod(df) +: tail   => defnNames(tail, df.name +: accum)
    case AxiomMod(ax) +: tail => defnNames(tail, ax.name +: accum)
    case QuotMod +: tail =>
      defnNames(tail,
                Vector(Name("quot"),
                       Name("quot", "ind"),
                       Name("quot", "mk"),
                       Name("quot", "lift")) ++: accum)
  }

  def defnExprs(mods: Vector[Modification]) = mods.collect {
    case DefMod(df) => df.value
  }

  def getMods(filename: String) = {
    val exportedCommands =
      TextExportParser.parseFile(filename).toVector

    exportedCommands.collect { case ExportedModification(mod) => mod }
  }
}
@deprecated("august 7, 2017", "use interfce via trepplein")
object LeanIO {
  import LeanExportElem._
  def readData(file: Path) = Data.readAll(read.lines(file))

  def readDefs(file: Path) = {
    val lines = read.lines(file)

    val dat = Data.readAll(lines)

    new DataBase(dat, lines).getAllDefs
  }

  def pickleDefs(file: Path, outputDir: Path) = {
    val defs  = readDefs(file)
    val lines = defs map (_.depPickle)
    val out   = outputDir / file.name
    rm(out)
    lines map ((l) => { write.append(out, l + "\n"); l })
  }

  def futPickleDefs(file: Path, outputDir: Path) =
    Future(pickleDefs(file, outputDir))

  def makeDefs(inDir: Path = pwd / 'data / 'leanlibrary,
               outDir: Path = pwd / 'data / 'leandefs) = {
    val files = ls(inDir) filter (_.ext == "export")
    (files map ((f) => (f.name + ".defs", futPickleDefs(f, outDir)))).toMap
  }

  def snapshot(fd: Map[String, Future[Vector[String]]]) =
    ((fd.values map (_.value)).flatten map (_.toOption)).flatten.toVector.flatten

  def recallDefs(defDir: Path = pwd / 'data / 'leandefs) = {
    ls(defDir).toVector flatMap
      ((f) => read.lines(f) map (_.split("\t").toList))
  }
}
