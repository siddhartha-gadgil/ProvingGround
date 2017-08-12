package provingground.interface
import provingground._

import ammonite.ops._
import scala.util.Try

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global

import HoTT.{Name => _, _}

import translation.{TermLang => TL}
import trepplein._

import cats.Eval

import LeanToTerm.Parser

case class LeanToTerm(defns: (Expr, Option[Typ[Term]]) => Option[Term],
                      recDefns: Eval[Parser] => Parser,
                      vars: Vector[Term]) { parser =>
  val parse: Eval[Parser] =
    Eval.later(
      (exp: Expr, typOpt: Option[Typ[Term]]) => parseTyped(parse)(exp, typOpt)
    )

  def functionTyp(arg: Term, typOpt: Option[Typ[Term]]) : Option[Typ[Term]] =
    typOpt map {(applnTyp) =>
      val dep = Try(applnTyp.dependsOn(arg)).getOrElse(false)
      if (dep) arg.typ ->: applnTyp else arg ~>: applnTyp
  }

  def parseTyped(rec: Eval[Parser])(
      exp: Expr,
      typOpt: Option[Typ[Term]] = None): Option[Term] =
    defns(exp, typOpt).orElse(
      exp match {
        case App(x, y) =>
          // val domOpt = typOpt.flatMap(TL.domTyp)
          {
            for {
            func <- parseTyped(rec: Eval[Parser])(x) // function without type
            domOpt = TL.domTyp(func)
            arg <- parseTyped(rec: Eval[Parser])(y, domOpt)
            res <- TL.appln(func, arg)
          } yield res}.orElse
          {
            for {
              arg <- parseTyped(rec: Eval[Parser])(y)
              funcTypOpt = functionTyp(arg, typOpt)
              func <- parseTyped(rec)(x, funcTypOpt)
              res <- TL.appln(func, arg)
            } yield res
          }
        case Sort(_) => Some(Type)
        case Lam(domain, body) =>
          val xo = parseVar(rec: Eval[Parser])(domain)
          def getCodom(t: Term): Option[Typ[Term]] =
            typOpt match {
              case Some(fn: FuncLike[u, v]) if fn.dom == t.typ =>
                Some(fn.depcodom(t.asInstanceOf[u]))
              case _ => None
            }
          for {
            x   <- xo
            pb  <- addVar(x).parseTyped(rec: Eval[Parser])(body, getCodom(x))
            res <- TL.lambda(x, pb)
          } yield res
        case Pi(domain, body) =>
          val xo = parseVar(rec: Eval[Parser])(domain)
          // def getCodom(t: Term): Option[Typ[Term]] =
          //   typOpt match {
          //     case Some(fn: FuncLike[u, v]) if fn.dom == t.typ =>
          //       Some(fn.depcodom(t.asInstanceOf[u]))
          //     case _ => None
          //   }
          for {
            x   <- xo
            pb  <- addVar(x).parseTyp(rec: Eval[Parser])(body)
            res <- TL.pi(x, pb)
          } yield res
        case Let(domain, value, body) =>
          val xo = parseVar(rec: Eval[Parser])(domain)
          for {
            x  <- xo
            pb <- addVar(x).parseTyped(rec: Eval[Parser])(body, typOpt)
            valTerm <- parseTyped(rec: Eval[Parser])(
              body,
              parseTyp(rec: Eval[Parser])(domain.ty))
          } yield pb.replace(x, valTerm)
        case LocalConst(b, _, Some(tp)) =>
          parseSym(rec: Eval[Parser])(b.prettyName, tp)
        case Var(n) => Some(vars(n))
        case c: Const =>
          println(s"Constant $c undefined")
          None
        case _ =>
          // println(s"failed to parse $exp")
          None
      }
    )

  def addVar(t: Term) = LeanToTerm(defns, recDefns, t +: vars)

  def parseTyp(rec: Eval[Parser])(x: Expr): Option[Typ[Term]] =
    parseTyped(rec: Eval[Parser])(x, Some(Type)).flatMap {
      case tp: Typ[_] => Some(tp)
      case t =>
        println(
          s"got term $t of type ${t.typ} but expected type when parsing $x")
        None
    }

  def parseSym(rec: Eval[Parser])(name: Name, ty: Expr) =
    parseTyp(rec: Eval[Parser])(ty).map(name.toString :: _)

  def parseVar(rec: Eval[Parser])(b: Binding) =
    parseSym(rec: Eval[Parser])(b.prettyName, b.ty)

  /**
    * add several definitions given as an option valued function
    */
  def addDefns(dfn: (Expr, Option[Typ[Term]]) => Option[Term]) =
    LeanToTerm((exp, typ) => dfn(exp, typ) orElse defns(exp, typ),
               recDefns,
               vars)

  def addDefn(dfn: Expr => Option[Term]) =
    LeanToTerm((exp, typ) => dfn(exp) orElse defns(exp, typ), recDefns, vars)

  def addRecDefns(dfn: Eval[Parser] => Parser) = {
    val mixin: Eval[Parser] => Parser = (base: Eval[Parser]) => {
      case (exp: Expr, typOpt: Option[Typ[Term]]) =>
        dfn(base)(exp, typOpt).orElse(recDefns(base)(exp, typOpt))
    }
    LeanToTerm(defns, mixin, vars)
  }

  def mkAxiom(name: Name, ty: Expr): Expr => Option[Term] = {
    case Const(`name`, _) => parseSym(parse)(name, ty)
    case _                => None
  }

  def mkDef(name: Name, value: Expr, tyOpt: Option[Expr] = None)
    : (Expr, Option[Typ[Term]]) => Option[Term] = {
    case (Const(`name`, _), _) =>
      parseTyped(parse)(value, tyOpt.flatMap(parseTyp(parse)(_)))
    case _ => None
  }

  def addAxiomMod(ax: AxiomMod) = addDefns(mkDef(ax.name, ax.ax.ty))

  def addDefMod(df: DefMod) =
    addDefns(mkDef(df.name, df.defn.value, Some(df.defn.ty)))

  def addQuotMod = {
    import quotient._
    val axs = Vector(quot, quotLift, quotMk, quotInd).map { (ax) =>
      mkAxiom(ax.name, ax.ty)
    }
    axs.foldLeft(parser)(_.addDefn(_))
  }

  def toTermIndMod(ind: IndMod): TermIndMod = {
    val inductiveTyp = parse.map(_(ind.inductiveType.ty, None)).value
    val typValue     = LeanToTerm.getValue(inductiveTyp.get, ind.numParams)
    val intros = ind.intros.map {
      case (name, tp) => name.toString :: (parseTyp(parse)(tp).get)
    }
    typValue match {
      case Some(typ: Typ[Term]) =>
        SimpleIndMod(ind.inductiveType.name, typ, intros, ind.numParams)
      case Some(t) =>
        IndexedIndMod(ind.inductiveType.name, t, intros, ind.numParams)
      case _ =>
        throw new Exception(
          s"Could not get type  value for inductive type $inductiveTyp")
    }
  }

  def addIndMod(ind: IndMod) = {
    val axs = { (ind.name -> ind.inductiveType.ty) +: ind.intros }.map {
      case (n, t) => mkAxiom(n, t)
    }
    val withAxioms = axs.foldLeft(parser)(_.addDefn(_))
    withAxioms.addRecDefns(toTermIndMod(ind).recDefn)
  }

  def add(mod: Modification) = mod match {
    case ind: IndMod  => addIndMod(ind)
    case ax: AxiomMod => addAxiomMod(ax)
    case df: DefMod   => addDefMod(df)
    case QuotMod      => addQuotMod
  }
}
import induction._ //, shapeless.{Path => _, _}
//
// import translation.TermLang.{appln, domTyp}

object LeanToTerm {
  val emptyParser: Parser = { case (x, y) => None }

  val empty =
    LeanToTerm(emptyParser, (_: Eval[Parser]) => emptyParser, Vector())

  def fromMods(mods: Vector[Modification], init: LeanToTerm = empty) =
    mods.foldLeft(init) { case (l: LeanToTerm, m: Modification) => l.add(m) }

  type Parser = (Expr, Option[Typ[Term]]) => Option[Term]

  def iterAp(name: Name, length: Int): Expr => Option[Vector[Expr]] = {
    case c @ Const(`name`, _) if length == 0 => Some(Vector(c))
    case App(f, z)                           => iterAp(name, length - 1)(f).map(_ :+ z)
    case _                                   => None
  }

  def iterApTyp(name: Name, length: Int, typOpt: Option[Typ[Term]])
    : Expr => Option[Vector[(Expr, Option[Typ[Term]])]] = {
    case c @ Const(`name`, _) if length == 0 => Some(Vector((c, typOpt)))
    case App(f, z) =>
      typOpt match {
        case Some(ft: FuncTyp[u, v]) =>
          iterApTyp(name, length - 1, Some(ft.codom))(f)
            .map(_ :+ (z, Some(ft.dom)))
        case Some(ft: GenFuncTyp[u, v]) =>
          iterApTyp(name, length - 1, None)(f).map(_ :+ (z, Some(ft.domain)))
        case _ => iterApTyp(name, length - 1, None)(f).map(_ :+ (z, None))
      }
    case _ => None
  }

  def unifier(
      a: Term,
      b: Term,
      numParams: Int,
      accum: Vector[(Term, Term)] = Vector()): Option[Vector[(Term, Term)]] =
    (a, b, numParams) match {
      case (x, y, 0) if x == y => Some(accum)
      case (FormalAppln(func1, arg1), FormalAppln(func2, arg2), n) if n > 0 =>
        unifier(func1.replace(arg1, arg2), func2, n - 1, (arg1, arg2) +: accum)
      case _ => None
    }

  def getValue(t: Term, n: Int): Option[Term] = (t, n) match {
    case (x, 0)                            => Some(x)
    case (l: LambdaLike[u, v], n) if n > 0 => Some(l.value)
    case (fn: FuncLike[u, v], n) if n > 0  => Some(fn(fn.dom.Var))
    case _                                 => None
  }

  import ConstructorShape._

}

abstract class TermIndMod(name: Name,
                          inductiveTyp: Term,
                          intros: Vector[Term],
                          numParams: Int) {
  val recName = Name.Str(name, "rec")

  object recAp {
    def unapply(exp: Expr) =
      LeanToTerm.iterAp(name, intros.length)(exp)
  }

  def recFromTyp(typ: Typ[Term]): Option[Term]

  import translation.TermLang

  val recDefn: Eval[Parser] => Parser =
    (base: Eval[Parser]) => {
      case (exp: Expr, typOpt: Option[Typ[Term]]) =>
        base.map { (recParser) =>
          defn(exp, typOpt, recParser)
        }.value
    }

  def defn(
      exp: Expr,
      typOpt: Option[Typ[Term]],
      predef: => ((Expr, Option[Typ[Term]]) => Option[Term])): Option[Term] = {
    val argsPair: Option[Vector[(Expr, Option[Typ[Term]])]] =
      LeanToTerm.iterApTyp(name, intros.length, typOpt)(exp)
    argsPair.flatMap { (v: Vector[(Expr, Option[Typ[Term]])]) =>
      val optArgs: Vector[Option[Term]] = v.map {
        case (exp, tpO) => predef(exp, tpO)
      }
      val argsOpt: Option[Vector[Term]] =
        if (optArgs.contains(None)) None else Some(optArgs.flatten)
      for {
        defnTyp            <- typOpt
        func: Term         <- recFromTyp(defnTyp)
        args: Vector[Term] <- argsOpt
        res                <- TermLang.applnFold(func, args)
      } yield res
    }
  }
}

case class SimpleIndMod(name: Name,
                        typ: Typ[Term],
                        intros: Vector[Term],
                        numParams: Int)
    extends TermIndMod(name, typ, intros, numParams) {
  lazy val ind = ConstructorSeqTL.getExst(typ, intros).value

  import LeanToTerm.unifier

  import implicits._

  def getInd(dom: Typ[Term]) =
    unifier(typ, dom, numParams).map { (vec) =>
      vec.foldLeft(ind) { case (a, (x, y)) => a.subst(x, y) }
    }

  def recFromTyp(typ: Typ[Term]): Option[Term] = typ match {
    case ft: FuncTyp[u, v] if ft.dom == typ =>
      Some(ind.recE(ft.codom))
    case ft: FuncTyp[u, v] =>
      getInd(ft.dom).map { (i) =>
        i.recE(ft.codom)
      }
    case gt: PiDefn[u, v] if gt.domain == typ =>
      Some(ind.inducE(gt.fibers.asInstanceOf[Func[Term, Typ[Term]]]))
    case gt: PiDefn[u, v] =>
      getInd(gt.domain).map { (i) =>
        i.inducE(gt.fibers.asInstanceOf[Func[Term, Typ[Term]]])
      }
    case _ => None
  }
}

case class IndexedIndMod(name: Name,
                         typF: Term,
                         intros: Vector[Term],
                         numParams: Int)
    extends TermIndMod(name, typF, intros, numParams) {
  lazy val ind = TypFamilyExst.getIndexedConstructorSeq(typF, intros).value

  import ind.family

  import LeanToTerm.unifier

  import implicits._

  def getInd(domF: Term) =
    unifier(typF, domF, numParams).map { (vec) =>
      vec.foldLeft(ind) { case (a, (x, y)) => a.subs(x, y) }
    }

  def recFromTyp(typ: Typ[Term]): Option[Term] = {
    for {
      dom  <- family.domFromRecType(typ)
      cod  <- family.codFromRecType(typ)
      ind0 <- getInd(dom)
    } yield ind0.recE(cod)
  }.orElse {
    for {
      dom <- family.domFromRecType(typ)
      cod = family.codFamily(typ)
      ind0 <- getInd(dom)
    } yield ind0.inducE(cod)
  }
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
