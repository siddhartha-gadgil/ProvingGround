package provingground.interface
import provingground._

import ammonite.ops._
import scala.util.Try

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global

import HoTT.{Name => _, _}

import translation.{TermLang => TL}
import trepplein._

import cats.Eval, Eval._

import LeanToTerm.Parser

case class LeanToTerm(defns: (Expr, Option[Typ[Term]]) => Option[Term],
                      recDefns: Eval[Parser] => Parser,
                      vars: Vector[Term]) { parser =>
  val parse: Eval[Parser] =
    Eval.later(
      (exp: Expr, typOpt: Option[Typ[Term]]) => parseTyped(parse)(exp, typOpt)
    )

  def parseTyped(rec: Eval[Parser])(
      exp: Expr,
      typOpt: Option[Typ[Term]] = None): Option[Term] =
    defns(exp, typOpt).orElse(
      exp match {
        case App(x, y) =>
          val domOpt = typOpt.flatMap(TL.domTyp)
          for {
            arg  <- parseTyped(rec: Eval[Parser])(y, domOpt)
            func <- parseTyped(rec: Eval[Parser])(x) // cannot infer type
            res  <- TL.appln(func, arg)
          } yield res
        case Sort(_) => Some(Type)
        case Lam(domain, body) =>
          val xo = parseVar(rec: Eval[Parser])(domain)
          for {
            x   <- xo
            pb  <- addVar(x).parseTyped(rec: Eval[Parser])(body)
            res <- TL.lambda(x, pb)
          } yield res
        case Pi(domain, body) =>
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

  def parseTyp(rec: Eval[Parser])(x: Expr) =
    parseTyped(rec: Eval[Parser])(x, Some(Type)).collect {
      case tp: Typ[_] => tp
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

  def mkDef(name: Name,
            value: Expr): (Expr, Option[Typ[Term]]) => Option[Term] = {
    case (Const(`name`, _), _) => parseTyped(parse)(value)
    case _                     => None
  }

  def addAxiomMod(ax: AxiomMod) = addDefns(mkDef(ax.name, ax.ax.ty))

  def addDefMod(df: DefMod) = addDefns(mkDef(df.name, df.defn.value))

  def addQuotMod = {
    import quotient._
    val axs = Vector(quot, quotLift, quotMk, quotInd).map { (ax) =>
      mkAxiom(ax.name, ax.ty)
    }
    axs.foldLeft(parser)(_.addDefn(_))
  }

  def addIndModConsts(ind: IndMod) = {
    val axs = { (ind.name -> ind.inductiveType.ty) +: ind.intros }.map {
      case (n, t) => mkAxiom(n, t)
    }
    axs.foldLeft(parser)(_.addDefn(_))
  }
}
import induction._, shapeless.{Path => _, _}

import translation.TermLang.{appln, domTyp}

object LeanToTerm {
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

  // /**
  //   * Data for recursively defining a type family corresponding to a constructor
  //   * this is the type family for an inductive definition
  //   */
  // def typData[S <: HList, ConstructorType <: Term with Subs[ConstructorType]](
  //     pattern: ConstructorShape[S, Term, ConstructorType],
  //     data: Term,
  //     typ: Typ[Term]): Option[Term] =
  //   pattern match {
  //     case _: IdShape[Term] => Some(data.typ)
  //     case shape: CnstDepFuncConsShape[a, Term, c, d, e] =>
  //       val x = shape.tail.Var
  //       for {
  //         y   <- appln(data, x)
  //         rec <- typData(shape.headfibre(x), y, typ)
  //       } yield x :~> rec
  //     case shape: CnstFuncConsShape[a, Term, c, d, e] =>
  //       val x = shape.tail.Var
  //       for {
  //         y   <- appln(data, x)
  //         rec <- typData(shape.head, y, typ)
  //       } yield x :-> rec
  //     case shape: FuncConsShape[a, Term, c, d] =>
  //       val x = shape.tail(typ).Var
  //       val A = Type.Var
  //       for {
  //         fx  <- appln(data, x)
  //         dom <- domTyp(fx)
  //         y = dom.Var
  //         g   <- appln(fx, y)
  //         rec <- typData(shape.head, g, typ)
  //       } yield x :-> A :-> rec
  //   }
  //
  // def typDataVec[SS <: HList, Intros <: HList](
  //     seqDom: ConstructorSeqDom[SS, Term, Intros],
  //     data: Vector[Term],
  //     typ: Typ[Term],
  //     accum: Vector[Term] = Vector()): Option[Vector[Term]] =
  //   (seqDom, data) match {
  //     case (_: ConstructorSeqDom.Empty[_], Vector()) => Some(accum)
  //     case (cons: ConstructorSeqDom.Cons[a, b, Term, d, e], x +: ys) =>
  //       for {
  //         h <- typData(cons.pattern, x, typ)
  //         t <- typDataVec(cons.tail, ys, typ, accum :+ h)
  //       } yield t
  //     case _ => None
  //   }

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

  def recDefn(defnTyp: Typ[Term]): Eval[Parser] => Parser =
    (base: Eval[Parser]) => {
      case (exp: Expr, typOpt: Option[Typ[Term]]) =>
        base.map { (recParser) =>
          defn(defnTyp)(exp, typOpt, recParser)
        }.value
    }

  def defn(defnTyp: Typ[Term])(
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
      val funcOpt: Option[Term] = recFromTyp(defnTyp)
      for {
        func: Term         <- funcOpt
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

// Too complicated and does not generalize
// case class BaseTermIndMod(name: Name,
//                           inductiveTyp: Typ[Term],
//                           intros: Vector[Term])
//     extends TermIndMod(name, inductiveTyp, intros, 0) {
//   lazy val ind = ConstructorSeqTL.getExst(inductiveTyp, intros).value
//
//   lazy val indDom = ind.seqDom
//
//   val typRec = ind.recE(Type)
//
//   def inducFamily(data: Vector[Term]) =
//     LeanToTerm
//       .typDataVec(indDom, data, inductiveTyp)
//       .map((dat) =>
//         (Option(typRec: Term) /: dat) {
//           case (Some(f), a) => translation.TermLang.appln(f, a)
//           case _            => None
//       })
//       .map(_.asInstanceOf[Func[Term, Typ[Term]]])
//
//   // def inducFamily(data: Vector[Term]) =
//   //   // ind.rec(Type)(LeanToTerm.typDataVec(indDom, data).get)
//   //   LeanToTerm.typFamily(indDom, data)
//
//   def recCod(data: Vector[Term])
//     : Option[Typ[u] forSome { type u <: Term with Subs[u] }] = {
//     val fmlyOpt = inducFamily(data)
//     val x       = inductiveTyp.Var
//     val y       = inductiveTyp.Var
//     val C       = fmlyOpt.flatMap(appln(_, x))
//     val CC      = fmlyOpt.flatMap(appln(_, y))
//     C match {
//       case Some(t: Typ[u]) if C == CC => Some(t)
//       case _                          => None
//     }
//   }
//
//   def recFunc(data: Vector[Term]) = recCod(data).map(ind.recE(_))
//
//   def recDef(data: Vector[Term]): Option[Term] = {
//     val init = recFunc(data)
//     (init /: data) {
//       case (Some(f), a) => translation.TermLang.appln(f, a)
//       case _            => None
//     }
//   }
//
//   def inducFunc(data: Vector[Term]) = inducFamily(data).map(ind.inducE(_))
//
//   def inducDef(data: Vector[Term]): Option[Term] = {
//     val init = inducFunc(data)
//     (init /: data) {
//       case (Some(f), a) => translation.TermLang.appln(f, a)
//       case _            => None
//     }
//   }
//
//   def recOrInduc(data: Vector[Term]) = recDef(data) orElse inducDef(data)
// }

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
