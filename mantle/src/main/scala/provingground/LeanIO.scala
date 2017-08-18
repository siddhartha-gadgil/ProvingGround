package provingground.interface
import provingground._

import ammonite.ops._
import scala.util.Try

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global

import HoTT.{Name => _, _}

import translation.{TermLang => TL}
import trepplein._

// import cats.Eval

import LeanToTerm.Parser

import translation.FansiShow._

case class LeanToTerm(defnMap: Map[Name, Term],
                      recDefns: (=> Parser) => Parser,
                      vars: Vector[Term]) { self =>
  def defns(exp: Expr, typOpt: Option[Typ[Term]]) = exp match {
    case Const(name, _) => defnMap.get(name)
    case _              => None
  }

  val parse: Parser =
    (exp: Expr, typOpt: Option[Typ[Term]]) => parseTyped(parse)(exp, typOpt)

  def functionTyp(arg: Term, typOpt: Option[Typ[Term]]): Option[Typ[Term]] =
    typOpt map { (applnTyp) =>
      val dep = Try(applnTyp.dependsOn(arg)).getOrElse(false)
      if (!dep) arg.typ ->: applnTyp else arg ~>: applnTyp
    }

  def wAppln(f: Term, a: Term) = {
    val res = TL.appln(f, a)
    // if (res.isEmpty)
    //   println(
    //     s"failed to apply ${f.fansi} with type ${f.typ.fansi} to ${a.fansi} with type ${a.typ}")
    res
  }

  def parseTyped(rec: => Parser)(
      exp: Expr,
      typOpt: Option[Typ[Term]] = None): Option[Term] =
    defns(exp, typOpt)
      .orElse(recDefns(parse)(exp, typOpt))
      .orElse(
        exp match {
          case App(x, y) =>
            // val domOpt = typOpt.flatMap(TL.domTyp)
            val resOpt = {
              for {
                func <- parseTyped(rec)(x) // function without type
                domOpt = TL.domTyp(func)
                // _ = if (domOpt.isEmpty)
                //   println(
                //     s"failed to get domain for ${func.fansi} with type ${func.typ.fansi}")
                arg <- parseTyped(rec)(y, domOpt)
                res <- wAppln(func, arg)
              } yield res
            }.orElse {
              for {
                arg <- parseTyped(rec)(y)
                funcTypOpt = functionTyp(arg, typOpt)
                _ = typOpt.foreach((_) =>
                  if (funcTypOpt.isEmpty)
                    println(
                      s"failed to get function type from ${typOpt} with arg ${arg.fansi}"))
                func <- parseTyped(rec)(x, funcTypOpt)
                res  <- wAppln(func, arg)
              } yield res
            }
            if (resOpt.isEmpty)
              LeanToTerm.appFailure += ((exp, x, y, typOpt, self))
            resOpt
          case Sort(_) => Some(Type)
          case Lam(domain, body) =>
            val xo = parseVar(rec)(domain)
            def getCodom(t: Term): Option[Typ[Term]] =
              typOpt match {
                case Some(ft: GenFuncTyp[u, v]) if ft.domain == t.typ =>
                  Some(ft.fib(t.asInstanceOf[u]))
                case _ =>
                  typOpt.foreach((_) =>
                    println(
                      s"could not get codomain for ${typOpt.map(_.fansi)} for lambda with variable of type ${t.typ.fansi}"))
                  None
              }
            for {
              x   <- xo
              pb  <- addVar(x).parse(body, getCodom(x))
              res <- TL.lambda(x, pb)
            } yield res
          case Pi(domain, body) =>
            val xo = parseVar(rec)(domain)
            // def getCodom(t: Term): Option[Typ[Term]] =
            //   typOpt match {
            //     case Some(fn: FuncLike[u, v]) if fn.dom == t.typ =>
            //       Some(fn.depcodom(t.asInstanceOf[u]))
            //     case _ => None
            //   }
            for {
              x   <- xo
              pb  <- addVar(x).parse(body, None)
              res <- TL.pi(x, pb)
            } yield res
          case Let(domain, value, body) =>
            val xo = parseVar(rec)(domain)
            for {
              x       <- xo
              pb      <- addVar(x).parseTyped(rec)(body, typOpt)
              valTerm <- parseTyped(rec)(body, parseTyp(rec)(domain.ty))
            } yield pb.replace(x, valTerm)
          case LocalConst(b, _, Some(tp)) =>
            parseSym(rec)(b.prettyName, tp)
          case Var(n)   => Some(vars(n))
          case c: Const =>
            // println(s"Constant $c undefined")
            LeanToTerm.badConsts += c
            // scala.io.StdIn.readLine
            None
          case _ =>
            println(s"failed to parse $exp")
            None
        }
      )

  def addVar(t: Term) = LeanToTerm(defnMap, recDefns, t +: vars)

  def parseTyp(rec: => Parser)(x: Expr): Option[Typ[Term]] =
    parseTyped(rec)(x, Some(Type)).flatMap {
      case tp: Typ[_] => Some(tp)
      case t =>
        println(
          s"got term $t of type ${t.typ} but expected type when parsing $x")
        None
    }

  def parseSym(rec: => Parser)(name: Name, ty: Expr) =
    parseTyp(rec)(ty).map(name.toString :: _)

  def parseVar(rec: => Parser)(b: Binding) =
    parseSym(rec)(b.prettyName, b.ty)

  /**
    * add several definitions given as an option valued function
    */
  // def addDefns(dfn: (Expr, Option[Typ[Term]]) => Option[Term]) =
  //   LeanToTerm((exp, typ) => dfn(exp, typ) orElse defns(exp, typ),
  //              recDefns,
  //              vars)
  //
  // def addDefn(dfn: Expr => Option[Term]) =
  //   LeanToTerm((exp, typ) => dfn(exp) orElse defns(exp, typ), recDefns, vars)

  def addRecDefns(dfn: (=> Parser) => Parser) = {
    def mixin(base: => Parser): Parser = {
      case (exp: Expr, typOpt: Option[Typ[Term]]) =>
        dfn(base)(exp, typOpt).orElse(recDefns(base)(exp, typOpt))
    }
    LeanToTerm(defnMap, mixin, vars)
  }

  def addDefnMap(name: Name, term: Term) =
    self.copy(defnMap = self.defnMap + (name -> term))

  def addDefnVal(name: Name, value: Expr, tp: Expr) = {
    val typ = parseTyp(parse)(tp)
    if (typ.isEmpty)
      println(s"while defining $name, failed to parse its type $tp")
    parseTyped(parse)(value, parseTyp(parse)(tp))
      .map(addDefnMap(name, _))
      .getOrElse(self)
  }

  def addAxiom(name: Name, ty: Expr) =
    parseSym(parse)(name, ty).map(addDefnMap(name, _)).getOrElse(self)

  def addAxioms(axs: Vector[(Name, Expr)]) =
    axs.foldLeft(self) { case (p, (n, v)) => p.addAxiom(n, v) }

  def addAxiomMod(ax: AxiomMod) = addAxiom(ax.name, ax.ax.ty)

  def addDefMod(df: DefMod) =
    addDefnVal(df.name, df.defn.value, df.defn.ty)

  def addQuotMod = {
    import quotient._
    val axs = Vector(quot, quotLift, quotMk, quotInd).map { (ax) =>
      (ax.name, ax.ty)
    }
    addAxioms(axs)
  }

  def toTermIndModOpt(ind: IndMod): Option[TermIndMod] = {
    val inductiveTypOpt = parseTyp(parse)(ind.inductiveType.ty)
    inductiveTypOpt.flatMap { (inductiveTyp) =>
      // println(s"Inductive type $inductiveTyp")
      // println(s"Parameters: ${ind.numParams}")
      val name = ind.inductiveType.name
      val typF = name.toString :: inductiveTyp
      val typValue =
        LeanToTerm.getValue(typF, ind.numParams, Vector())
      val withTypeName = addAxiom(name, ind.inductiveType.ty)
      // println(
      //   s"type family with parameters: ${typF.fansi} with type ${typF.typ.fansi}")
      // println(
      //   s"Type value ${typValue.map(_.fansi)} with type ${typValue.map(_.typ.fansi)}")
      val introsOpt = ind.intros.map {
        case (name, tp) =>
          // println(s"intro type : $tp")
          // println(
          //   s"parsed as ${withTypeName.parse.map(_(tp, Some(Type))).value}")
          withTypeName.parseTyp(withTypeName.parse)(tp).map(name.toString :: _)
      }
      if (introsOpt.contains(None)) {
        println("failed to parse intro rules")
        None
      } else {
        val intros = introsOpt map (_.get)
        typValue match {
          case Some((typ: Typ[Term], params)) =>
            Some(SimpleIndMod(ind.inductiveType.name, typ, intros, params))
          case Some((t, params)) =>
            Some(IndexedIndMod(ind.inductiveType.name, t, intros, params))
          case None =>
            println("No type value")
            None
        }
      }
    }
  }

  def addIndMod(ind: IndMod) = {
    val withTypDef = addAxiom(ind.name, ind.inductiveType.ty)
    // println("type added")
    // val axs = ind.intros.map {
    //   case (n, t) => withTypDef.mkAxiom(n, t)
    // }
    val withAxioms = withTypDef.addAxioms(ind.intros)
    // println("added axioms")
    val indOpt = withAxioms.toTermIndModOpt(ind)
    // println(s"indOpt: $indOpt")
    indOpt
      .map { (ind) =>
        withAxioms.addRecDefns(ind.recDefn)
      }
      .getOrElse {
        println("no rec definitions")
        withAxioms
      }
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
  import collection.mutable.ArrayBuffer
  val badConsts: ArrayBuffer[Const] = ArrayBuffer()

  val appFailure
    : ArrayBuffer[(Expr, Expr, Expr, Option[Typ[Term]], LeanToTerm)] =
    ArrayBuffer()

  def emptyRecParser(base: => Parser): Parser = {
    case (x, y) =>
      // println("trying rec definition")
      None
  }

  val empty =
    LeanToTerm(Map(), emptyRecParser, Vector())

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
    case c @ Const(`name`, _) if length == 0 => Some(Vector())
    case App(f, z) =>
      typOpt match {
        case Some(ft: FuncTyp[u, v]) =>
          iterApTyp(name, length - 1, Some(ft.codom))(f)
            .map(_ :+ (z -> Some(ft.dom)))
        case Some(ft: GenFuncTyp[u, v]) =>
          iterApTyp(name, length - 1, None)(f).map(_ :+ (z -> Some(ft.domain)))
        case _ => iterApTyp(name, length - 1, None)(f).map(_ :+ (z -> None))
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
      case _ =>
        println(s"failed to unify ${a.fansi} and ${b.fansi}")
        None
    }

  def getValue(t: Term,
               n: Int,
               accum: Vector[Term]): Option[(Term, Vector[Term])] =
    (t, n) match {
      case (x, 0) => Some(x -> accum)
      case (l: LambdaLike[u, v], n) if n > 0 =>
        getValue(l.value, n - 1, l.variable +: accum)
      case (fn: FuncLike[u, v], n) if n > 0 =>
        val x = fn.dom.Var
        getValue(fn(x), n - 1, x +: accum)
      case _ => None
    }

  import ConstructorShape._

}

abstract class TermIndMod(name: Name,
                          inductiveTyp: Term,
                          intros: Vector[Term],
                          params: Vector[Term]) {
  val numParams = params.length

  val introsFolded =
    intros.map((rule) => translation.TermLang.applnFold(rule, params)).flatten

  val recName = Name.Str(name, "rec")

  object recAp {
    def unapply(exp: Expr) =
      LeanToTerm.iterAp(name, intros.length)(exp)
  }

  def recFromTyp(typ: Typ[Term]): Option[Term]

  import translation.TermLang

  def recDefn(base: => Parser): Parser = {
    case (exp: Expr, typOpt: Option[Typ[Term]]) => {
      // println(s"trying rec definition for $inductiveTyp")
      defn(exp, typOpt, base)
    }
  }

  def defn(exp: Expr,
           typOpt: Option[Typ[Term]],
           predef: (Expr, Option[Typ[Term]]) => Option[Term]): Option[Term]

}

case class SimpleIndMod(name: Name,
                        typ: Typ[Term],
                        intros: Vector[Term],
                        params: Vector[Term])
    extends TermIndMod(name, typ, intros, params) {
  lazy val ind = ConstructorSeqTL.getExst(typ, introsFolded).value

  import LeanToTerm.unifier

  import implicits._

  def getInd(dom: Typ[Term]) =
    unifier(typ, dom, numParams).map { (vec) =>
      vec.foldLeft(ind) { case (a, (x, y)) => a.subst(x, y) }
    }

  import scala.util.Try

  def defn(exp: Expr,
           typOpt: Option[Typ[Term]],
           predef: (Expr, Option[Typ[Term]]) => Option[Term]): Option[Term] = {
    val argsFmlyOpt = LeanToTerm.iterAp(recName, numParams + 1)(exp)
    argsFmlyOpt.flatMap { (argsFmly) =>
      val newParams = argsFmly.init.map((t) => predef(t, None)).flatten
      if (newParams.length < numParams) None
      else {
        val indNew = (newParams.zipWithIndex).foldLeft(ind) {
          case (a, (y, n)) => a.subst(params(n), y)
        }
        println(s"New simple inductive type: ${indNew.typ.fansi}")

        predef(argsFmly.last, None) match {
          case Some(l: LambdaLike[u, v]) =>
            l.value match {
              case tp: Typ[u] =>
                if (tp.dependsOn(l.variable))
                  Some(indNew.inducE((l.variable: Term) :-> (tp: Typ[u])))
                else Some(indNew.recE(tp))
            }
          case Some(fn: FuncLike[u, v]) =>
            val x = fn.dom.Var
            val y = fn(x)
            y match {
              case tp: Typ[u] =>
                if (tp.dependsOn(x))
                  Some(indNew.inducE((x: Term) :-> (tp: Typ[u])))
                else Some(indNew.recE(tp))
            }
          case Some(t) =>
            println(
              s"family for induction type ${t.fansi} with type ${t.typ.fansi} unmatched")
            None
          case None =>
            println(s"parsing failed for ${argsFmly.last}")
            None
        }
      }
    }
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
    case _ =>
      println(s"unmatched type ${typ.fansi} in recFromTyp for $name")
      None
  }
}

case class IndexedIndMod(name: Name,
                         typF: Term,
                         intros: Vector[Term],
                         params: Vector[Term])
    extends TermIndMod(name, typF, intros, params) {
  lazy val ind =
    TypFamilyExst.getIndexedConstructorSeq(typF, introsFolded).value

  import ind.family

  import LeanToTerm.unifier

  import implicits._

  def getInd(domF: Term) =
    unifier(typF, domF, numParams).map { (vec) =>
      vec.foldLeft(ind) { case (a, (x, y)) => a.subs(x, y) }
    }

  def defn(exp: Expr,
           typOpt: Option[Typ[Term]],
           predef: (Expr, Option[Typ[Term]]) => Option[Term]): Option[Term] = {
    val argsFmlyOpt = LeanToTerm.iterAp(recName, numParams + 1)(exp)
    argsFmlyOpt.flatMap { (argsFmly) =>
      val newParams = argsFmly.init.map((t) => predef(t, None)).flatten
      if (newParams.length < numParams) None
      else {
        val indNew = (newParams.zipWithIndex).foldLeft(ind) {
          case (a, (y, n)) => a.subs(params(n), y)
        }
        println(
          s"New indexed inductive type: ${indNew.W.fansi} with type ${indNew.W.fansi}")
        val fmlOpt = predef(argsFmly.last, None)
        val recOpt =
          for {
            fml <- fmlOpt
            cod <- family.constFinalCod(fml)
          } yield indNew.recE(cod)
        val inducOpt = fmlOpt.map(indNew.inducE(_))
        predef(argsFmly.last, None) match {
          case Some(l: LambdaLike[u, v]) =>
            l.value match {
              case tp: Typ[u] =>
                if (tp.dependsOn(l.variable))
                  Some(indNew.inducE((l.variable: Term) :-> (tp: Typ[u])))
                else Some(indNew.recE(tp))
            }
          case Some(fn: FuncLike[u, v]) =>
            val x = fn.dom.Var
            val y = fn(x)
            y match {
              case tp: Typ[u] =>
                if (tp.dependsOn(x))
                  Some(indNew.inducE((x: Term) :-> (tp: Typ[u])))
                else Some(indNew.recE(tp))
            }
          case Some(t) =>
            println(
              s"family for induction type ${t.fansi} with type ${t.typ.fansi} unmatched")
            None
          case None =>
            println(s"parsing failed for ${argsFmly.last}")
            None
        }
      }
    }
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
