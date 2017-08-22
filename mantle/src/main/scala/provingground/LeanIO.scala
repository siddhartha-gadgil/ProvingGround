package provingground.interface
import provingground._

import ammonite.ops._
import scala.util._

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global

import HoTT.{Name => _, _}

import translation.{TermLang => TL}
import trepplein._

// import cats.Eval

import LeanToTerm.{TypedParser, Parser, OptParser, RecIterAp}

import translation.FansiShow._

case class LeanToTerm(defnMap: Map[Name, Term],
                      mods: Map[Name, TermIndMod],
                      recDefns: (=> Parser) => OptParser,
                      vars: Vector[Term]) { self =>
  def defns(exp: Expr, typOpt: Option[Typ[Term]]) = exp match {
    case Const(name, _) => defnMap.get(name)
    case _              => None
  }

  val parse: Parser = recParser(parse)

  case class UnParsedException(exp: Expr)
      extends IllegalArgumentException("could not parse expression")

  case class NoConstantException(name: Name)
      extends IllegalArgumentException(s"No constant with name $name found")

  def defnOpt(exp: Expr) =
    exp match {
      case Const(name, _) => defnMap.get(name)
      case _              => None
    }

  object Predef {
    def unapply(exp: Expr): Option[Term] =
      (
        defnOpt(exp)
        // .orElse(recDefns(parse)(exp))
      )
  }

  def recParser(rec: => Parser)(exp: Expr): Try[Term] =
    exp match {
      case Predef(t) => Success(t)
      case Sort(_)   => Success(Type)
      case Var(n)    => Try(vars(n))
      case RecIterAp(name, args) =>
        val indMod         = mods(name)
        val (argsFmly, xs) = args.splitAt(indMod.numParams + 1)
        val recFnTry       = indMod.getRec(argsFmly, parse)
        for {
          recFn <- recFnTry
          vec   <- parseVec(xs)
          res   <- Try(foldFunc(recFn, vec))
        } yield res
      case App(a, b) =>
        for {
          func <- recParser(rec)(a)
          arg  <- recParser(rec)(b)
          res  <- Try(applyFunc(func, arg))
        } yield res
      case Lam(domain, body) =>
        for {
          domTerm <- recParser(rec)(domain.ty)
          domTyp  <- Try(toTyp(domTerm))
          x       = domTyp.Var
          withVar = addVar(x)
          value <- withVar.parse(body)
        } yield x :~> value
      case Pi(domain, body) =>
        for {
          domTerm <- recParser(rec)(domain.ty)
          domTyp  <- Try(toTyp(domTerm))
          x       = domTyp.Var
          withVar = addVar(x)
          value <- withVar.parse(body)
          cod   <- Try(toTyp(value))
        } yield if (cod.dependsOn(x)) x ~>: cod else x.typ ->: cod
      case Let(domain, value, body) =>
        for {
          domTerm <- recParser(rec)(domain.ty)
          domTyp  <- Try(toTyp(domTerm))
          x = domTyp.Var
          valueTerm <- recParser(rec)(value)
          withVar = addVar(x)
          bodyTerm <- withVar.parse(body)
        } yield bodyTerm.replace(x, valueTerm)
      case e => Failure(UnParsedException(e))
    }

  // val typedParse: TypedParser =
  //   (exp: Expr, typOpt: Option[Typ[Term]]) => parse(exp).toOption //parseTyped(typedParse)(exp, None)

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

  // def parseTyped(rec: => TypedParser)(
  //     exp: Expr,
  //     typOpt: Option[Typ[Term]] = None): Option[Term] =
  //   defns(exp, typOpt)
  //     .orElse(recDefns(typedParse)(exp, typOpt))
  //     .orElse(
  //       exp match {
  //         case App(x, y) =>
  //           // val domOpt = typOpt.flatMap(TL.domTyp)
  //           val resOpt = {
  //             for {
  //               func <- parseTyped(rec)(x) // function without type
  //               domOpt = TL.domTyp(func)
  //               // _ = if (domOpt.isEmpty)
  //               //   println(
  //               //     s"failed to get domain for ${func.fansi} with type ${func.typ.fansi}")
  //               arg <- parseTyped(rec)(y, domOpt)
  //               res <- wAppln(func, arg)
  //             } yield res
  //           }.orElse {
  //             for {
  //               arg <- parseTyped(rec)(y)
  //               funcTypOpt = functionTyp(arg, typOpt)
  //               _ = typOpt.foreach((_) =>
  //                 if (funcTypOpt.isEmpty)
  //                   println(
  //                     s"failed to get function type from ${typOpt} with arg ${arg.fansi}"))
  //               func <- parseTyped(rec)(x, funcTypOpt)
  //               res  <- wAppln(func, arg)
  //             } yield res
  //           }
  //           if (resOpt.isEmpty)
  //             LeanToTerm.appFailure += ((exp, x, y, typOpt, self))
  //           resOpt
  //         case Sort(_) => Some(Type)
  //         case Lam(domain, body) =>
  //           val xo = parseVar(domain)
  //           def getCodom(t: Term): Option[Typ[Term]] =
  //             typOpt match {
  //               case Some(ft: GenFuncTyp[u, v]) if ft.domain == t.typ =>
  //                 Some(ft.fib(t.asInstanceOf[u]))
  //               case _ =>
  //                 typOpt.foreach((_) =>
  //                   println(
  //                     s"could not get codomain for ${typOpt.map(_.fansi)} for lambda with variable of type ${t.typ.fansi}"))
  //                 None
  //             }
  //           for {
  //             x   <- xo
  //             pb  <- addVar(x).typedParse(body, getCodom(x))
  //             res <- TL.lambda(x, pb)
  //           } yield res
  //         case Pi(domain, body) =>
  //           val xo = parseVar(domain)
  //           // def getCodom(t: Term): Option[Typ[Term]] =
  //           //   typOpt match {
  //           //     case Some(fn: FuncLike[u, v]) if fn.dom == t.typ =>
  //           //       Some(fn.depcodom(t.asInstanceOf[u]))
  //           //     case _ => None
  //           //   }
  //           for {
  //             x   <- xo
  //             pb  <- addVar(x).typedParse(body, None)
  //             res <- TL.pi(x, pb)
  //           } yield res
  //         case Let(domain, value, body) =>
  //           val xo = parseVar(domain)
  //           for {
  //             x       <- xo
  //             pb      <- addVar(x).parseTyped(rec)(body, typOpt)
  //             valTerm <- parseTyped(rec)(body, parseTyp(domain.ty))
  //           } yield pb.replace(x, valTerm)
  //         case LocalConst(b, _, Some(tp)) =>
  //           parseSym(b.prettyName, tp)
  //         case Var(n)   => Some(vars(n))
  //         case c: Const =>
  //           // println(s"Constant $c undefined")
  //           LeanToTerm.badConsts += c
  //           // scala.io.StdIn.readLine
  //           None
  //         case _ =>
  //           println(s"failed to parse $exp")
  //           None
  //       }
  //     )

  def addVar(t: Term) = LeanToTerm(defnMap, mods, recDefns, t +: vars)

  def parseTyp(x: Expr): Try[Typ[Term]] =
    parse(x).flatMap {
      case tp: Typ[_] => Success(tp)
      case t          =>
        // println(
        //   s"got term $t of type ${t.typ} but expected type when parsing $x")
        throw NotTypeException(t)
    }

  def parseVec(vec: Vector[Expr]): Try[Vector[Term]] = vec match {
    case Vector() => Success(Vector())
    case x +: ys =>
      for {
        head <- parse(x)
        tail <- parseVec(ys)
      } yield head +: tail
  }

  def parseTypVec(vec: Vector[Expr]): Try[Vector[Typ[Term]]] = vec match {
    case Vector() => Success(Vector())
    case x +: ys =>
      for {
        head <- parseTyp(x)
        tail <- parseTypVec(ys)
      } yield head +: tail
  }

  def parseSymVec(vec: Vector[(Name, Expr)]): Try[Vector[Term]] = vec match {
    case Vector() => Success(Vector())
    case (name, expr) +: ys =>
      for {
        tp <- parseTyp(expr)
        head = name.toString :: tp
        tail <- parseSymVec(ys)
      } yield head +: tail
  }

  def parseSym(name: Name, ty: Expr) =
    parseTyp(ty).map(name.toString :: _)

  def parseVar(b: Binding) =
    parseSym(b.prettyName, b.ty)

  def addRecDefns(dfn: (=> Parser) => OptParser) = {
    def mixin(base: => Parser): OptParser = {
      case (exp: Expr) =>
        dfn(base)(exp).orElse(recDefns(base)(exp))
    }
    LeanToTerm(defnMap, mods, mixin, vars)
  }

  def addDefnMap(name: Name, term: Term) =
    self.copy(defnMap = self.defnMap + (name -> term))

  def addDefnVal(name: Name, value: Expr, tp: Expr) = {
    val typ = parseTyp(tp)
    // if (typ.isEmpty)
    //   println(s"while defining $name, failed to parse its type $tp")
    parse(value)
      .map((t) => addDefnMap(name, t))
      .getOrElse(self)
  }

  def addAxiom(name: Name, ty: Expr) =
    parseSym(name, ty).map(addDefnMap(name, _)).getOrElse(self)

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

  def toTermIndModOpt(ind: IndMod): Try[TermIndMod] = {
    val inductiveTypOpt = parseTyp(ind.inductiveType.ty)
    val isPropn         = LeanToTerm.isPropn(ind.inductiveType.ty)
    inductiveTypOpt.flatMap { (inductiveTyp) =>
      val name = ind.inductiveType.name
      val typF = name.toString :: inductiveTyp
      val typValueOpt =
        LeanToTerm.getValue(typF, ind.numParams, Vector())
      val withTypeName = addAxiom(name, ind.inductiveType.ty)
      val introsOpt = ind.intros.map {
        case (name, tp) =>
          withTypeName
            .parseTyp(tp)
            .map(name.toString :: _)
      }
      val introsTry =
        withTypeName.parseSymVec(ind.intros)
      introsTry.flatMap { (intros) =>
        typValueOpt.map { (typValue) =>
          typValue match {
            case (typ: Typ[Term], params) =>
              SimpleIndMod(ind.inductiveType.name,
                           typ,
                           intros,
                           params,
                           isPropn)
            case (t, params) =>
              IndexedIndMod(ind.inductiveType.name, t, intros, params, isPropn)
          }
        }
      }
    }
  }

  def addIndMod(ind: IndMod) = {
    val withTypDef = addAxiom(ind.name, ind.inductiveType.ty)
    val withAxioms = withTypDef.addAxioms(ind.intros)
    val indOpt     = withAxioms.toTermIndModOpt(ind)
    indOpt
      .map { (indMod) =>
        withAxioms
        // .addRecDefns(indMod.recDefn)
          .copy(mods = self.mods + (ind.name -> indMod))
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
case class NewParseDiffersException(name: Name,
                                    value: Expr,
                                    old: Term,
                                    p: Try[Term])
    extends Exception("Parses not matched")

import induction._ //, shapeless.{Path => _, _}

object LeanToTerm {
  import collection.mutable.ArrayBuffer
  val badConsts: ArrayBuffer[Const] = ArrayBuffer()

  val appFailure
    : ArrayBuffer[(Expr, Expr, Expr, Option[Typ[Term]], LeanToTerm)] =
    ArrayBuffer()

  def emptyRecParser(base: => Parser): OptParser = { (_) =>
    // println("trying rec definition")
    None
  }

  val empty =
    LeanToTerm(Map(), Map(), emptyRecParser, Vector())

  def fromMods(mods: Vector[Modification], init: LeanToTerm = empty) =
    mods.foldLeft(init) { case (l: LeanToTerm, m: Modification) => l.add(m) }

  type TypedParser = (Expr, Option[Typ[Term]]) => Option[Term]

  type Parser = Expr => Try[Term]

  type OptParser = Expr => Option[Term]

  object RecIterAp {
    def unapply(exp: Expr): Option[(Name, Vector[Expr])] = exp match {
      case Const(Name.Str(prefix, "rec"), _) => Some((prefix, Vector()))
      case App(func, arg) =>
        unapply(func).map { case (name, vec) => (name, vec :+ arg) }
      case _ => None
    }
  }

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
               accum: Vector[Term]): Try[(Term, Vector[Term])] =
    (t, n) match {
      case (x, 0) => Success(x -> accum)
      case (l: LambdaLike[u, v], n) if n > 0 =>
        getValue(l.value, n - 1, accum :+ l.variable)
      case (fn: FuncLike[u, v], n) if n > 0 =>
        val x = fn.dom.Var
        getValue(fn(x), n - 1, accum :+ x)
      case _ => Failure(new Exception("getValue failed"))
    }

  val isPropn: Expr => Boolean = {
    case Pi(_, t) => isPropn(t)
    case Sort(l)  => l == Level.Zero
    case _        => false
  }

  val proofLift: (Term, Term) => Try[Term] = {
    case (w: Typ[u], tp: Typ[v]) => Success { (w.Var) :-> tp }
    case (w: FuncLike[u, v], tp: FuncLike[a, b]) if w.dom == tp.dom =>
      val x = w.dom.Var
      Try(proofLift(w(x), tp(x.asInstanceOf[a]))).flatten
        .map((g: Term) => x :~> (g: Term))
    case _ => Failure(new Exception("could not lift proof"))
  }

  import ConstructorShape._

}

abstract class TermIndMod(name: Name,
                          inductiveTyp: Term,
                          intros: Vector[Term],
                          params: Vector[Term],
                          isPropn: Boolean) {
  val numParams = params.length

  // def proofRelevant(fib: Term): Option[Term] =
  //   if (isPropn) LeanToTerm.proofLift(inductiveTyp, fib) else Some(fib)

  val introsFolded =
    intros
      .map((rule) => translation.TermLang.applnFold(rule, params))
      .flatten
  // TODO  is reverse correct?

  val recName = Name.Str(name, "rec")

  def recFromTyp(typ: Typ[Term]): Option[Term]

  import translation.TermLang

  def recDefn(base: => Parser): OptParser = {
    case (exp: Expr) => {
      defn(exp, base)
    }
  }

  def defn(exp: Expr, predef: (Expr) => Try[Term]): Option[Term] = {
    val argsFmlyOpt = LeanToTerm.iterAp(recName, numParams + 1)(exp)
    argsFmlyOpt.flatMap { (argsFmly) =>
      getRec(argsFmly, predef).toOption
    }
  }

  def getRec(argsFmly: Vector[Expr], predef: Expr => Try[Term]): Try[Term]

}

case class SimpleIndMod(name: Name,
                        typ: Typ[Term],
                        intros: Vector[Term],
                        params: Vector[Term],
                        isPropn: Boolean)
    extends TermIndMod(name, typ, intros, params, isPropn: Boolean) {
  lazy val ind = ConstructorSeqTL.getExst(typ, introsFolded).value

  // println(s"inductive type: ${typ.fansi}")
  // println(s"params: ${params map (_.fansi)}")
  // println(s"introsFolded: ${introsFolded.map(_.fansi)}\n")

  import LeanToTerm.unifier

  import implicits._

  def getInd(dom: Typ[Term]) =
    unifier(typ, dom, numParams).map { (vec) =>
      vec.foldLeft(ind) { case (a, (x, y)) => a.subst(x, y) }
    }

  import scala.util.Try

  def getRec(argsFmly: Vector[Expr], predef: Expr => Try[Term]): Try[Term] = {
    val newParams = argsFmly.init.map((t) => predef(t).toOption).flatten
    val indNew = (newParams.zipWithIndex).foldLeft(ind) {
      case (a, (y, n)) => a.subst(params(n), y)
    }
    // println(s"New simple inductive type: ${indNew.typ.fansi}")

    val fmlyOpt = predef(argsFmly.last)

    // println(s"type argument ${argsFmly.last}\n parsed as ${fmlyOpt.map(
    //   _.fansi)}\n with type ${fmlyOpt.map(_.typ.fansi)}")

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
      case tp: Typ[u] if (isPropn) =>
        val x = typ.Var
        if (tp.dependsOn(x)) {
          (indNew.inducE((x: Term) :-> (tp: Typ[u])))
        } else (indNew.recE(tp))
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

case class NoIndexedInducE(mod: IndexedIndMod,
                           fmlOpt: Option[Term],
                           exp: Expr,
                           W: Term,
                           family: Any,
                           newParams: Vector[Term]
                           // predef: (Expr, Option[Typ[Term]]) => Option[Term]
) extends Exception("no final cod")

case class IndexedIndMod(name: Name,
                         typF: Term,
                         intros: Vector[Term],
                         params: Vector[Term],
                         isPropn: Boolean)
    extends TermIndMod(name, typF, intros, params, isPropn) {
  lazy val ind =
    TypFamilyExst.getIndexedConstructorSeq(typF, introsFolded).value

  import ind.family

  import LeanToTerm.unifier

  import implicits._

  def getInd(domF: Term) =
    unifier(typF, domF, numParams).map { (vec) =>
      vec.foldLeft(ind) { case (a, (x, y)) => a.subs(x, y) }
    }

  def getRec(argsFmly: Vector[Expr], predef: Expr => Try[Term]): Try[Term] = {
    val newParams = argsFmly.init.map((t) => predef(t).toOption).flatten
    val indNew = (newParams.zipWithIndex).foldLeft(ind) {
      case (a, (y, n)) => a.subs(params(n), y)
    }
    val fmlOptRaw = predef(argsFmly.last)
    val fmlOpt =
      if (isPropn)
        fmlOptRaw.flatMap((fib) => LeanToTerm.proofLift(indNew.W, fib))
      else fmlOptRaw
    // println(s"${fmlOpt0.map(_.fansi)} ; ${fmlOpt.map(_.fansi)}; $isPropn")
    val recOpt =
      for {
        fml <- fmlOpt
        cod <- Try(family.constFinalCod(fml).get)
      } yield indNew.recE(cod)
    val inducOpt =
      fmlOpt.map((fib) => indNew.inducE(fib))
    recOpt orElse inducOpt

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
