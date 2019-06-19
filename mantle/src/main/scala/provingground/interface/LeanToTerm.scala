package provingground.interface
import provingground._

import os._
import scala.util._

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global

import HoTT.{Name => _, _}

import translation.{TermLang => TL}
import trepplein._

import LeanToTerm._

import translation.FansiShow._

import scala.collection.mutable.{Map => mMap, ArrayBuffer}

import LeanInterface._

trait LeanParse { self =>
  val defnMap: collection.Map[Name, Term]

  val termIndModMap: collection.Map[Name, TermIndMod]

  def defns(exp: Expr, typOpt: Option[Typ[Term]]) = exp match {
    case Const(name, _) => defnMap.get(name)
    case _              => None
  }

  val parse: Parser = recParser(parse)

  val parseOpt: OptParser = recOptParser(parseOpt)

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

  val inPropFamily: Term => Boolean = {
    case FormalAppln(f, _) => inPropFamily(f)
    case s: Symbolic =>
      val name = trepplein.Name(s.name.toString.split('.'): _*)
      termIndModMap.get(name).exists(_.isPropn)
    case _ => false
  }

  def applyFuncProp(func: Term,
                    arg: Term,
                    vars: Vector[Term],
                    data: Vector[Expr] = Vector()): Term =
    Try(applyFuncLean(func, arg))
      .fold(
        (exc) =>
          if (inPropFamily(arg.typ)) func
          else
            throw LeanContextException(exc, vars, data),
        (t) => t
      )

  def applyFuncPropOpt(func: Term, arg: Term): Option[Term] =
    applyFuncOpt(func, arg).orElse {
      if (inPropFamily(arg.typ)) Some(func) else None
    }

  def recOptParser(rec: => OptParser)(exp: Expr,
                                      vars: Vector[Term]): Option[Term] =
    exp match {
      case Predef(t) => Some(t)
      case Sort(_)   => Some(Type)
      case Var(n)    => Some(vars(n))
      case RecIterAp(name, args) =>
        val indMod         = termIndModMap(name)
        val (argsFmly, xs) = args.splitAt(indMod.numParams + 1)
        val argsFmlyTerm   = parseVecOpt(argsFmly, vars)
        val recFnOpt: Option[Term] = indMod
          .getRecOpt(argsFmlyTerm)
        for {
          recFn <- recFnOpt
          vec   <- parseVecOpt(xs, vars)
          res <- vec.foldLeft(Option(recFn)) {
            case (xo, y) => xo.flatMap(applyFuncPropOpt(_, y))
          }
        } yield res
      case App(a, b) =>
        for {
          func <- rec(a, vars)
          arg  <- rec(b, vars)
          res  <- applyFuncPropOpt(func, arg)
        } yield res
      case Lam(domain, body) =>
        for {
          domTerm <- rec(domain.ty, vars)
          domTyp  <- toTypOpt(domTerm)
          x = domTyp.Var
          value <- parseOpt(body, x +: vars)
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
          domTerm <- rec(domain.ty, vars)
          domTyp  <- toTypOpt(domTerm)
          x = domTyp.Var
          // withVar = addVar(x)
          value <- parseOpt(body, x +: vars)
          cod   <- toTypOpt(value)
        } yield if (cod.dependsOn(x)) PiDefn(x, cod) else x.typ ->: cod
      case Let(domain, value, body) =>
        for {
          domTerm <- rec(domain.ty, vars)
          domTyp  <- toTypOpt(domTerm)
          x = domTyp.Var
          valueTerm <- recOptParser(rec)(value, vars)
          // withVar = addVar(x)
          bodyTerm <- parseOpt(body, x +: vars)
        } yield bodyTerm.replace(x, valueTerm)
      case _ => None
    }

  def recParser(rec: => Parser)(exp: Expr, vars: Vector[Term]): Try[Term] =
    exp match {
      case Predef(t) => Success(t)
      case Sort(_)   => Success(Type)
      case Var(n)    => Try(vars(n))
      case RecIterAp(name, args) =>
        val indMod         = termIndModMap(name)
        val (argsFmly, xs) = args.splitAt(indMod.numParams + 1)
        val argsFmlyTerm   = parseVec(argsFmly, vars)
        val recFnTry: Try[Term] = indMod
          .getRecTry(argsFmlyTerm)
          .fold((ex) =>
                  throw RecFuncException(indMod,
                                         argsFmly.map(parse(_, vars)),
                                         xs.map(parse(_, vars)),
                                         ex),
                (x) => Try(x))
        for {
          recFn <- recFnTry
          vec   <- parseVec(xs, vars)
          resTry = Try(vec.foldLeft(recFn)(applyFuncProp(_, _, vars, Vector())))
            .fold((ex) =>
                    throw RecFuncException(indMod,
                                           argsFmly.map(parse(_, vars)),
                                           xs.map(parse(_, vars)),
                                           ex),
                  (x) => Try(x))
          res <- resTry
        } yield res
      case App(a, b) =>
        for {
          func <- rec(a, vars)
          arg  <- rec(b, vars)
          res  <- Try(applyFuncProp(func, arg, vars, Vector(a, b)))
        } yield res
      case Lam(domain, body) =>
        for {
          domTerm <- rec(domain.ty, vars)
          domTyp  <- Try(toTyp(domTerm))
          x = domTyp.Var
          // withVar = addVar(x)
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
          domTerm <- rec(domain.ty, vars)
          domTyp  <- Try(toTyp(domTerm))
          x = domTyp.Var
          // withVar = addVar(x)
          value <- parse(body, x +: vars)
          cod   <- Try(toTyp(value))
        } yield if (cod.dependsOn(x)) PiDefn(x, cod) else x.typ ->: cod
      case Let(domain, value, body) =>
        for {
          domTerm <- rec(domain.ty, vars)
          domTyp  <- Try(toTyp(domTerm))
          x = domTyp.Var
          valueTerm <- rec(value, vars)
          // withVar = addVar(x)
          bodyTerm <- parse(body, x +: vars)
        } yield bodyTerm.replace(x, valueTerm)
      case e => Failure(UnParsedException(e))
    }

  // def addVar(t: Term) = self.copy(vars = t +: self.vars)

  def parseTypOpt(x: Expr, vars: Vector[Term]) =
    parseOpt(x, vars).flatMap(toTypOpt)

  def parseTyp(x: Expr, vars: Vector[Term]): Try[Typ[Term]] =
    parse(x, vars).flatMap {
      case tp: Typ[_] => Success(tp)
      case t          =>
        // println(
        //   s"got term $t of type ${t.typ} but expected type when parsing $x")
        throw NotTypeException(t)
    }

  def parseVec(vec: Vector[Expr], vars: Vector[Term]): Try[Vector[Term]] =
    vec match {
      case Vector() => Success(Vector())
      case x +: ys =>
        for {
          head <- parse(x, vars)
          tail <- parseVec(ys, vars)
        } yield head +: tail
    }

  def parseVecOpt(vec: Vector[Expr], vars: Vector[Term]): Option[Vector[Term]] =
    optSequence(vec.map(parseOpt(_, vars)))

  def parseTypVec(vec: Vector[Expr],
                  vars: Vector[Term]): Try[Vector[Typ[Term]]] = vec match {
    case Vector() => Success(Vector())
    case x +: ys =>
      for {
        head <- parseTyp(x, vars)
        tail <- parseTypVec(ys, vars)
      } yield head +: tail
  }

  def parseSymVec(vec: Vector[(Name, Expr)],
                  vars: Vector[Term]): Try[Vector[Term]] = vec match {
    case Vector() => Success(Vector())
    case (name, expr) +: ys =>
      for {
        tp <- parseTyp(expr, vars)
        head = name.toString :: tp
        tail <- parseSymVec(ys, vars)
      } yield head +: tail
  }

  def parseSymVecOpt(vec: Vector[(Name, Expr)],
                     vars: Vector[Term]): Option[Vector[Term]] =
    optSequence(vec.map {
      case (name, expr) => parseSymOpt(name, expr, vars)
    })

  def parseSymOpt(name: Name, ty: Expr, vars: Vector[Term]) =
    parseTypOpt(ty, vars).map(name.toString :: _)

  def parseSym(name: Name, ty: Expr, vars: Vector[Term]) =
    parseTyp(ty, vars).map(name.toString :: _)

  def parseVar(b: Binding, vars: Vector[Term]) =
    parseSym(b.prettyName, b.ty, vars)

  def addAxiom(name: Name, exp: Expr): LeanParse

  def addAxiomOpt(name: Name, exp: Expr): LeanParse

  def addDefnVal(name: Name, ty: Expr, value: Expr): LeanParse

  def addDefnValOpt(name: Name, ty: Expr, value: Expr): LeanParse

  def toTermIndModTry(ind: IndMod): Try[TermIndMod] = {
    val inductiveTypOpt = parseTyp(ind.ty, Vector())
    val isPropn         = LeanToTerm.isPropn(ind.ty)
    inductiveTypOpt.flatMap { (inductiveTyp) =>
      val name = ind.name
      val typF = name.toString :: inductiveTyp
      val typValueOpt =
        LeanToTerm.getValue(typF, ind.numParams, Vector())
      val withTypeName = addAxiom(name, ind.ty)
      val introsTry =
        withTypeName.parseSymVec(ind.intros, Vector())
      introsTry.flatMap { (intros) =>
        typValueOpt.map { (typValue) =>
          typValue match {
            case (typ: Typ[Term], params) =>
              SimpleIndMod(ind.name, typF, intros, params.size, isPropn)
            case (t, params) =>
              IndexedIndMod(ind.name, typF, intros, params.size, isPropn)
          }
        }
      }
    }
  }

  def toTermIndModOpt(ind: IndMod): Option[TermIndMod] = {
    val inductiveTypOpt = parseTypOpt(ind.ty, Vector())
    val isPropn         = LeanToTerm.isPropn(ind.ty)
    inductiveTypOpt.flatMap { (inductiveTyp) =>
      val name = ind.name
      val typF = name.toString :: inductiveTyp
      val typValueOpt =
        LeanToTerm.getValueOpt(typF, ind.numParams, Vector())
      val withTypeName = addAxiomOpt(name, ind.ty)
      val introsOpt = ind.intros.map {
        case (name, tp) =>
          withTypeName
            .parseTypOpt(tp, Vector())
            .map(name.toString :: _)
      }
      val introsTry: Option[Vector[Term]] =
        withTypeName.parseSymVecOpt(ind.intros, Vector())
      introsTry.flatMap { (intros) =>
        typValueOpt.map { (typValue) =>
          typValue match {
            case (typ: Typ[Term], params) =>
              SimpleIndMod(ind.name, typF, intros, params.size, isPropn)
            case (t, params) =>
              IndexedIndMod(ind.name, typF, intros, params.size, isPropn)
          }
        }
      }
    }
  }

}

case class LeanToTerm(defnMap: Map[Name, Term],
                      termIndModMap: Map[Name, TermIndMod],
                      unparsed: Vector[Name])
    extends LeanParse { self =>

  def addDefnMap(name: Name, term: Term): LeanToTerm =
    self.copy(defnMap = self.defnMap + (name -> term))

  def addDefnVal(name: Name, value: Expr, tp: Expr): LeanToTerm = {
    // val typ = parseTyp(tp)
    parse(value, Vector())
      .map((t) => addDefnMap(name, t))
      .getOrElse(self.copy(unparsed = self.unparsed :+ name))
  }

  def addDefnValOpt(name: Name, value: Expr, tp: Expr): LeanToTerm = {
    // val typ = parseTypOpt(tp)
    parseOpt(value, Vector())
      .map((t) => addDefnMap(name, t))
      .getOrElse(self.copy(unparsed = self.unparsed :+ name))
  }

  override def addAxiom(name: Name, ty: Expr): LeanToTerm =
    parseSym(name, ty, Vector())
      .map(addDefnMap(name, _))
      .getOrElse(self.copy(unparsed = self.unparsed :+ name))

  override def addAxiomOpt(name: Name, ty: Expr): LeanToTerm =
    parseSymOpt(name, ty, Vector())
      .map(addDefnMap(name, _))
      .getOrElse(self.copy(unparsed = self.unparsed :+ name))

  def addAxioms(axs: Vector[(Name, Expr)]): LeanToTerm =
    axs.foldLeft(self) { case (p, (n, v)) => p.addAxiom(n, v) }

  def addAxiomsOpt(axs: Vector[(Name, Expr)]): LeanToTerm =
    axs.foldLeft(self) { case (p, (n, v)) => p.addAxiomOpt(n, v) }

  def addIndMod(ind: IndMod): LeanToTerm = {
    val withTypDef = addAxiomOpt(ind.name, ind.ty)
    val withAxioms = withTypDef.addAxioms(ind.intros)
    val indOpt     = withAxioms.toTermIndModTry(ind)
    indOpt
      .map { (indMod) =>
        withAxioms.copy(
          termIndModMap = self.termIndModMap + (ind.name -> indMod))
      }
      .getOrElse {
        // println(s"no rec definitions for ${ind.name}")
        withAxioms.copy(unparsed = self.unparsed :+ Name.Str(ind.name, "rec"))
      }
  }

  def addIndModOpt(ind: IndMod): LeanToTerm = {
    val withTypDef = addAxiomOpt(ind.name, ind.ty)
    val withAxioms = withTypDef.addAxiomsOpt(ind.intros)
    val indOpt     = withAxioms.toTermIndModOpt(ind)
    indOpt
      .map { (indMod) =>
        withAxioms
          .copy(termIndModMap = self.termIndModMap + (ind.name -> indMod))
      }
      .getOrElse {
        withAxioms.copy(unparsed = self.unparsed :+ Name.Str(ind.name, "rec"))
      }
  }

  def addAxiomMod(ax: AxiomMod): LeanToTerm =
    addAxiom(ax.name, ax.ty)

  def addDefMod(df: DefMod): LeanToTerm =
    addDefnVal(df.name, df.value, df.ty)

  def addAxiomModOpt(ax: AxiomMod): LeanToTerm =
    addAxiomOpt(ax.name, ax.ty)

  def addDefModOpt(df: DefMod): LeanToTerm =
    addDefnValOpt(df.name, df.value, df.ty)

  def addQuotMod: LeanToTerm = {
    import quotient._
    val axs = Vector(quot, quotLift, quotMk, quotInd).map { (ax) =>
      (ax.name, ax.ty)
    }
    addAxioms(axs)
  }

  def addQuotModOpt: LeanToTerm = {
    import quotient._
    val axs = Vector(quot, quotLift, quotMk, quotInd).map { (ax) =>
      (ax.name, ax.ty)
    }
    addAxiomsOpt(axs)
  }

  def add(mod: Modification): LeanToTerm = mod match {
    case ind: IndMod  => addIndMod(ind)
    case ax: AxiomMod => addAxiomMod(ax)
    case df: DefMod   => addDefMod(df)
    case QuotMod      => addQuotMod
  }

  def addOpt(mod: Modification): LeanToTerm = mod match {
    case ind: IndMod  => addIndModOpt(ind)
    case ax: AxiomMod => addAxiomModOpt(ax)
    case df: DefMod   => addDefModOpt(df)
    case QuotMod      => addQuotModOpt
  }

}

object LeanToTermMut {
  def fromMods(mods: Seq[Modification]) = {
    val init = LeanToTermMut(mMap(), mMap())
    mods.foreach((m) => init.add(m))
    init
  }

  def fromModsOpt(mods: Seq[Modification]) = {
    val init = LeanToTermMut(mMap(), mMap())
    mods.foreach((m) => init.addOpt(m))
    init
  }
}

case class LeanToTermMut(defnMap: mMap[trepplein.Name, Term],
                         termIndModMap: mMap[Name, TermIndMod],
                         unparsed: ArrayBuffer[Name] = ArrayBuffer())
    extends LeanParse { self =>

  def addDefnMap(name: Name, term: Term): LeanParse = {
    defnMap += (name -> term)
    self
  }

  def addDefnVal(name: Name, value: Expr, tp: Expr): LeanParse = {
    parse(value, Vector())
      .foreach((term) => defnMap += (name -> term))
    self
  }

  def addDefnValOpt(name: Name, value: Expr, tp: Expr): LeanParse = {
    parseOpt(value, Vector())
      .foreach((term) => defnMap += (name -> term))
    self
  }

  def putAxiom(name: Name, ty: Expr): Unit =
    parseSym(name, ty, Vector())
      .foreach((term) => defnMap += (name -> term))

  override def addAxiom(name: Name, ty: Expr): LeanParse = {
    putAxiom(name, ty)
    self
  }

  def putAxiomOpt(name: Name, ty: Expr): Unit =
    parseSymOpt(name, ty, Vector())
      .foreach((term) => defnMap += (name -> term))

  override def addAxiomOpt(name: Name, ty: Expr): LeanParse = {
    putAxiomOpt(name, ty)
    self
  }

  def addAxioms(axs: Vector[(Name, Expr)]): LeanParse = {
    axs
      .foreach { case (name, ty) => putAxiom(name, ty) }
    self
  }

  def addAxiomsOpt(axs: Vector[(Name, Expr)]) = {
    axs
      .foreach { case (name, ty) => putAxiomOpt(name, ty) }
    self
  }

  def addIndMod(ind: IndMod): LeanParse = {
    putAxiom(ind.name, ind.ty)
    ind.intros.foreach { case (n, t) => putAxiom(n, t) }
    val indOpt = toTermIndModTry(ind)
    indOpt
      .foreach { (indMod) =>
        termIndModMap += (ind.name -> indMod)
      }
    self
  }

  def addIndModOpt(ind: IndMod): LeanParse = {
    putAxiomOpt(ind.name, ind.ty)
    ind.intros.foreach { case (n, t) => putAxiom(n, t) }
    val indOpt = toTermIndModOpt(ind)
    indOpt
      .foreach { (indMod) =>
        termIndModMap += (ind.name -> indMod)
      }
    self
  }

  def addAxiomMod(ax: AxiomMod): LeanParse =
    addAxiom(ax.name, ax.ty)

  def addDefMod(df: DefMod): LeanParse =
    addDefnVal(df.name, df.value, df.ty)

  def addAxiomModOpt(ax: AxiomMod): LeanParse =
    addAxiomOpt(ax.name, ax.ty)

  def addDefModOpt(df: DefMod): LeanParse =
    addDefnValOpt(df.name, df.value, df.ty)

  def addQuotMod: LeanParse = {
    import quotient._
    val axs = Vector(quot, quotLift, quotMk, quotInd).map { (ax) =>
      (ax.name, ax.ty)
    }
    addAxioms(axs)
  }

  def addQuotModOpt: LeanParse = {
    import quotient._
    val axs = Vector(quot, quotLift, quotMk, quotInd).map { (ax) =>
      (ax.name, ax.ty)
    }
    addAxiomsOpt(axs)
  }

  def add(mod: Modification): LeanParse = mod match {
    case ind: IndMod  => addIndMod(ind)
    case ax: AxiomMod => addAxiomMod(ax)
    case df: DefMod   => addDefMod(df)
    case QuotMod      => addQuotMod
  }

  def addOpt(mod: Modification): LeanParse = mod match {
    case ind: IndMod  => addIndModOpt(ind)
    case ax: AxiomMod => addAxiomModOpt(ax)
    case df: DefMod   => addDefModOpt(df)
    case QuotMod      => addQuotModOpt
  }

}


case class LeanContextException(exc: Throwable,
                                vars: Vector[Term],
                                args: Vector[Expr] = Vector())
    extends Exception("error while parsing lean")

case class UnParsedException(exp: Expr)
    extends IllegalArgumentException(s"could not parse expression $exp")

case class NoConstantException(name: Name)
    extends IllegalArgumentException(s"No constant with name $name found")

case class RecFuncException(indMod: TermIndMod,
                            argsFmly: Vector[Try[Term]],
                            xs: Vector[Try[Term]],
                            exception: Throwable)
    extends Exception("Could not parse recursive definition")

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

  def emptyRecParser(base: => Parser)(e: Expr, vars: Vector[Term]) = {
    // println("trying rec definition")
    None
  }

  val empty =
    LeanToTerm(Map(), Map(), Vector())

  def fromMods(mods: Vector[Modification], init: LeanToTerm = empty) =
    mods.foldLeft(init) { case (l: LeanToTerm, m: Modification) => l.add(m) }

  def fromModsOpt(mods: Vector[Modification], init: LeanToTerm = empty) =
    mods.foldLeft(init) {
      case (l: LeanToTerm, m: Modification) => l.addOpt(m)
    }

  def iterMods(mods: Vector[Modification], init: LeanToTerm = empty) =
    mods.toIterator.scanLeft(init) {
      case (l: LeanToTerm, m: Modification) => l.add(m)
    }

  def iterModsOpt(mods: Vector[Modification], init: LeanToTerm = empty) =
    mods.toIterator.scanLeft(init) {
      case (l: LeanToTerm, m: Modification) => l.addOpt(m)
    }

  type TypedParser = (Expr, Option[Typ[Term]]) => Option[Term]

  type Parser = (Expr, Vector[Term]) => Try[Term]

  type OptParser = (Expr, Vector[Term]) => Option[Term]


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



  def getValueOpt(t: Term,
                  n: Int,
                  accum: Vector[Term]): Option[(Term, Vector[Term])] =
    (t, n) match {
      case (x, 0) => Some(x -> accum)
      case (l: LambdaLike[u, v], n) if n > 0 =>
        getValueOpt(l.value, n - 1, accum :+ l.variable)
      case (fn: FuncLike[u, v], n) if n > 0 =>
        val x = fn.dom.Var
        getValueOpt(fn(x), n - 1, accum :+ x)
      case _ => None
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

  def optSequence[A](vec: Vector[Option[A]]): Option[Vector[A]] =
    if (vec.contains(None)) None else Some(vec.map(_.get))
}
