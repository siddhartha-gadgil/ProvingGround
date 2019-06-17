package provingground.interface
import provingground._, induction._

import os._
import scala.util._

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global

import HoTT.{Name => _, _}

import translation.{TermLang => TL}
import trepplein._

import translation.FansiShow._

import scala.collection.mutable.{Map => mMap, ArrayBuffer}

import LeanInterface._


object LeanInterface {
  def domVarFlag(introTyp: Typ[Term], typF: Term): Vector[Boolean] =
  introTyp match {
    case pd: PiDefn[u, v] =>
      (pd.domain.dependsOn(typF)) +: domVarFlag(pd.value, typF)
    case ft: FuncTyp[u, v] =>
      (ft.dom.dependsOn(typF)) +: domVarFlag(ft.codom, typF)
    case _ => Vector()
  }

    /**
    * shuffles the dat variables from leans convention where they are last to an interleaved form.
    *
    * @param intro the introduction rule
    * @param typF the inductive type
    * @param data the recursion data
    */
    def introShuffle(
      intro: Term,
      typF: Term,
      data: Term,
      numParams: Int
  ): Term = {
    val flag    = domVarFlag(intro.typ, typF).drop(numParams)
    val codSize = flag.count(identity)
    pprint.log(s"${data.typ.fansi}, $flag, ${intro.typ.fansi}")
    println(intro.typ.fansi)
    println(data.fansi)
    println(data.typ.fansi)
    if (codSize < 2) data
    else {
      val (allVars, value) = getVars(data, flag.size + codSize)
      val (baseVars: Vector[Term], codVars: Vector[Term]) =
        allVars.splitAt(flag.size)
      // val (vars, _) = maxLambda(intro)
      // val domVars = vars.filter(_.typ.dependsOn(typF)) // number of variables in the codomain
      val domVars  = baseVars.zip(flag).filter(_._2).map(_._1)
      val varPairs = domVars.zip(codVars)
      val interLeaved = baseVars.flatMap(
        (v) =>
          varPairs
            .find(_._1 == v)
            .map { case (a, b) => Vector(a, b) }
            .getOrElse(Vector(v))
      )

      pprint.log(
        s"${domVars.size} domain vars in $intro for $typF; all variable types ${allVars
          .map(_.typ)}"
      )
      println(allVars.map(_.fansi))
      println(interLeaved.map(_.fansi))
      polyLambda(interLeaved.toList, value)
    }
  }


  /**
    * fill in witnesses if proposition, including within lambdas
    **/
  def witLess(t: Term): Vector[Term] = {
    val topFilled: Vector[Term] = t match {
      case l : LambdaLike[u, v] if isWitness(l.variable) =>
        l.variable.typ match {
          case pd : PiDefn[u, v] if isWitness(pd.variable) /* && isPropFmly(pd.typ)*/ =>
            val x = "_" :: pd.value
            pprint.log(s"nested witnesses: ${l.variable}, , ${pd.variable}, ${pd.value}, ${isWitness(pd.variable)}")
            witLess(l.value) ++ witLess(l.value).map(lambda(x))
          case _ => witLess(l.value)
        }

      case fn: FuncLike[u, v] if isPropFmly(fn.dom) =>
        witLess(fn(fn.dom.Var.asInstanceOf[u]))
      case _ => Vector()
    }
    val recFilled = t match {
      case l: LambdaFixed[u, v] => witLess(l.value).map(LambdaFixed(l.variable, _))
      case l: LambdaTerm[u, v] => witLess(l.value).map(LambdaTerm(l.variable, _))
      case _                   => Vector(t)
    }

//    pprint.log(recFilled)
//    pprint.log(topFilled)
    recFilled ++ topFilled
  }

  def applyFuncLean(func: Term, arg: Term): Term = func match {
    // case fn: Func[u, v] if isWitness(arg) => "_" :: fn.codom
    case fn: FuncLike[u, v] if fn.dom == arg.typ =>
      fn.applyUnchecked(arg.asInstanceOf[u])
    case fn if isWitness(arg) =>
      pprint.log(fn)
      pprint.log(arg)
      fn
    case fn: FuncLike[u, v] =>
      witLess(arg)
        .find(_.typ == fn.dom)
        .map{x =>
          pprint.log(s"$arg becomes $x for $fn")
          fn.applyUnchecked(x.asInstanceOf[u])}
        .getOrElse(throw new ApplnFailException(func, arg))
    case _ => throw new ApplnFailException(func, arg)
  }


  def foldFuncLean(func: Term, args: Vector[Term]): Term =
    args.foldLeft(func)(applyFuncLean) // FIXME should just apply a function


  def consts(expr: Expr): Vector[Name] = expr match {
    case Const(name, _)   => Vector(name)
    case App(x, y)        => consts(x) ++ consts(y)
    case Var(_)           => Vector()
    case Sort(_)          => Vector()
    case Lam(b, x)        => consts(b.ty) ++ consts(x)
    case Pi(b, x)         => consts(b.ty) ++ consts(x)
    case Let(_, x, y)     => consts(x) ++ consts(y)
    case LocalConst(_, _) => Vector()
  }

  def usesVar(expr: Expr, index: Int, ignoreTypes: Boolean = false): Boolean =
    expr match {
      case Const(name, _) => false
      case App(x, y)      => usesVar(x, index) || usesVar(y, index)
      case Var(n)         => n == index
      case Sort(_)        => false
      case Lam(b, x) =>
        usesVar(x, index + 1) || ((!ignoreTypes) && usesVar(b.ty, index))
      case Pi(b, x) =>
        usesVar(x, index + 1) || ((!ignoreTypes) && usesVar(b.ty, index))
      case Let(_, x, y)     => usesVar(y, index + 1)
      case LocalConst(_, _) => false
    }

  def varsUsed(expr: Expr): Set[Int] =
    expr match {
      case Const(name, _) => Set()
      case App(x, y)      => varsUsed(x) union varsUsed(y)
      case Var(n)         => Set(n)
      case Sort(_)        => Set()
      case Lam(b, x) =>
        varsUsed(x).map(_ + 1) union varsUsed(b.ty)
      case Pi(b, x) =>
        varsUsed(x).map(_ + 1) union varsUsed(b.ty)
      case Let(_, x, y)     => varsUsed(y).map(_ + 1)
      case LocalConst(_, _) => Set()
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

  def modSubExpr: Modification => Set[Expr] = {
    case df: DefMod =>
      subExpr(df.ty).toSet union subExpr(df.value).toSet
    case ax: AxiomMod =>
      subExpr(ax.ty).toSet
    case ind: IndMod =>
      val tyExprs = (subExpr(ind.ty)).toSet
      val introExprs =
        for {
          (_, ty) <- ind.intros
          exp     <- subExpr(ty)
        } yield ty
      tyExprs union (introExprs.toSet)
    case QuotMod => Set()
  }

  def recApp: Expr => Boolean = {
    case exp @ App(Const(Name.Str(_, "rec"), _), _) => true
    case _                                          => false
  }

  @annotation.tailrec
  def defnNames(mods: Vector[Modification],
                accum: Vector[Name] = Vector()): Vector[Name] = mods match {
    case Vector() => accum
    case IndMod(name, _, ty, numParams, intros) +: tail =>
      defnNames(tail,
                name +: Name.Str(name, "rec") +: intros.map(_._1) ++: accum)
    case DefMod(name, _, ty, value) +: tail => defnNames(tail, name +: accum)
    case AxiomMod(name, _, ty) +: tail      => defnNames(tail, name +: accum)
    case QuotMod +: tail =>
      defnNames(tail,
                Vector(Name("quot"),
                       Name("quot", "ind"),
                       Name("quot", "mk"),
                       Name("quot", "lift")) ++: accum)
  }

  def defnExprs(mods: Vector[Modification]): Vector[Expr] = mods.collect {
    case DefMod(name, _, ty, value) => value
  }

  def getMods(filename: String): Vector[Modification] = {
    val exportedCommands =
      TextExportParser.parseFile(filename).toVector

    exportedCommands.collect { case ExportedModification(mod) => mod }
  }

  def getModsFromStream(in: java.io.InputStream): Vector[Modification] = {
    val exportedCommands =
      TextExportParser.parseStream(in).toVector

    exportedCommands.collect { case ExportedModification(mod) => mod }
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
}



sealed trait TermIndMod {
  val name: Name
  val intros: Vector[Term]
  val numParams: Int
  val isPropn: Boolean

  val typF: Term

  

  def interleaveData(v: Vector[Term]) : Vector[Term] = {
    val (dataBase, extra) = v.splitAt(intros.size)
    val newBase = (intros.zip(dataBase)).map{
      case (in, dat) => introShuffle(in, typF, dat, numParams)
    }
    newBase ++ extra
  }


  def introsFold(p: Vector[Term]): Vector[Term] = intros.map((rule) => foldFuncLean(rule, p))

  val recName = Name.Str(name, "rec")


  def getRecTry(argsFmlyTerm: Try[Vector[Term]]): Try[Term]

  def getRecOpt(argsFmlyTerm: Option[Vector[Term]]): Option[Term]
}


case class SimpleIndMod(name: Name,
                        typF: Term,
                        intros: Vector[Term],
                        numParams: Int,
                        isPropn: Boolean)
    extends TermIndMod {


  def getInd(p: Vector[Term]) =
    ConstructorSeqTL.getExst(toTyp(foldFuncLean(typF, p)), introsFold(p)).value




  import LeanInterface.unifier

  import implicits._

  import scala.util.Try

  def getRecOpt(argsFmlyTerm: Option[Vector[Term]]): Option[Term] = {
    val newParamsOpt = argsFmlyTerm map (_.init)
    newParamsOpt.flatMap { (newParams) =>
      val indNew =
        getInd(newParams)
      val fmlyOpt = argsFmlyTerm map (_.last)
      fmlyOpt map {
        case l: LambdaLike[u, v] =>
          // println(s"family seen: ${l.fansi} : ${l.typ.fansi}")
          l.value match {
            case tp: Typ[u] =>
              if (tp.dependsOn(l.variable))(indNew.inducE(
                (l.variable: Term) :-> (tp: Typ[u])))
              else (indNew.recE(tp))
          }
        case fn: FuncLike[u, v] =>
          // println(s"family seen ${fn.fansi} : ${fn.typ.fansi}")
          val x = fn.dom.Var
          val y = fn(x)
          y match {
            case tp: Typ[u] =>
              if (tp.dependsOn(x)) {
                (indNew.inducE((x: Term) :-> (tp: Typ[u])))
              } else (indNew.recE(tp))
          }
        case pt: PiDefn[u, v] if isPropn && pt.domain == indNew.typ =>
          indNew.inducE(
            (pt.variable : Term) :-> pt.value
            // LambdaFixedpt.variable, pt.value)
          )
        case tp: Typ[u] if (isPropn) =>
          val x = tp.Var
          if (tp.dependsOn(x)) {
            (indNew.inducE((x: Term) :-> (tp: Typ[u])))
          } else (indNew.recE(tp))
      }

    }
  }

  def getRecTry(argsFmlyTerm: Try[Vector[Term]]): Try[Term] = {
    val newParamsTry = argsFmlyTerm map (_.init)
    newParamsTry.flatMap { (newParams) =>
      val indNew =
        getInd(newParams)
      val fmlyOpt = argsFmlyTerm map (_.last)
      fmlyOpt map {
        case l: LambdaLike[u, v] =>
          // println(s"family seen: ${l.fansi} : ${l.typ.fansi}")
          l.value match {
            case tp: Typ[u] =>
              if (tp.dependsOn(l.variable))(indNew.inducE(
                (l.variable: Term) :-> (tp: Typ[u])))
              else (indNew.recE(tp))
          }
        case fn: FuncLike[u, v] =>
          // println(s"family seen ${fn.fansi} : ${fn.typ.fansi}")
          val x = fn.dom.Var
          val y = fn(x)
          y match {
            case tp: Typ[u] =>
              if (tp.dependsOn(x)) {
                (indNew.inducE((x: Term) :-> (tp: Typ[u])))
              } else (indNew.recE(tp))
          }
        case pt: PiDefn[u, v] if isPropn && pt.domain == indNew.typ =>
          indNew.inducE(
            (pt.variable : Term) :-> pt.value
            // LambdaFixed(pt.variable, pt.value)
          )
        case tp: Typ[u] if (isPropn) =>
          val x = tp.Var
          if (tp.dependsOn(x)) {
            (indNew.inducE((x: Term) :-> (tp: Typ[u])))
          } else (indNew.recE(tp))
      }

    }
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
                         numParams: Int,
                         isPropn: Boolean)
    extends TermIndMod {
  // val typF = foldFuncLean(typF, params)

  def getInd(p: Vector[Term]) =
    TypFamilyExst
      .getIndexedConstructorSeq(foldFuncLean(typF, p), introsFold(p))
      .value


  import LeanInterface.unifier

  import implicits._

  def getRecOpt(argsFmlyTerm: Option[Vector[Term]]): Option[Term] = {
    val newParamsOpt = argsFmlyTerm map (_.init)
    newParamsOpt.flatMap { (newParams) =>
      val indNew =
        getInd(newParams)
      val fmlOptRaw = argsFmlyTerm map (_.last)
      val fmlOpt =
        if (isPropn)
          fmlOptRaw.flatMap((fib) => LeanToTerm.proofLiftOpt(indNew.W, fib))
        else fmlOptRaw
      val recOpt =
        for {
          fml <- fmlOpt
          cod <- indNew.family.constFinalCod(fml)
        } yield indNew.recE(cod)
      val inducOpt =
        fmlOpt.map((fib) => indNew.inducE(fib))
      recOpt orElse inducOpt
    }
  }

  def getRecTry(argsFmlyTerm: Try[Vector[Term]]): Try[Term] = {
    val newParamsTry = argsFmlyTerm map (_.init)
    newParamsTry.flatMap { (newParams) =>
      val indNew =
        getInd(newParams)
      val fmlOptRaw = argsFmlyTerm map (_.last)
      val fmlOpt =
        if (isPropn)
          fmlOptRaw.flatMap((fib) => LeanToTerm.proofLift(indNew.W, fib))
        else fmlOptRaw
      val recOpt =
        for {
          fml <- fmlOpt
          cod <- Try(indNew.family.constFinalCod(fml).get)
        } yield indNew.recE(cod)
      val inducOpt =
        fmlOpt.map((fib) => indNew.inducE(fib))
      recOpt orElse inducOpt
    }
  }

}
