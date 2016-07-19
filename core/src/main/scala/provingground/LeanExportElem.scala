package provingground

import scala.util.Try

sealed trait LeanExportElem

object LeanExportElem {
  case class Data(index: Long, tpe: String, params: List[String])

  object Data {
    def find(ds: Vector[Data], index: Long, tpes: String*) =
      ds find ((d) => d.index == index && tpes.toSet.contains(d.tpe))

    def read(s: String): Option[Data] =
      Try {
        val head :: tpe :: params = s.split(" ").toList
        Data(head.toLong, tpe, params)
      }.toOption

    def readAll(lines: Vector[String]) = (lines map (read)).flatten
  }

  class DataBase(dat: Vector[Data], lines: Vector[String]) {
    val map =
      (dat map ((data) => ((data.index, data.tpe.take(2)), data))).toMap

    def find(index: Long, tpe: String) = map.get((index, tpe))

    def getName(index: Long): Option[Name] =
      if (index == 0) Some(Name.anonymous)
      else
        find(index, "#N") flatMap {
          case Data(_, "#NS", List(nid, name)) =>
            getName(nid.toLong) map (Name.NameString(_, name))
          case Data(_, "#NI", List(nid, id)) =>
            getName(nid.toLong) map (Name.NameLong(_, id.toLong))
        }

    def getUniv(index: Long): Option[Univ] =
      if (index == 0) Some(Univ.Zero)
      else
        find(index, "#U") flatMap {
          case Data(_, "#US", List(uid)) =>
            getUniv(uid.toLong) map (Univ.Succ(_))
          case Data(_, "#UM", List(uid1, uid2)) =>
            for (a <- getUniv(uid1.toLong); b <- getUniv(uid2.toLong)) yield
              (Univ.Max(a, b))
          case Data(_, "#UIM", List(uid1, uid2)) =>
            for (a <- getUniv(uid1.toLong); b <- getUniv(uid2.toLong)) yield
              (Univ.IMax(a, b))
          case Data(_, "#UP", List(nid)) =>
            getName(nid.toLong) map (Univ.Param(_))
          case Data(_, "#UG", List(nid)) =>
            getName(nid.toLong) map (Univ.Global(_))
        }

    def getExpr(index: Long): Option[Expr] =
      find(index, "#E") flatMap {
        case Data(_, "#EV", List(ind)) => Some(Expr.Var(ind.toInt))
        case Data(_, "#ES", List(uid)) =>
          getUniv(uid.toLong) map (Expr.Sort(_))
        case Data(_, "#EA", List(eid1, eid2)) =>
          for (a <- getExpr(eid1.toLong); b <- getExpr(eid2.toLong)) yield
            (Expr.Appln(a, b))
        case Data(_, "#EC", nid :: uids) =>
          {
            val univs = (uids map ((x) => getUniv(x.toLong))).flatten
            getName(nid.toLong) map (Expr.Const(_, univs))
          }
        case Data(_, "#EL", List(info, nid, eid1, eid2)) =>
          for (a <- getName(nid.toLong);
               b <- getExpr(eid1.toLong);
               c <- getExpr(eid2.toLong)) yield
            (Expr.Lambda(Info.get(info), a, b, c))
        case Data(_, "#EP", List(info, nid, eid1, eid2)) =>
          for (a <- getName(nid.toLong);
               b <- getExpr(eid1.toLong);
               c <- getExpr(eid2.toLong)) yield
            (Expr.Pi(Info.get(info), a, b, c))
      }

    def readGlobalUniv(command: String): Option[GlobalUniv] =
      if (command.startsWith("#UNI"))
        getName(command.split(' ')(1).toLong) map (GlobalUniv(_))
      else None

    def readDef(command: String): Option[Definition] = {
      if (command.startsWith("#DEF")) {
        val args = command.drop(5)
        val Array(headArgs, tailArgs) =
          args split ('|') map ((s) => s.split(' '))
        val nid = headArgs.head.toLong
        val nids = headArgs.tail.map(_.toLong).toList
        val (eid1, eid2) = (tailArgs(1).toLong, tailArgs(2).toLong)
        val univParams = (nids map ((x) => getName(x.toLong))).flatten
        for (a <- getName(nid.toLong);
             b <- getExpr(eid1.toLong);
             c <- getExpr(eid2.toLong)) yield (Definition(a, univParams, b, c))
      } else None
    }

    lazy val getAllDefs = (lines map (readDef)).flatten

    def getDef(name: Name) =
      getAllDefs find ((dfn) => dfn.name == name) map (_.value)

    def readAxiom(command: String): Option[Axiom] = {
      if (command.startsWith("#DEF")) {
        val args = command.drop(5)
        val Array(headArgs, tailArgs) =
          args split ('|') map ((s) => s.split(' '))
        val nid = headArgs.head.toLong
        val nids = headArgs.tail.map(_.toLong).toList
        val eid = tailArgs(1).toLong
        val univParams = (nids map ((x) => getName(x.toLong))).flatten
        for (a <- getName(nid.toLong);
             b <- getExpr(eid.toLong)) yield (Axiom(a, univParams, b))
      } else None
    }

    lazy val readAxioms = (lines map (readAxiom)).flatten

    def readInd(line: String) =
      if (line.startsWith("#IND")) {
        val args = line.drop(5)
        val Array(headArgs, tailArgs) =
          args split ('|') map ((s) => s.split(' '))
        val numParams = headArgs.head.toInt
        val uids = headArgs.tail.map(_.toLong).toList
        val (nid, eid, numConstructors) =
          (tailArgs(1).toLong, tailArgs(2).toLong, tailArgs(3).toInt)
        val univParams = (uids map ((x) => getUniv(x.toLong))).flatten
        for (a <- getName(nid.toLong);
             b <- getExpr(eid.toLong)) yield
          (Ind(numParams, univParams, a, b, numConstructors))
      } else None

    def readIntro(line: String) =
      if (line.startsWith("#INTRO")) {
        val Array(nid, eid) = line.split(' ').tail map (_.toLong)
        for (a <- getName(nid); b <- getExpr(eid)) yield (Intro(a, b))
      } else None

    def readInducDefn(lines: Vector[String]) = {
      val ind = readInd(lines.head).get
      val intros = lines.tail take (ind.numConstructors) map (readIntro(_).get)
      InducDefn(ind, intros)
    }

    def readInducDefnFrom(lines: Vector[String]) = {
      val ls = lines.head +: (lines.tail.takeWhile(_.startsWith("#INTRO")))
      readInducDefn(ls)
    }

    def readNextInducDefn(lines: Vector[String]): Option[InducDefn] = {
      val start = lines dropWhile ((x) => !(x.startsWith("#IND")))
      if (start.isEmpty) None
      else Some(readInducDefnFrom(start))
    }

    def readAllInducDefns(lines: Vector[String],
                          accum: Vector[InducDefn] =
                            Vector()): Vector[InducDefn] = {
      (readNextInducDefn(lines) map ((dfn) =>
                readAllInducDefns(lines drop (dfn.size), dfn +: accum)))
        .getOrElse(accum)
    }

    lazy val getAllInducDefns = readAllInducDefns(lines)
  }

  sealed trait Name extends LeanExportElem

  object Name {
    def readAtom(s: String, env: Name): Name =
      Try(NameLong(env, s.toLong)).toOption getOrElse (NameString(env, s))

    /**
      * Reads a String, assuming the first part of the name represents anonymous.
      */
    def read(s: String): Name = {
      val l = s.split(".")
      if (l.length > 1) readAtom(l.last, read(l.init.mkString(".")))
      else anonymous
    }

    case object anonymous extends Name {
      override def toString = "@"
    }

    case class NameString(env: Name, name: String) extends Name {
      override def toString = env.toString() + "." + name
    }

    case class NameLong(env: Name, number: Long) extends Name {
      override def toString = env.toString() + "." + "#" + number.toString
    }

    def get(ds: Vector[Data], index: Long): Option[Name] =
      if (index == 0) Some(anonymous)
      else
        Data.find(ds, index, "#NS", "#NI") flatMap {
          case Data(_, "#NS", List(nid, name)) =>
            get(ds, nid.toLong) map (NameString(_, name))
          case Data(_, "#NI", List(nid, id)) =>
            get(ds, nid.toLong) map (NameLong(_, id.toLong))
        }
  }

  sealed trait Univ extends LeanExportElem

  object Univ {
    case object Zero extends Univ

    case class Succ(base: Univ) extends Univ

    case class Max(first: Univ, second: Univ) extends Univ

    case class IMax(first: Univ, second: Univ) extends Univ

    case class Param(name: Name) extends Univ

    case class Global(name: Name) extends Univ

    def get(ds: Vector[Data], index: Long): Option[Univ] =
      Data.find(ds, index, "#US", "#UM", "#UIM", "#UP", "#UG") flatMap {
        case Data(_, "#US", List(uid)) => get(ds, uid.toLong) map (Succ(_))
        case Data(_, "#UM", List(uid1, uid2)) =>
          for (a <- get(ds, uid1.toLong); b <- get(ds, uid2.toLong)) yield
            (Max(a, b))
        case Data(_, "#UIM", List(uid1, uid2)) =>
          for (a <- get(ds, uid1.toLong); b <- get(ds, uid2.toLong)) yield
            (IMax(a, b))
        case Data(_, "#UP", List(nid)) =>
          Name.get(ds, nid.toLong) map (Param(_))
        case Data(_, "#UG", List(nid)) =>
          Name.get(ds, nid.toLong) map (Global(_))
      }
  }

  sealed trait Info

  object Info {
    case object BD extends Info

    case object BI extends Info

    case object BS extends Info

    case object BC extends Info

    val get = Map("#BD" -> BD, "#BI" -> BI, "#BS" -> BS, "#BC" -> BC)
  }

  sealed trait Expr extends LeanExportElem {
    val constants: List[Name]
  }

  object Expr {
    def get(ds: Vector[Data], index: Long): Option[Expr] =
      Data.find(ds, index, "#EV", "#ES", "#EC", "#EA", "#EL", "#EP") flatMap {
        case Data(_, "#EV", List(ind)) => Some(Var(ind.toInt))
        case Data(_, "#ES", List(uid)) =>
          Univ.get(ds, uid.toLong) map (Sort(_))
        case Data(_, "#EA", List(eid1, eid2)) =>
          for (a <- get(ds, eid1.toLong); b <- get(ds, eid2.toLong)) yield
            (Appln(a, b))
        case Data(_, "#EC", nid :: uids) =>
          {
            val univs = (uids map ((x) => Univ.get(ds, x.toLong))).flatten
            Name.get(ds, nid.toLong) map (Const(_, univs))
          }
        case Data(_, "#EL", List(info, nid, eid1, eid2)) =>
          for (a <- Name.get(ds, nid.toLong);
               b <- get(ds, eid1.toLong);
               c <- get(ds, eid2.toLong)) yield
            (Lambda(Info.get(info), a, b, c))

        case Data(_, "#EP", List(info, nid, eid1, eid2)) =>
          for (a <- Name.get(ds, nid.toLong);
               b <- get(ds, eid1.toLong);
               c <- get(ds, eid2.toLong)) yield (Pi(Info.get(info), a, b, c))
      }

    case class Var(index: Int) extends Expr {
      val constants = List()
    }

    case class Sort(univ: Univ) extends Expr {
      val constants = List()
    }

    case class Const(name: Name, univs: List[Univ]) extends Expr {
      val constants = List(name)
    }

    case class Appln(func: Expr, arg: Expr) extends Expr {
      val constants = func.constants ++ arg.constants
    }

    case class Lambda(info: Info, varName: Name, varTyp: Expr, value: Expr)
        extends Expr {
      val constants = value.constants
    }

    case class Pi(info: Info, varName: Name, varTyp: Expr, value: Expr)
        extends Expr {
      val constants = value.constants
    }
  }

  sealed trait Import extends LeanExportElem

  object Import {
    case class Direct(file: Name) extends Import

    case class Relative(backtrack: Long, file: Name) extends Import
  }

  case class GlobalUniv(name: Name) extends LeanExportElem

  case class Definition(
      name: Name, univParams: List[Name] = List(), tpe: Expr, value: Expr)
      extends LeanExportElem {
    def dependents = tpe.constants ++ value.constants

    def depPickle = (name :: dependents).map(_.toString.drop(2)).mkString("\t")
  }

  object Definition {
//    def read(command: String, ds: Vector[Data]) : Option[Definition] = {
//      if (command.startsWith("#DEF"))
//        {
//          val args = command.drop(5)
//          val Array(headArgs, tailArgs) = args split ('|') map ((s) => s.split(' '))
//          val nid  = headArgs.head.toLong
//          val nids = headArgs.tail.map(_.toLong).toList
//          val (eid1, eid2) = (tailArgs(1).toLong, tailArgs(2).toLong)
//          val univParams = (nids map ((x) => Name.get(ds, x.toLong))).flatten
//          for (
//            a <- Name.get(ds, nid.toLong);
//            b <- Expr.get(ds, eid1.toLong);
//            c <- Expr.get(ds, eid2.toLong)
//          ) yield (Definition(a, univParams, b, c))
//        }
//      else None
//    }
//
//    def readAll(lines: Vector[String]) = readDefs(lines, (lines map (Data.read)).flatten)
//
//    def readDefs(lines: Vector[String], ds: Vector[Data]) =
//      (lines map (read(_, ds))).flatten

    def defnMap(defs: Vector[Definition]) = {
      (defs map ((d) => (d.name, d.dependents))).toMap
    }
  }

  case class Axiom(name: Name, univParams: List[Name] = List(), tpe: Expr)
      extends LeanExportElem {
    def dependents = tpe.constants
  }

  object Axiom {
//    def read(command: String, ds: Vector[Data]) : Option[Axiom] = {
//      if (command.startsWith("#DEF"))
//        {
//          val args = command.drop(5)
//          val Array(headArgs, tailArgs) = args split ('|') map ((s) => s.split(' '))
//          val nid  = headArgs.head.toLong
//          val nids = headArgs.tail.map(_.toLong).toList
//          val eid = tailArgs(1).toLong
//          val univParams = (nids map ((x) => Name.get(ds, x.toLong))).flatten
//          for (
//            a <- Name.get(ds, nid.toLong);
//            b <- Expr.get(ds, eid.toLong)
//          ) yield (Axiom(a, univParams, b))
//        }
//      else None
//    }
  }

  case class Ind(numParam: Int,
                 univParams: List[Univ],
                 name: Name,
                 tpe: Expr,
                 numConstructors: Int)
      extends LeanExportElem

  object Ind {}

  case class Intro(name: Name, tpe: Expr) extends LeanExportElem

  case class InducDefn(ind: Ind, cons: Vector[Intro]) {
    def size = cons.size + 1
  }
}

import HoTT._
import LeanExportElem._

class LeanToTerm(
    univs: LeanExportElem.Univ => Option[HoTT.Univ] = (u) => Some(Type),
    predef: Expr => Option[Term] = (c) => None,
    env: LeanExportElem.Name => Option[Expr] = (name) => None) {
  import Expr._
  def exprToTerm(
      expr: Expr, variables: Vector[Term] = Vector()): Option[Term] =
    expr match {
      case expr if !predef(expr).isEmpty => predef(expr)
      case Var(n) =>
        Try(variables(n)).toOption
      case Const(name, _) =>
        env(name) flatMap ((e) => exprToTerm(e))
      case Sort(univ) => univs(univ)
      case Appln(func, arg) =>
        exprToTerm(func, variables) match {
          case Some(fn: FuncLike[u, v]) =>
            exprToTerm(arg, variables) flatMap ((x) =>
                  Try(fn(x.asInstanceOf[u])).toOption)
          case _ => None
        }
      case Expr.Lambda(_, name, typExpr, valueExpr) =>
        val typOpt = exprToTerm(typExpr, variables)
        typOpt match {
          case Some(typ: Typ[u]) =>
            val x = name.toString :: typ
            val yOpt = exprToTerm(valueExpr, variables :+ x)
            yOpt map ((y) => lmbda(x)(y))
          case _ => None
        }
      case Expr.Pi(_, name, typExpr, valueExpr) =>
        val typOpt = exprToTerm(typExpr, variables)
        typOpt match {
          case Some(typ: Typ[u]) =>
            val x = name.toString :: typ
            val yOpt = exprToTerm(valueExpr, variables :+ x)
            yOpt match {
              case Some(tp: Typ[v]) =>
                Try(pi(x)(tp)).toOption
              case _ => None
            }
          case _ => None
        }
      case _ => None
    }

  def defnToEquality(dfn: Definition) = {
    val varTypOpt = exprToTerm(dfn.tpe)
    val varOpt: Option[Term] = varTypOpt match {
      case Some(typ: Typ[u]) => Some(dfn.name.toString :: typ)
      case _ => None
    }
    for (x <- varOpt; y <- exprToTerm(dfn.value)) yield (x =:= y)
  }

  def inductiveTypes(induc: InducDefn) = {
    val fullTypOpt = exprToTerm(induc.ind.tpe)

    val paramsOpt =
      fullTypOpt flatMap ((typ) => Try(getVariables(induc.size)(typ)).toOption)

    val fullConstructorList =
      (induc.cons map ((c) => exprToTerm(c.tpe))).flatten

    val formalConsOpt =
      paramsOpt map { (params) =>
        fullConstructorList map (foldterms(_, params))
      }

    val formalTyp = induc.ind.name.toString :: Type

    val valueOpt =
      formalConsOpt map ((formalCons) =>
            InductiveTyp.fromFormal(formalCons.toList, formalTyp))

    val checkTyp =
      fullTypOpt flatMap ((typ) => Try(foldterms(typ, paramsOpt.get)).toOption)

    for (params <- paramsOpt; value <- valueOpt
         if checkTyp == Some(Type)) yield polyLambda(params, value)
  }
}
