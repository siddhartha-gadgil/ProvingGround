package provingground

import scala.util.Try

sealed trait LeanExportElem

object LeanExportElem {
  case class Data(index: Long, tpe: String, params: List[String])

  object Data {
    def find(ds: Vector[Data], index: Long, tpes: String*) =
      ds find ((d) => d.index == index && tpes.toSet.contains(d.tpe))

    def read(s: String) : Option[Data] = Try{
      val head :: tpe :: params = s.split(" ").toList
      Data(head.toLong, tpe, params)
    }.toOption

    def readAll(lines: Vector[String]) = (lines map (read)).flatten
  }

  sealed trait Name extends LeanExportElem

  object Name {
    case object anonymous extends Name

    case class NameString(env: Name, name: String) extends Name

    case class NameLong(env: Name, number: Long) extends Name

    def get(ds: Vector[Data], index: Long): Option[Name] =
      if (index == 0) Some(anonymous)
      else Data.find(ds, index, "#NS", "#NI") flatMap {
        case Data(_, "#NS", List(nid, name)) =>
          get(ds, nid.toLong) map (NameString(_, name))
        case Data(_, "#NI", List(nid, id)) =>
          get(ds, nid.toLong) map (NameLong(_, id.toLong))
      }
  }

  sealed trait Univ extends LeanExportElem

  object Univ {
    case class Succ(base: Univ) extends Univ

    case class Max(first: Univ, second: Univ) extends Univ

    case class IMax(first: Univ, second: Univ) extends Univ

    case class Param(name: Name) extends Univ

    case class Global(name: Name) extends Univ

    def get(ds: Vector[Data], index: Long): Option[Univ] =
      Data.find(ds, index, "#US", "#UM", "#UIM", "#UP", "#UG") flatMap {
        case Data(_, "#US", List(uid)) => get(ds, uid.toLong) map (Succ(_))
        case Data(_, "#UM", List(uid1, uid2)) =>
          for (a <- get(ds, uid1.toLong); b <- get(ds, uid2.toLong)) yield (Max(a, b))
        case Data(_, "#UIM", List(uid1, uid2)) =>
          for (a <- get(ds, uid1.toLong); b <- get(ds, uid2.toLong)) yield (IMax(a, b))
        case Data(_, "#UP", List(nid)) => Name.get(ds, nid.toLong) map (Param(_))
        case Data(_, "#UG", List(nid)) => Name.get(ds, nid.toLong) map (Global(_))
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

  sealed trait Expr extends LeanExportElem

  object Expr {
    def get(ds: Vector[Data], index: Long): Option[Expr] =
      Data.find(ds, index, "#EV", "#ES", "#EC", "#EA", "#EL", "#EP") flatMap {
        case Data(_, "#EV", List(ind)) => Some(Var(ind.toLong))
        case Data(_, "#ES", List(uid)) =>
          Univ.get(ds, uid.toLong) map (Sort(_))
        case Data(_, "#EA", List(eid1, eid2)) =>
          for (a <- get(ds, eid1.toLong); b <- get(ds, eid2.toLong)) yield (Appln(a, b))
        case Data(_, "#EC", nid :: uids) =>
          {
            val univs = (uids map ((x) => Univ.get(ds, x.toLong))).flatten
            Name.get(ds, nid.toLong) map (Const(_, univs))
          }
        case Data(_, "#EL", List(info, nid, eid1, eid2)) =>
          for (
            a <- Name.get(ds, nid.toLong);
            b <- get(ds, eid1.toLong);
            c <- get(ds, eid2.toLong)
          ) yield (Lambda(Info.get(info), a, b, c))

        case Data(_, "#EP", List(info, nid, eid1, eid2)) =>
          for (
            a <- Name.get(ds, nid.toLong);
            b <- get(ds, eid1.toLong);
            c <- get(ds, eid2.toLong)
          ) yield (Pi(Info.get(info), a, b, c))
      }

    case class Var(index: Long) extends Expr

    case class Sort(univ: Univ) extends Expr

    case class Const(name: Name, univs: List[Univ]) extends Expr

    case class Appln(func: Expr, arg: Expr) extends Expr

    case class Lambda(info: Info, varName: Name, variable: Expr, value: Expr) extends Expr

    case class Pi(info: Info, varName: Name, variable: Expr, value: Expr) extends Expr
  }

  sealed trait Import extends LeanExportElem

  object Import {
    case class Direct(file: Name) extends Import

    case class Relative(backtrack: Long, file: Name) extends Import
  }

  case class GlobalUniv(name: Name) extends LeanExportElem

  object GlobalUniv{
    def get(ds: Vector[Data], index: Long) : Option[GlobalUniv] =
      Data.find(ds, index, "#UNI") flatMap {
        case Data(_, _, List(nid)) =>
          Name.get(ds, nid.toLong) map (GlobalUniv(_))
      }
  }

  case class Definition(name: Name, univParams: List[Name] = List(), tpe: Expr, value: Expr) extends LeanExportElem

  object Definition{
    def read(command: String, ds: Vector[Data]) : Option[Definition] = {
      if (command.startsWith("#DEF"))
        {
          val args = command.drop(5)
          val Array(headArgs, tailArgs) = args split ('|') map ((s) => s.split(' '))
          val nid  = headArgs.head.toLong
          val nids = headArgs.tail.map(_.toLong).toList
          val (eid1, eid2) = (tailArgs(1).toLong, tailArgs(2).toLong)
          val univParams = (nids map ((x) => Name.get(ds, x.toLong))).flatten
          for (
            a <- Name.get(ds, nid.toLong);
            b <- Expr.get(ds, eid1.toLong);
            c <- Expr.get(ds, eid2.toLong)
          ) yield (Definition(a, univParams, b, c))
        }
      else None
    }

    def readAll(lines: Vector[String]) = readDefs(lines, (lines map (Data.read)).flatten)

    def readDefs(lines: Vector[String], ds: Vector[Data]) =
      (lines map (read(_, ds))).flatten

  }

  case class Axiom(name: Name, univParams: List[Name] = List(), tpe: Expr) extends LeanExportElem

  object Axiom{
    def read(command: String, ds: Vector[Data]) : Option[Axiom] = {
      if (command.startsWith("#DEF"))
        {
          val args = command.drop(5)
          val Array(headArgs, tailArgs) = args split ('|') map ((s) => s.split(' '))
          val nid  = headArgs.head.toLong
          val nids = headArgs.tail.map(_.toLong).toList
          val eid = tailArgs(1).toLong
          val univParams = (nids map ((x) => Name.get(ds, x.toLong))).flatten
          for (
            a <- Name.get(ds, nid.toLong);
            b <- Expr.get(ds, eid.toLong)
          ) yield (Axiom(a, univParams, b))
        }
      else None
    }

    def readAll(lines: Vector[String]) = readAxs(lines, (lines map (Data.read)).flatten)

    def readAxs(lines: Vector[String], ds: Vector[Data]) =
      (lines map (read(_, ds))).flatten
  }

  case class Bind(numParam: Long, numTypes: Long, univParams: List[Long]) extends LeanExportElem

  case object Eind extends LeanExportElem

  case class Ind(name: Name, tpe: Expr) extends LeanExportElem

  case class Longro(name: Name, tpe: Expr) extends LeanExportElem

}
