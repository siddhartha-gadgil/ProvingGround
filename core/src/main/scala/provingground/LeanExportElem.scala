package provingground

import scala.util.Try

sealed trait LeanExportElem

object LeanExportElem {
  case class Data(index: Int, tpe: String, params: List[String])

  object Data {
    def find(ds: List[Data], index: Int, tpes: String*) =
      ds find ((d) => d.index == index && tpes.toSet.contains(d.tpe))

    def get(s: String) : Option[Data] = Try{
      val head :: tpe :: params = s.split(" ").toList
      Data(head.toInt, tpe, params)
    }.toOption
  }

  sealed trait Name extends LeanExportElem

  object Name {
    case object anonymous extends Name

    case class NameString(env: Name, name: String) extends Name

    case class NameInt(env: Name, number: Int) extends Name

    def get(ds: List[Data], index: Int): Option[Name] =
      if (index == 0) Some(anonymous)
      else Data.find(ds, index, "#NS", "#NI") flatMap {
        case Data(_, "#NS", List(nid, name)) =>
          get(ds, nid.toInt) map (NameString(_, name))
        case Data(_, "#NI", List(nid, id)) =>
          get(ds, nid.toInt) map (NameInt(_, id.toInt))
      }
  }

  sealed trait Univ extends LeanExportElem

  object Univ {
    case class Succ(base: Univ) extends Univ

    case class Max(first: Univ, second: Univ) extends Univ

    case class IMax(first: Univ, second: Univ) extends Univ

    case class Param(name: Name) extends Univ

    case class Global(name: Name) extends Univ

    def get(ds: List[Data], index: Int): Option[Univ] =
      Data.find(ds, index, "#US", "#UM", "#UIM", "#UP", "#UG") flatMap {
        case Data(_, "#US", List(uid)) => get(ds, uid.toInt) map (Succ(_))
        case Data(_, "#UM", List(uid1, uid2)) =>
          for (a <- get(ds, uid1.toInt); b <- get(ds, uid2.toInt)) yield (Max(a, b))
        case Data(_, "#UIM", List(uid1, uid2)) =>
          for (a <- get(ds, uid1.toInt); b <- get(ds, uid2.toInt)) yield (IMax(a, b))
        case Data(_, "#UP", List(nid)) => Name.get(ds, nid.toInt) map (Param(_))
        case Data(_, "#UG", List(nid)) => Name.get(ds, nid.toInt) map (Global(_))
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
    def get(ds: List[Data], index: Int): Option[Expr] =
      Data.find(ds, index, "#EV", "#ES", "#EC", "#EA", "#EL", "#EP") flatMap {
        case Data(_, "#EV", List(ind)) => Some(Var(ind.toInt))
        case Data(_, "#ES", List(uid)) =>
          Univ.get(ds, uid.toInt) map (Sort(_))
        case Data(_, "#EA", List(eid1, eid2)) =>
          for (a <- get(ds, eid1.toInt); b <- get(ds, eid2.toInt)) yield (Appln(a, b))
        case Data(_, "#EC", nid :: uids) =>
          {
            val univs = (uids map ((x) => Univ.get(ds, x.toInt))).flatten
            Name.get(ds, nid.toInt) map (Const(_, univs))
          }
        case Data(_, "#EL", List(info, nid, eid1, eid2)) =>
          for (
            a <- Name.get(ds, nid.toInt);
            b <- get(ds, eid1.toInt);
            c <- get(ds, eid2.toInt)
          ) yield (Lambda(Info.get(info), a, b, c))

        case Data(_, "#EP", List(info, nid, eid1, eid2)) =>
          for (
            a <- Name.get(ds, nid.toInt);
            b <- get(ds, eid1.toInt);
            c <- get(ds, eid2.toInt)
          ) yield (Pi(Info.get(info), a, b, c))
      }

    case class Var(index: Int) extends Expr

    case class Sort(univ: Univ) extends Expr

    case class Const(name: Name, univs: List[Univ]) extends Expr

    case class Appln(func: Expr, arg: Expr) extends Expr

    case class Lambda(info: Info, varName: Name, variable: Expr, value: Expr) extends Expr

    case class Pi(info: Info, varName: Name, variable: Expr, value: Expr) extends Expr
  }

  sealed trait Import extends LeanExportElem

  object Import {
    case class Direct(file: Name) extends Import

    case class Relative(backtrack: Int, file: Name) extends Import
  }

  case class GlobalUniv(name: Name) extends LeanExportElem

  object GlobalUniv{
    def get(ds: List[Data], index: Int) : Option[GlobalUniv] =
      Data.find(ds, index, "#UNI") flatMap {
        case Data(_, _, List(nid)) =>
          Name.get(ds, nid.toInt) map (GlobalUniv(_))
      }
  }

  case class Definition(name: Name, univParams: List[Name] = List(), tpe: Expr, value: Expr) extends LeanExportElem

  object Definition{
    def get(ds: List[Data], index: Int) : Option[Definition] =
      Data.find(ds, index, "#DEF") flatMap {
        case Data(_, _, params) =>
          val nid = params.head
          val List(eid1, eid2) = params.takeRight(2)
          val nids = params.tail.dropRight(2)
          val univParams = (nids map ((x) => Name.get(ds, x.toInt))).flatten
          for (
            a <- Name.get(ds, nid.toInt);
            b <- Expr.get(ds, eid1.toInt);
            c <- Expr.get(ds, eid2.toInt)
          ) yield (Definition(a, univParams, b, c))
      }
  }

  case class Axiom(name: Name, univParams: List[Name] = List(), tpe: Expr) extends LeanExportElem

  object Axiom{
    def get(ds: List[Data], index: Int) : Option[Axiom] =
      Data.find(ds, index, "#DEF") flatMap {
        case Data(_, _, params) =>
          val nid = params.head
          val eid = params.last
          val nids = params.tail.init
          val univParams = (nids map ((x) => Name.get(ds, x.toInt))).flatten
          for (
            a <- Name.get(ds, nid.toInt);
            b <- Expr.get(ds, eid.toInt)
          ) yield (Axiom(a, univParams, b))
      }
  }

  case class Bind(numParam: Int, numTypes: Int, univParams: List[Int]) extends LeanExportElem

  case object Eind extends LeanExportElem

  case class Ind(name: Name, tpe: Expr) extends LeanExportElem

  case class Intro(name: Name, tpe: Expr) extends LeanExportElem

}
