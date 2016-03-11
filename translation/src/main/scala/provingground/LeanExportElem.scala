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
  
  implicit class DataBase(dat: Vector[Data]){
    val map = (dat map ((data) => ((data.index, data.tpe.take(2)), data))).toMap
    
    def find(index: Long, tpe: String) = map.get((index, tpe))
    
    def getName(index: Long) : Option[Name] =
      if (index == 0) Some(Name.anonymous)
      else find(index, "#N") flatMap {
        case Data(_, "#NS", List(nid, name)) =>
          getName(nid.toLong) map (Name.NameString(_, name))
        case Data(_, "#NI", List(nid, id)) =>
          getName(nid.toLong) map (Name.NameLong(_, id.toLong))
      }
    
    def getUniv(index: Long): Option[Univ] =
      find(index, "#U") flatMap {
        case Data(_, "#US", List(uid)) => getUniv(uid.toLong) map (Univ.Succ(_))
        case Data(_, "#UM", List(uid1, uid2)) =>
          for (a <- getUniv(uid1.toLong); b <- getUniv(uid2.toLong)) yield (Univ.Max(a, b))
        case Data(_, "#UIM", List(uid1, uid2)) =>
          for (a <- getUniv(uid1.toLong); b <- getUniv(uid2.toLong)) yield (Univ.IMax(a, b))
        case Data(_, "#UP", List(nid)) => getName(nid.toLong) map (Univ.Param(_))
        case Data(_, "#UG", List(nid)) => getName(nid.toLong) map (Univ.Global(_))
      }
    
    def getExpr(index: Long): Option[Expr] =
      find(index, "#E") flatMap {
        case Data(_, "#EV", List(ind)) => Some(Expr.Var(ind.toLong))
        case Data(_, "#ES", List(uid)) =>
          getUniv(uid.toLong) map (Expr.Sort(_))
        case Data(_, "#EA", List(eid1, eid2)) =>
          for (a <- getExpr(eid1.toLong); b <- getExpr(eid2.toLong)) yield (Expr.Appln(a, b))
        case Data(_, "#EC", nid :: uids) =>
          {
            val univs = (uids map ((x) => getUniv(x.toLong))).flatten
            getName(nid.toLong) map (Expr.Const(_, univs))
          }
        case Data(_, "#EL", List(info, nid, eid1, eid2)) =>
          for (
            a <- getName(nid.toLong);
            b <- getExpr(eid1.toLong);
            c <- getExpr(eid2.toLong)
          ) yield (Expr.Lambda(Info.get(info), a, b, c))
        case Data(_, "#EP", List(info, nid, eid1, eid2)) =>
          for (
            a <- getName(nid.toLong);
            b <- getExpr(eid1.toLong);
            c <- getExpr(eid2.toLong)
          ) yield (Expr.Pi(Info.get(info), a, b, c))
      }
   
    def readGlobalUniv(command: String) : Option[GlobalUniv] = 
      if (command.startsWith("#UNI")) getName(command.split(' ')(1).toLong) map (GlobalUniv(_))
      else None
    
    def readDef(command: String) : Option[Definition] = {
      if (command.startsWith("#DEF"))
        {
          val args = command.drop(5)
          val Array(headArgs, tailArgs) = args split ('|') map ((s) => s.split(' '))
          val nid  = headArgs.head.toLong
          val nids = headArgs.tail.map(_.toLong).toList
          val (eid1, eid2) = (tailArgs(1).toLong, tailArgs(2).toLong)
          val univParams = (nids map ((x) => getName(x.toLong))).flatten
          for (
            a <- getName(nid.toLong);
            b <- getExpr(eid1.toLong);
            c <- getExpr(eid2.toLong)
          ) yield (Definition(a, univParams, b, c))
        }
      else None
    }
    
    def readDefs(lines: Vector[String]) = (lines map (readDef)).flatten
    
    def readAxiom(command: String) : Option[Axiom] = {
      if (command.startsWith("#DEF"))
        {
          val args = command.drop(5)
          val Array(headArgs, tailArgs) = args split ('|') map ((s) => s.split(' '))
          val nid  = headArgs.head.toLong
          val nids = headArgs.tail.map(_.toLong).toList
          val eid = tailArgs(1).toLong
          val univParams = (nids map ((x) => getName(x.toLong))).flatten
          for (
            a <- getName(nid.toLong);
            b <- getExpr(eid.toLong)
          ) yield (Axiom(a, univParams, b))
        }
      else None
    }
    
    def readAxioms(lines: Vector[String]) = (lines map (readAxiom)).flatten
    
    def readBind(line: String) : Option[Bind] =
      if (line.startsWith("#BIND"))
        {
          val params = line.split(' ').tail map (_.toLong)
          val numParam = params(0).toInt
          val numTypes = params(1).toInt
          val univParams = (params.drop(2) map ((x) => getName(x))).flatten.toList
          Some(Bind(numParam, numTypes, univParams))
        }
      else None
    
    def readInd(line: String) = 
      if (line.startsWith("#IND"))
      {
      val Array(nid, eid) = line.split(' ').tail map (_.toLong)
      for (a <- getName(nid); b <- getExpr(eid)) yield (Ind(a, b))
    }
      else None
  
    def readIntro(line: String) =
      if (line.startsWith("#INTRO"))
      {
      val Array(nid, eid) = line.split(' ').tail map (_.toLong)
      for (a <- getName(nid); b <- getExpr(eid)) yield (Intro(a, b))
    }
      else None

    def readInducDefn(lines: Vector[String]) =
      InducDefn(readInd(lines.head).get, lines.tail map (readIntro(_).get))

    def readInducDefnFrom(lines: Vector[String]) = {
      val ls = lines.head +: (lines.tail.takeWhile(_.startsWith("#INTRO")))
      readInducDefn(ls)
    }

    def readAllInducDefn(lines: Vector[String], n: Int): Vector[InducDefn] =  {
      if (n == 1) Vector(readInducDefnFrom(lines))
      else {
        val head = readInducDefnFrom(lines)
        head +: readAllInducDefn(lines drop (head.size), n-1)
      }
    }
    
    def readInducBlock(lines: Vector[String]) = {
      readBind(lines.head) map {(bind) =>
        InducBlock(bind, readAllInducDefn(lines.tail, bind.numTypes))
      }
    }
    
    def readAllInducBlock(lines: Vector[String]) : Vector[InducBlock] = {
      InducBlock.getBlock(lines) flatMap {(block: Vector[String]) =>
        val head = readInducBlock(block)
        val tail = readAllInducBlock(lines drop(block.size+1))
        head map (_ +: tail)
      }
    }.getOrElse(Vector())
  }

  sealed trait Name extends LeanExportElem

  object Name {
    def readAtom(s: String, env: Name) : Name = 
      Try(NameLong(env, s.toLong)).toOption getOrElse(NameString(env, s))
    
    /**
     * Reads a String, assuming the first part of the name represents anonymous.
     */
    def read(s: String) : Name = {
        val l = s.split(".")
        if (l.length > 1) readAtom(l.last, read(l.init.mkString("."))) else anonymous
      }
    
    case object anonymous extends Name{
      override def toString = "@"
    }

    case class NameString(env: Name, name: String) extends Name{
      override def toString = env.toString()+"."+name
    }

    case class NameLong(env: Name, number: Long) extends Name{
      override def toString = env.toString()+"."+number.toString
    }

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

  sealed trait Expr extends LeanExportElem{
    val constants: List[Name]
  }

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

    case class Var(index: Long) extends Expr{
      val constants = List()
    }

    case class Sort(univ: Univ) extends Expr{
      val constants = List()
    }

    case class Const(name: Name, univs: List[Univ]) extends Expr{
      val constants = List(name)
    }

    case class Appln(func: Expr, arg: Expr) extends Expr{
      val constants = func.constants ++ arg.constants
    }

    case class Lambda(info: Info, varName: Name, variable: Expr, value: Expr) extends Expr{
      val constants = varName :: value.constants
    }

    case class Pi(info: Info, varName: Name, variable: Expr, value: Expr) extends Expr{
      val constants = varName :: value.constants
    }
  }

  sealed trait Import extends LeanExportElem

  object Import {
    case class Direct(file: Name) extends Import

    case class Relative(backtrack: Long, file: Name) extends Import
  }

  case class GlobalUniv(name: Name) extends LeanExportElem


  case class Definition(name: Name, univParams: List[Name] = List(), tpe: Expr, value: Expr) extends LeanExportElem{
    def dependents = tpe.constants ++ value.constants
    
    def linePickle = (name :: dependents).mkString("\t")
  }

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

    def defnMap(defs : Vector[Definition]) = {
      (defs map ((d) => (d.name, d.dependents))).toMap
    }
  }

  case class Axiom(name: Name, univParams: List[Name] = List(), tpe: Expr) extends LeanExportElem{
    def dependents = tpe.constants
  }

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

  case class Bind(numParam: Int, numTypes: Int, univParams: List[Name]) extends LeanExportElem

  object Bind{
    def read(line: String, ds: Vector[Data]) : Option[Bind] =
      if (line.startsWith("#BIND"))
        {
          val params = line.split(' ').tail map (_.toLong)
          val numParam = params(0).toInt
          val numTypes = params(1).toInt
          val univParams = (params.drop(2) map ((x) => Name.get(ds, x))).flatten.toList
          Some(Bind(numParam, numTypes, univParams))
        }
      else None
  }

  case object Eind extends LeanExportElem

  case class Ind(name: Name, tpe: Expr) extends LeanExportElem

  object Ind{
    def read(line: String, ds: Vector[Data]) =
      if (line.startsWith("#IND"))
      {
      val Array(nid, eid) = line.split(' ').tail map (_.toLong)
      for (a <- Name.get(ds, nid); b <- Expr.get(ds, eid)) yield (Ind(a, b))
    }
      else None
  }

  case class Intro(name: Name, tpe: Expr) extends LeanExportElem

  object Intro{
    def read(line: String, ds: Vector[Data]) =
      if (line.startsWith("#INTRO"))
      {
      val Array(nid, eid) = line.drop(7) split(' ') map (_.toLong)
      for (a <- Name.get(ds, nid); b <- Expr.get(ds, eid)) yield (Intro(a, b))
    }
      else None
  }

  case class InducDefn(ind: Ind, cons: Vector[Intro]){
    def size = cons.size + 1
  }

  object InducDefn{
    def read(lines: Vector[String], ds: Vector[Data]) =
      InducDefn(Ind.read(lines.head,ds).get, lines.tail map (Intro.read(_, ds).get))

    def readFrom(lines: Vector[String], ds: Vector[Data]) = {
      val ls = lines.head +: (lines.tail.takeWhile(_.startsWith("#INTRO")))
      read(ls, ds)
    }

    def readAll(lines: Vector[String], ds: Vector[Data], n: Int): Vector[InducDefn] =  {
      if (n == 1) Vector(readFrom(lines, ds))
      else {
        val head = readFrom(lines, ds)
        head +: readAll(lines drop (head.size), ds, n-1)
      }
    }
  }

  case class InducBlock(bind: Bind, defns: Vector[InducDefn])

  object InducBlock{
    def read(lines: Vector[String], ds: Vector[Data]) = {
      Bind.read(lines.head, ds) map {(bind) =>
        InducBlock(bind, InducDefn.readAll(lines.tail, ds, bind.numTypes))
      }
    }

    def getBlock(lines: Vector[String]) = {
      val block =
        (lines dropWhile ((x) => !(x.startsWith("#BIND")))) takeWhile((x) => !(x.startsWith("#EIND")))
      if (block.size > 0) Some(block) else None
    }

    def readAll(lines: Vector[String]) : Vector[InducBlock] = {
      val ds = Data.readAll(lines)
      getBlock(lines) flatMap {(block: Vector[String]) =>
        val head = InducBlock.read(block, ds)
        val tail = readAll(lines drop(block.size+1))
        head map (_ +: tail)
      }
    }.getOrElse(Vector())
  }
}
