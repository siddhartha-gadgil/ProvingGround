package provingground.interface
import provingground._

import LeanExportElem._
import ammonite.ops._
import scala.util.Try

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global

object LeanInterface {
  import trepplein._

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
