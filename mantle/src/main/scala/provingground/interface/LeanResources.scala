package provingground.interface

import provingground._
import os._
import monix.eval._
import LeanInterface._
import LeanParser._
import provingground.HoTT._
import trepplein.{Modification, Name}
import monix.execution.CancelableFuture
import provingground.translation.TeXTranslate

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable
import upickle.default.{read => _}

import scala.util.Try

object LeanResources {
  def index: IndexedSeq[String] =
    read.lines(resource / "index.txt").filter(_.endsWith(".lean.export"))

  val modTasks: Map[String, Task[Vector[Modification]]] = index.map { (name) =>
    val path = resource / name
    val in   = new java.io.ByteArrayInputStream(read.bytes(path))
    name -> Task(getModsFromStream(in)).memoize
  }.toMap

  val loadedNames: mutable.Set[Name] = mutable.Set()

  lazy val baseParser: LeanParser =
    new LeanParser(Seq(),
                   library.LeanMemo.defTaskMap,
                   library.LeanMemo.indTaskMap)

  val defnMap: mutable.Map[Name, Term] = mutable.Map()

  val termIndModMap: mutable.Map[Name, TermIndMod] = mutable.Map()

  val mods: ArrayBuffer[Modification] = ArrayBuffer.empty[Modification]

  val parseCanc: ArrayBuffer[(String, CancelableFuture[Try[Term]])] =
    ArrayBuffer()

  def loadTask(f: String): Task[Unit] =
    if (loadedFiles.contains(f)) Task.pure(())
    else
      modTasks(f).map { (m) =>
        mods ++= m
        loadedNames ++= m.map(_.name)
        loadedFiles += f
      }

  val loadedFiles: ArrayBuffer[String] = ArrayBuffer()

  def logUpdate: Logger = {
    case LeanParser.Defined(name, term) => defnMap += name -> term
    case LeanParser.DefinedInduc(name, termIndMod) =>
      termIndModMap += name -> termIndMod
    case LeanParser.ParseWork(expr) => ()
    case LeanParser.Parsed(expr)    => ()
  }

  def indModView(ind: TermIndMod): ujson.Value = {
    def introJs(t: Term) =
      ujson.Obj(
        "name"  -> ujson.Str(t.toString),
        "tex"   -> ujson.Str(TeXTranslate(t.typ, true).replace("'", "\\check ")),
        "plain" -> ujson.Str(t.typ.toString))
    ujson.Obj(
      "type" -> ujson.Str("inductive-definition"),
      "name" -> ujson.Str(ind.name.toString),
      "tex" -> ujson.Str(
        TeXTranslate(ind.typF.typ, true).replace("'", "\\check ")),
      "plain"  -> ujson.Str(ind.typF.typ.toString),
      "intros" -> ujson.Arr(ind.intros.map(introJs): _*)
    )
  }
}
