package provingground.interface

import trepplein._
import Name.Str
import BinderInfo._
import Level._
import ammonite.ops._
import monix.eval.Task
import provingground.library

import scala.collection.mutable.ArrayBuffer
object LeanBug{
  import   LeanInterface._

  val mods: ArrayBuffer[Modification] = ArrayBuffer.empty[Modification]

  def parser =
    new LeanParser(mods,
      library.LeanMemo.defTaskMap,
      library.LeanMemo.indTaskMap)

  def getMods(file: String): Unit= {
    val path    = resource / file
    val in      = new java.io.ByteArrayInputStream(read.bytes(path))
    val newMods = getModsFromStream(in)
    mods ++= newMods

  }

  getMods("basic.lean.export")

  lazy val p: LeanParser = parser

  lazy val expr: Expr =
      p.findDefMod(trepplein.Name("bin_tree", "cases_on")).get.value

  lazy val tsk = p.parse(expr).materialize.map(t => t.failed)

}
