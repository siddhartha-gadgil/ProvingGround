package provingground.interface

import trepplein._
import Name.Str
import BinderInfo._
import Level._
import ammonite.ops._
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

  lazy val expr =
      p.findDefMod(trepplein.Name("nat", "pred_lt")).get.value

  lazy val minExpr = subExpr(expr)(24)

  lazy val arg = subExpr(expr)(30)
}
