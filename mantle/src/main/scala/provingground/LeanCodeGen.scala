package provingground.interface
import provingground._, translation._
import induction._
import HoTT.{Name => _, _}

import trepplein._

import scala.meta.{Term => _, Type => _, _}
import ammonite.ops._

case class LeanCodeGen(parser: LeanParser){
  import parser._

  val base = pwd / "leanlib" / "src"

  val header = // just write this to a file
"""
package provingground.library
import provingground._
import HoTT._
import implicits._
import shapeless._
"""

  def writeDefn(name: trepplein.Name, codeOpt: Option[meta.Term]) = {
    val obj = CodeGen.mkObject(name.toString, codeOpt.get)
    val file = base / name.toString
    write.over(file, header)

    write.append(file, obj.toString)
  }

  def writeInduc(name: trepplein.Name, ind: TermIndMod) = {
    val code = codeFromInd(ind)
    val obj = CodeGen.mkObject(name.toString, code)
    val file = base / name.toString
    write.over(file, header)

    write.append(file, obj.toString)
  }

  def save() = {
    defnCode.foreach{case (name, code) => writeDefn(name, code)}
    termIndModMap.foreach{case (name, ind) => writeInduc(name, ind)}
  }


}
