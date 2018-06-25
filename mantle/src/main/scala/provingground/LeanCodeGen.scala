package provingground.interface
import provingground._, translation._
import induction._
import HoTT.{Name => _, _}

import trepplein._

import scala.meta.{Term => _, Type => _, _}
import ammonite.ops._

object LeanCodeGen {
  def nameCode(name: trepplein.Name) = {
    val pieces =
      name.toString
        .split('.')
        .map(
          (s) => Lit.String(s)
        )
        .toList
    q"trepplein.Name(..$pieces)"
  }
}

case class LeanCodeGen(parser: LeanParser) {
  import parser._
  import LeanCodeGen._

  val base = pwd / "leanlib" / "src" / "main" / "scala" / "provingground" / "library"

  val header = // just write this to a file
    """package provingground.library
import provingground._
import HoTT._
import induction._
import implicits._
import shapeless._
import Fold._
"""

  def defMapCode = {
    val kvs =
      defnMap.map {
        case (name, term) =>
          val termCode =
            if (defNames.contains(name))
              q"${meta.Term.Name(CodeGen.escape(name.toString))}.value"
            else
              codeGen(term).get
          q"${nameCode(name)} -> $termCode"
      }.toList
    q"Map(..$kvs)"
  }

  def defTaskMapCode = {
    val kvs =
      defnMap.map {
        case (name, term) =>
          val termCode =
            if (defNames.contains(name))
              q"${meta.Term.Name(CodeGen.escape(name.toString))}.value"
            else
              codeGen(term).get
          q"${nameCode(name)} -> monix.eval.Task($termCode)"
      }.toList
    q"Map(..$kvs)"
  }



  def vecCode(v: Vector[Term]) = {
    val codes = v.map((t) => codeGen(t).get).toList
    q"Vector(..$codes)"
  }

  def indCode(m: TermIndMod) = {
    val classCode = m match {
      case _: SimpleIndMod  => meta.Term.Name("SimpleIndMod")
      case _: IndexedIndMod => meta.Term.Name("IndexedIndMod")
    }
    q"""$classCode(
      ${nameCode(m.name)},
      ${codeGen(m.typF).get},
      ${vecCode(m.intros)},
      ${Lit.Int(m.numParams)},
      ${Lit.Boolean(m.isPropn)}
    )"""
  }

  def indMapCode = {
    val kvs =
      termIndModMap.map {
        case (name, m) =>
          q"${nameCode(name)} -> ${indCode(m)}"
      }.toList
    q"Map(..$kvs)"

  }

  def indTaskMapCode = {
    val kvs =
      termIndModMap.map {
        case (name, m) =>
          q"${nameCode(name)} -> monix.eval.Task(${indCode(m)})"
      }.toList
    q"Map(..$kvs)"

  }

  def memoObj =
    q"""
object LeanMemo {
  val defMap = $defMapCode

  val indMap = $indMapCode

  val defTaskMap = $defTaskMapCode

  val indTaskMap = $indTaskMapCode
}
"""

  def writeDefn(name: trepplein.Name, code: meta.Term) = {
    val obj  = CodeGen.mkObject(name.toString, code)
    val file = base / "definitions" / s"$name.scala"
    write.over(file, header)

    write.append(file, obj.toString + "\n")
  }

  def writeInduc(name: trepplein.Name, ind: TermIndMod) = {
    val code = codeFromInd(ind)
    val obj  = CodeGen.mkObject(s"${name}Ind", code)
    val file = base / "inductive-types" / s"${name}Ind.scala"
    write.over(file, header)

    write.append(file, obj.toString + "\n")
  }

  def save() = {
    defnCode.foreach { case (name, code)     => writeDefn(name, code) }
    termIndModMap.foreach { case (name, ind) => writeInduc(name, ind) }
  }

  def memo() = {
    val file = pwd / "mantle" / "src" / "main" / "scala" / "provingground" / "LeanMemo.scala"
    write.over(file, header + "\nimport interface._\n")

    write.append(file, memoObj.toString + "\n")
  }

}
