package provingground.interface
import provingground._
import translation._
import induction._
import HoTT.{Name => _, _}
import trepplein._

import scala.meta.{Term => _, Type => _, _}
import scala.meta
import ammonite.ops._

import scala.collection.immutable

object LeanCodeGen {
  def nameCode(name: trepplein.Name): meta.Term.Apply = {
    val pieces =
      name.toString
        .split('.')
        .map(
          (s) => Lit.String(s)
        )
        .toList
    // q"trepplein.Name(..$pieces)"
    meta.Term.Apply(meta.Term.Select(meta.Term.Name("trepplein"), meta.Term.Name("Name")), pieces)
  }

  val memoLines: IndexedSeq[String] = read.lines(pwd/ "mantle" / "src" / "main" / "scala" / "provingground" / "LeanMemo.scala").map(_.trim)

  val memoStats: IndexedSeq[Stat] = memoLines.flatMap(_.parse[Stat].toOption)

  val memoDefKV = memoStats.collect{
    case Defn.Def(List(),meta.Term.Name("defMap"),List(),List(), _, meta.Term.Apply(meta.Term.Name("Map"), kvs)
      ) => kvs
    // q"def defMap : $t = Map(..$kvs)" => kvs
  }.head

  val memoDefTaskKV =
    memoStats.collect{
      case Defn.Def(List(),meta.Term.Name("defTaskMap"),List(),List(), _, meta.Term.Apply(meta.Term.Name("Map"), kvs)
        ) => kvs
      // q"def defMap : $t = Map(..$kvs)" => kvs
    }.head
     // memoStats.collect{case q"def defTaskMap : $t = Map(..$kvs)" => kvs}.head

  val memoIndKV =
    memoStats.collect{
      case Defn.Def(List(),meta.Term.Name("indMap"),List(),List(), _, meta.Term.Apply(meta.Term.Name("Map"), kvs)
        ) => kvs
      // q"def defMap : $t = Map(..$kvs)" => kvs
    }.head
    // memoStats.collect{case q"def indMap : $t = Map(..$kvs)" => kvs}.head

  val memoIndTaskKV =
    memoStats.collect{
      case Defn.Def(List(),meta.Term.Name("indTaskMap"),List(),List(), _, meta.Term.Apply(meta.Term.Name("Map"), kvs)
        ) => kvs
      // q"def defMap : $t = Map(..$kvs)" => kvs
    }.head
    // memoStats.collect{case q"def indTaskMap : $t = Map(..$kvs)" => kvs}.head
}

case class LeanCodeGen(parser: LeanParser) {
  import parser._
  import LeanCodeGen._

  val base
    : Path = pwd / "leanlib" / "src" / "main" / "scala" / "provingground" / "library"

  val header = // just write this to a file
    """package provingground.library
import provingground._
import HoTT._
import induction._
import implicits._
import shapeless._
import Fold._
"""

  def defMapCode: meta.Term.Apply = {
    val kvs =
      defnMap.map {
        case (name, term) =>
          val termCode =
            if (defNames.contains(name))
              meta.Term.Select(
                meta.Term.Name(CodeGen.escape(name.toString)),
                meta.Term.Name("value")
              )
            else
              codeGen(term).get
          // q"${nameCode(name)} -> $termCode"
          meta.Term.ApplyInfix(nameCode(name), meta.Term.Name("->"), List(), List(termCode))
      }.toList ++ memoDefKV
    // q"Map(..$kvs)"
    meta.Term.Apply(
      meta.Term.Name("Map"),
      kvs
    )
  }

  def defTaskMapCode: meta.Term.Apply = {
    val kvs =
      defnMap.map {
        case (name, term) =>
          val termCode =
            if (defNames.contains(name))
            meta.Term.Select(
              meta.Term.Name(CodeGen.escape(name.toString)),
              meta.Term.Name("value")
            )
            else
              codeGen(term).get
              meta.Term.ApplyInfix(nameCode(name), meta.Term.Name("->"), List(),
              List(
                meta.Term.Apply(
                  meta.Term.Name("Task"),
                  List(termCode)))
                )
          // q"${nameCode(name)} -> Task($termCode)"
      }.toList ++ memoDefTaskKV
    // q"Map(..$kvs)"
    meta.Term.Apply(
      meta.Term.Name("Map"),
      kvs
    )
  }

  def vecCode(v: Vector[Term]): meta.Term.Apply = {
    val codes = v.map((t) => codeGen(t).get).toList
    // q"Vector(..$codes)"
    meta.Term.Apply(meta.Term.Name("Vector"), codes)
  }

  def indCode(m: TermIndMod): meta.Term.Apply = {
    val classCode = m match {
      case _: SimpleIndMod  => meta.Term.Name("SimpleIndMod")
      case _: IndexedIndMod => meta.Term.Name("IndexedIndMod")
    }
    // q"""$classCode(
    //   ${nameCode(m.name)},
    //   ${codeGen(m.typF).get},
    //   ${vecCode(m.intros)},
    //   ${Lit.Int(m.numParams)},
    //   ${Lit.Boolean(m.isPropn)}
    // )"""
    meta.Term.Apply(
      classCode, List(nameCode(m.name), codeGen(m.typF).get, vecCode(m.intros), Lit.Int(m.numParams), Lit.Boolean(m.isPropn))
    )
  }

  def indMapCode: meta.Term.Apply = {
    val kvs =
      termIndModMap.map {
        case (name, m) =>
        meta.Term.ApplyInfix(nameCode(name), meta.Term.Name("->"), List(), List(indCode(m)))
          // q"${nameCode(name)} -> ${indCode(m)}"
      }.toList  ++ memoIndKV
      meta.Term.Apply(
        meta.Term.Name("Map"),
        kvs
      )
          // q"Map(..$kvs)"

  }

  def indTaskMapCode: meta.Term.Apply = {
    val kvs =
      termIndModMap.map {
        case (name, m) =>
        meta.Term.ApplyInfix(nameCode(name), meta.Term.Name("->"), List(),
        List(
          meta.Term.Apply(
            meta.Term.Name("Task"),
            List(indCode(m))))
          )
          // q"${nameCode(name)} -> Task(${indCode(m)})"
      }.toList ++ memoIndTaskKV
      meta.Term.Apply(
        meta.Term.Name("Map"),
        kvs
      )
    // q"Map(..$kvs)"

  }

  def memoObj: meta.Defn.Object =
    Defn.Object(
  List(),
  meta.Term.Name("LeanMemo"),
  Template(
    List(),
    List(),
    Self(meta.Name(""), None),
    List(
      Defn.Def(
        List(),
        meta.Term.Name("defMap"),
        List(),
        List(),
        Some(meta.Type.Apply(meta.Type.Name("Map"), List(meta.Type.Select(meta.Term.Name("trepplein"), meta.Type.Name("Name")), meta.Type.Name("Term")))),
        defMapCode
      ),
      Defn.Def(
        List(),
        meta.Term.Name("indMap"),
        List(),
        List(),
        Some(meta.Type.Apply(meta.Type.Name("Map"), List(meta.Type.Select(meta.Term.Name("trepplein"), meta.Type.Name("Name")), meta.Type.Name("TermIndMod")))),
        indMapCode
      ),
      Defn.Def(
        List(),
        meta.Term.Name("defTaskMap"),
        List(),
        List(),
        Some(
          meta.Type.Apply(
            meta.Type.Name("Map"),
            List(meta.Type.Select(meta.Term.Name("trepplein"), meta.Type.Name("Name")), meta.Type.Apply(meta.Type.Name("Task"), List(meta.Type.Name("Term"))))
          )
        ),
        defTaskMapCode
      ),
      Defn.Def(
        List(),
        meta.Term.Name("indTaskMap"),
        List(),
        List(),
        Some(
          meta.Type.Apply(
            meta.Type.Name("Map"),
            List(
              meta.Type.Select(meta.Term.Name("trepplein"), meta.Type.Name("Name")),
              meta.Type.Apply(meta.Type.Name("Task"), List(meta.Type.Name("TermIndMod")))
            )
          )
        ),
        indTaskMapCode
      )
    )
  )
)

//     q"""
//
// object LeanMemo {
//   def defMap : Map[trepplein.Name, Term] = $defMapCode
//
//   def indMap : Map[trepplein.Name, TermIndMod] = $indMapCode
//
//   def defTaskMap : Map[trepplein.Name, Task[Term]] = $defTaskMapCode
//
//   def indTaskMap : Map[trepplein.Name, Task[TermIndMod]] = $indTaskMapCode
// }
// """

  def writeDefn(name: trepplein.Name, code: meta.Term): Unit = {
    val obj  = CodeGen.mkObject(name.toString, code)
    val file = base / "definitions" / s"$name.scala"
    write.over(file, header)

    write.append(file, obj.toString + "\n")
  }

  def writeInduc(name: trepplein.Name, ind: TermIndMod): Unit = {
    val code = codeFromInd(ind)
    val obj  = CodeGen.mkObject(s"${name}Ind", code)
    val file = base / "inductive-types" / s"${name}Ind.scala"
    write.over(file, header)

    write.append(file, obj.toString + "\n")
  }

  def save(): Unit = {
    defnCode.foreach { case (name, code)     => writeDefn(name, code) }
    termIndModMap.foreach { case (name, ind) => writeInduc(name, ind) }
  }

  def memo(): Unit = {
    val file = pwd / "mantle" / "src" / "main" / "scala" / "provingground" / "LeanMemo.scala"
    write.over(file, header + "\nimport interface._\n" + "import monix.eval.Task\n" +  memoObj.toString + "\n")
  }

}
