package provingground

import HoTT._

object Context{
  case object Empty extends Context{
    val constants : Vector[Term] = Vector()

    val variables : Vector[Term] = Vector()

    val definitions: Vector[Context.Defn[Term]] = Vector()

    def export(t: Term): Term = t

    def exportTyp(typ: Typ[Term]): Typ[Term] = typ

  }

  case class Defn[+U <: Term with Subs[U]](name: Term, value: U){
    def map(f: Term => Term) = Defn(name, f(value))
  }

  case class AppendDefn[U <: Term with Subs[U]](init: Context, defn: Defn[U]) extends Context{
    val constants = init.constants

    val variables = init.variables

    val definitions = init.definitions :+ defn.map(init.export)

    def export(t: Term) = init.export(t.replace(defn.name, defn.value))

    def exportTyp(t: Typ[Term]) = init.exportTyp(t.replace(defn.name, defn.value))
  }

  case class AppendConstant[U <: Term with Subs[U]](init: Context, constant: U) extends Context{
    val constants = init.constants :+ init.export(constant)

    val variables = init.variables

    val definitions = init.definitions

    def export(t: Term) = init.export(t)

    def exportTyp(t: Typ[Term]) = init.exportTyp(t)
  }

  case class AppendVariable[U <: Term with Subs[U]](init: Context, variable: U) extends Context{
    val constants = init.constants

    val variables = init.variables :+ init.export(variable)

    val definitions = init.definitions

    def export(t: Term) =
      init.export(
        if (t.dependsOn(variable)) variable :~> t else t
      )

    def exportTyp(t: Typ[Term]) =
      init.exportTyp(
        if (t.dependsOn(variable)) variable ~>: t else t
      )
  }
}

trait Context{
  import Context._

  val constants : Vector[Term]

  val variables : Vector[Term]

  val definitions: Vector[Context.Defn[Term]]

  def export(t: Term): Term

  def exportTyp(typ: Typ[Term]): Typ[Term]

  def define[U <: Term with Subs[U]](name: Term, value: U) = AppendDefn(this, Defn(name, value))

  def addConstant[U <: Term with Subs[U]](const: U) = AppendConstant(this, const)

  def addVariable[U <: Term with Subs[U]](variable: U) = AppendConstant(this, variable)

  def assume(tp: Typ[Term], text: String ="assumption") = addConstant(text :: tp)

  def given[U <: Term with Subs[U]](v: Term) = addVariable(v)

}
