package provingground

import HoTT._

object Context {

  case object Empty extends Context {
    val constants: Vector[Term] = Vector()

    val variables: Vector[Term] = Vector()

    val terms = Vector()

    val definitions: Vector[Context.Defn[Term]] = Vector()

    def export(t: Term): Term = t

    def exportTyp(typ: Typ[Term]): Typ[Term] = typ
  }

  case class Defn[+U <: Term with Subs[U]](name: Term, value: U) {
    def map(f: Term => Term) = Defn(name, f(value))
  }

  case class AppendDefn[U <: Term with Subs[U]](
    init: Context,
    defn: Defn[U],
    global: Boolean)
    extends Context {
    val constants = init.constants

    val variables = init.variables

    val terms = init.terms

    val definitions =
      if (global) init.definitions :+ defn.map(init.export)
      else init.definitions

    def export(t: Term) = init.export(t.replace(defn.name, defn.value))

    def exportTyp(t: Typ[Term]) =
      init.exportTyp(t.replace(defn.name, defn.value))
  }

  case class AppendConstant[U <: Term with Subs[U]](init: Context, constant: U)
    extends Context {
    val constants = init.constants :+ init.export(constant)

    val variables = init.variables

    val terms = init.terms

    val definitions = init.definitions

    def export(t: Term) = init.export(t)

    def exportTyp(t: Typ[Term]) = init.exportTyp(t)
  }

  sealed trait Role

  case object Assert extends Role

  case object Consider extends Role

  case class AppendTerm[U <: Term with Subs[U]](
    init: Context,
    term: U,
    role: Role)
    extends Context {
    val constants = init.constants

    val variables = init.variables

    val terms = init.terms :+ init.export(term)

    val definitions = init.definitions

    def export(t: Term) = init.export(t)

    def exportTyp(t: Typ[Term]) = init.exportTyp(t)
  }

  case class AppendVariable[U <: Term with Subs[U]](init: Context, variable: U)
    extends Context {
    val constants = init.constants

    val variables = init.variables :+ init.export(variable)

    val terms = init.terms

    val definitions = init.definitions

    def export(t: Term) =
      init.export(
        if (t.dependsOn(variable)) variable :~> t else t)

    def exportTyp(t: Typ[Term]) =
      init.exportTyp(
        if (t.dependsOn(variable)) variable ~>: t else t)
  }
}

trait Context {
  import Context._

  val constants: Vector[Term]

  val variables: Vector[Term]

  val terms: Vector[Term]

  val definitions: Vector[Context.Defn[Term]]

  def export(t: Term): Term

  def exportTyp(typ: Typ[Term]): Typ[Term]

  def define[U <: Term with Subs[U]](name: Term, value: U) =
    AppendDefn(this, Defn(name, value), true)

  def let[U <: Term with Subs[U]](name: Term, value: U) =
    AppendDefn(this, Defn(name, value), false)

  def addConstant[U <: Term with Subs[U]](const: U) =
    AppendConstant(this, const)

  def addVariable[U <: Term with Subs[U]](variable: U) =
    AppendConstant(this, variable)

  def assume(tp: Typ[Term], text: String = "assumption") =
    addConstant(text :: tp)

  def assert(tp: Typ[Term]) = introduce(tp, Assert)

  def given[U <: Term with Subs[U]](v: Term) = addVariable(v)

  def introduce[U <: Term with Subs[U]](t: U, role: Role = Consider) =
    AppendTerm(this, t, role)
}
