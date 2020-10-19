package provingground

import HoTT._
import induction._

object Context {

  case object Empty extends Context {
    def subs(x: Term, y: Term) = this

    val constants: Vector[Term] = Vector()

    val variables: Vector[Term] = Vector()

    val exportVariables: Vector[Term] = Vector()

    val terms = Vector()

    val definitions: Vector[Context.Defn[Term]] = Vector()

    val inductiveDefns: Vector[ExstInducStrucs] = Vector()

    val statements: Vector[String] = Vector()

    def export(t: Term): Term = t

    def exportTyp(typ: Typ[Term]): Typ[Term] = typ

    def exportStrict(t: Term): Term = t

    def exportTypStrict(typ: Typ[Term]): Typ[Term] = typ

    def importOpt(t: Term): Option[Term] = Some(t)

    def importTypOpt(t: Typ[Term]): Option[Typ[Term]] = Some(t)

    val valueOpt: Option[Term] = None
  }

  case class Defn[+U <: Term with Subs[U]](name: Term, value: U) {
    def map(f: Term => Term) = Defn(name, f(value))

    def valueTerm: Term = value

    def subs(x: Term, y: Term) = Defn(name.replace(x, y), value.replace(x, y))
  }

  case class AppendDefn[U <: Term with Subs[U]](
      init: Context,
      defn: Defn[U],
      global: Boolean
  ) extends Context {

    def subs(x: Term, y: Term) =
      AppendDefn(init.subs(x, y), defn.subs(x, y), global)
    val valueOpt: Option[Term] = Some(defn.value)

    val constants: Vector[Term] = init.constants

    val variables: Vector[Term] = init.variables

    val exportVariables: Vector[Term] = init.exportVariables

    val terms: Vector[Term] = init.terms

    val definitions: Vector[Defn[Term]] =
      if (global) init.definitions :+ defn.map(init.export)
      else init.definitions

    val inductiveDefns: Vector[ExstInducStrucs] = init.inductiveDefns

    val statements: Vector[String] = init.statements :+ s"Let ${defn.name} : ${defn.name.typ} = ${defn.value}"

    def export(t: Term): Term = init.export(t.replace(defn.name, defn.value))

    def exportTyp(t: Typ[Term]): Typ[Term] =
      init.exportTyp(t.replace(defn.name, defn.value))

    def exportStrict(t: Term): Term =
      init.exportStrict(t.replace(defn.name, defn.value))

    def exportTypStrict(typ: Typ[Term]): Typ[Term] =
      init.exportTypStrict(typ.replace(defn.name, defn.value))

    def importOpt(t: Term): Option[Term] = Some(t)

    def importTypOpt(t: Typ[Term]): Option[Typ[Term]] = Some(t)
  }

  case class AppendConstant[U <: Term with Subs[U]](init: Context, constant: U)
      extends Context {
    def subs(x: Term, y: Term) =
      AppendConstant(init.subs(x, y), constant.subs(x, y))

    val valueOpt: Option[Term] = Some(constant)

    val constants: Vector[Term] = init.constants :+ init.export(constant)

    val variables: Vector[Term] = init.variables

    val exportVariables: Vector[Term] = init.exportVariables

    val terms: Vector[Term] = init.terms

    val definitions: Vector[Defn[Term]] = init.definitions

    val inductiveDefns: Vector[ExstInducStrucs] = init.inductiveDefns

    val statements: Vector[String] = init.statements :+ s"Const ${constant} : ${constant.typ}"

    def export(t: Term): Term = init.export(t)

    def exportTyp(t: Typ[Term]): Typ[Term] = init.exportTyp(t)

    def exportStrict(t: Term): Term = init.exportStrict(t)

    def exportTypStrict(typ: Typ[Term]): Typ[Term] = init.exportTypStrict(typ)

    def importOpt(t: Term): Option[Term] = init.importOpt(t)

    def importTypOpt(t: Typ[Term]): Option[Typ[Term]] = init.importTypOpt(t)
  }

  case class AppendIndDef(init: Context, defn: ExstInducStrucs)
      extends Context {
    def subs(x: Term, y: Term) = AppendIndDef(init.subs(x, y), defn.subs(x, y))

    val valueOpt: Option[Term] = None

    val constants: Vector[Term] = init.constants ++ defn.constants

    val variables: Vector[Term] = init.variables

    val exportVariables: Vector[Term] = init.exportVariables

    val terms: Vector[Term] = init.terms

    val definitions: Vector[Defn[Term]] = init.definitions

    val inductiveDefns: Vector[ExstInducStrucs] = init.inductiveDefns :+ defn

    val statements: Vector[String] = init.statements :+ s"Inductive $defn"

    def export(t: Term): Term = init.export(t)

    def exportTyp(t: Typ[Term]): Typ[Term] = init.exportTyp(t)

    def exportStrict(t: Term): Term = init.exportStrict(t)

    def exportTypStrict(typ: Typ[Term]): Typ[Term] = init.exportTypStrict(typ)

    def importOpt(t: Term): Option[Term] = init.importOpt(t)

    def importTypOpt(t: Typ[Term]): Option[Typ[Term]] = init.importTypOpt(t)
  }

  sealed trait Role

  case object Assert extends Role

  case object Consider extends Role

  case class AppendTerm[U <: Term with Subs[U]](
      init: Context,
      term: U,
      role: Role
  ) extends Context {
    def subs(x: Term, y: Term) =
      AppendTerm(init.subs(x, y), term.replace(x, y), role)

    val valueOpt: Option[Term] = Some(term)

    val constants: Vector[Term] = init.constants

    val variables: Vector[Term] = init.variables

    val exportVariables: Vector[Term] = init.exportVariables

    val terms: Vector[Term] = init.terms :+ init.export(term)

    val definitions: Vector[Defn[Term]] = init.definitions

    val inductiveDefns: Vector[ExstInducStrucs] = init.inductiveDefns

    val statements: Vector[String] = init.statements :+ s"$role $term : ${term.typ}"

    def export(t: Term): Term = init.export(t)

    def exportTyp(t: Typ[Term]): Typ[Term] = init.exportTyp(t)

    def exportStrict(t: Term): Term = init.exportStrict(t)

    def exportTypStrict(typ: Typ[Term]): Typ[Term] = init.exportTypStrict(typ)

    def importOpt(t: Term): Option[Term] = init.importOpt(t)

    def importTypOpt(t: Typ[Term]): Option[Typ[Term]] = init.importTypOpt(t)
  }

  case class AppendVariable[U <: Term with Subs[U]](init: Context, variable: U)
      extends Context {
    def subs(x: Term, y: Term) =
      AppendVariable(init.subs(x, y), variable.replace(x, y))

    val valueOpt: Option[Term] = Some(variable)

    val constants: Vector[Term] = init.constants

    val variables: Vector[Term] = init.variables :+ variable

    val exportVariables: Vector[Term] = init.exportVariables :+ init.export(variable)


    val terms: Vector[Term] = init.terms

    val definitions: Vector[Defn[Term]] = init.definitions

    val inductiveDefns: Vector[ExstInducStrucs] = init.inductiveDefns

    val statements: Vector[String] = init.statements :+ s"Given $variable : ${variable.typ}"

    def export(t: Term): Term =
      init.export(if (t.dependsOn(variable)) variable :~> t else t)

    def exportTyp(t: Typ[Term]): Typ[Term] =
      init.exportTyp(if (t.dependsOn(variable)) variable ~>: t else t)

    def exportStrict(t: Term): Term = init.exportStrict(variable :~> t)

    def exportTypStrict(typ: Typ[Term]): Typ[Term] =
      init.exportTypStrict(variable ~>: typ)

    def importOpt(t: Term): Option[Term] = 
      init.importOpt(t).flatMap{
        case lm: LambdaLike[u, v] if lm.dom == variable.typ => Some(lm.value.replace(lm.variable, variable))
        case _ => None
      }

    def importTypOpt(t: Typ[Term]): Option[Typ[Term]] = 
      init.importTypOpt(t).flatMap{
        case pd: PiDefn[u, v] if pd.domain == variable.typ => Some(pd.value.replace(pd.variable, variable))
        case _ => None
      }
  }

  def consider(ts: Term*): Context = Empty.consider(ts: _*)

  def apply(kvs: (String, Term)*): Context = Empty(kvs: _*)

}

sealed trait Context {
  import Context._

  def subs(x: Term, y: Term): Context

  val constants: Vector[Term]

  val variables: Vector[Term]

  val exportVariables: Vector[Term]

  val terms: Vector[Term]

  val definitions: Vector[Context.Defn[Term]]

  val inductiveDefns: Vector[ExstInducStrucs]

  val statements: Vector[String]

  override def toString(): String = statements.mkString("{", "; ","}")

  lazy val inducStruct: ExstInducStrucs =
    inductiveDefns.reverse
      .foldRight[ExstInducStrucs](ExstInducStrucs.Base)(_ || _)

  def export(t: Term): Term

  def exportTyp(typ: Typ[Term]): Typ[Term]

  def exportStrict(t: Term): Term

  def exportTypStrict(typ: Typ[Term]): Typ[Term]

  def importOpt(t: Term): Option[Term]

  def importTypOpt(t: Typ[Term]): Option[Typ[Term]]

  def define[U <: Term with Subs[U]](name: Term, value: U) =
    AppendDefn(this, Defn(name, value), global = true)

  def defineSym[U <: Term with Subs[U]](name: AnySym, value: U) =
    AppendDefn(this, Defn(value.typ.variable(name), value), global = true)

  def defineInduc(ind: ExstInducStrucs) = AppendIndDef(this, ind)

  def let[U <: Term with Subs[U]](name: Term, value: U) =
    AppendDefn(this, Defn(name, value), global = false)

  def addConstant[U <: Term with Subs[U]](const: U) =
    AppendConstant(this, const)

  def addVariable[U <: Term with Subs[U]](variable: U) =
    AppendVariable(this, variable)

  def assume(tp: Typ[Term], text: String = "assumption"): AppendConstant[Term] =
    addConstant(text :: tp)

  def assert(tp: Typ[Term]): AppendTerm[Typ[Term]] = introduce(tp, Assert)

  def given[U <: Term with Subs[U]](v: Term): Context = addVariable(v)

  def introduce[U <: Term with Subs[U]](
      t: U,
      role: Role = Consider
  ): AppendTerm[U] =
    AppendTerm(this, t, role)

  def consider(ts: Term*): Context =
    ts.foldLeft(this) { case (ctx, t) => ctx.introduce(t) }

  def apply(kvs: (String, Term)*): Context =
    kvs.foldLeft(this) { case (ctx, (k, v)) => ctx.defineSym(Name(k), v) }

  val valueOpt: Option[Term]

  lazy val namedTerms: Map[String, Term] =
    (definitions.collect {
      case Defn(NamedTerm(name), value) => (name -> value)
    } ++
      constants.collect {
        case const @ NamedTerm(name) => name -> const
      }).toMap

  def ++(that: Context): Context = that match {
    case Context.Empty => this
    case AppendDefn(init, defn: Defn[u], global) =>
      AppendDefn[u](++(init), defn, global)
    case ap: AppendConstant[u]    => AppendConstant[u](++(ap.init), ap.constant)
    case AppendIndDef(init, defn) => AppendIndDef(++(init), defn)
    case ap: AppendTerm[u]        => AppendTerm(++(ap.init), ap.term, ap.role)
    case ap: AppendVariable[u]    => AppendVariable(++(ap.init), ap.variable)
  }
}
