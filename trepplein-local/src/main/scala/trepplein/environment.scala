package trepplein

import scala.concurrent.{ExecutionContext, Future}
import scala.language.implicitConversions
import scala.util.Try

sealed trait Declaration {
  def name: Name
  def univParams: Vector[Level.Param]
  def ty: Expr

  def asAxiom: Axiom = Axiom(name, univParams, ty)
}
final case class Axiom(name: Name,
                       univParams: Vector[Level.Param],
                       ty: Expr,
                       builtin: Boolean = false)
    extends Declaration {
  def check(env: PreEnvironment): Unit = {
    require(!env.declarations.contains(name))
    require(ty.univParams.subsetOf(univParams.toSet))
    require(!ty.hasVars)
    require(!ty.hasLocals)
    val tc = new TypeChecker(env)
    tc.inferUniverseOfType(ty)
  }
}
final case class Definition(name: Name,
                            univParams: Vector[Level.Param],
                            ty: Expr,
                            value: Expr,
                            height: Int = 0)
    extends Declaration {
  def check(env: PreEnvironment): Unit = {
    asAxiom.check(env)
    require(!value.hasVars)
    require(!value.hasLocals)
    val tc = new TypeChecker(env)
    tc.checkType(value, ty)
  }
}

trait CompiledModification {
  def check(): Unit
  def decls: Seq[Declaration]
  def rules: Seq[ReductionRule]
}

trait Modification {
  def name: Name
  def compile(env: PreEnvironment): CompiledModification
}
object Modification {
  implicit def ofAxiom(axiom: Axiom): Modification = AxiomMod(axiom)
}
final case class AxiomMod(ax: Axiom) extends Modification {
  def name: Name = ax.name

  def compile(env: PreEnvironment) = new CompiledModification {
    def check(): Unit             = ax.check(env)
    def decls: Seq[Declaration]   = Seq(ax)
    def rules: Seq[ReductionRule] = Seq()
  }
}
final case class DefMod(defn: Definition) extends Modification {
  def name: Name = defn.name

  val decls = Seq(defn)

  def compile(env: PreEnvironment) = new CompiledModification {
    val height: Int = value.constants.view
      .flatMap(env.get)
      .collect { case d: Definition => d.height }
      .fold(0)(math.max) + 1

    def check(): Unit           = defn.check(env)
    def decls: Seq[Declaration] = Seq(defn.copy(height = height))
    def rules: Seq[ReductionRule] =
      Seq(
        ReductionRule(Vector[Binding](),
                      Const(defn.name, defn.univParams),
                      value,
                      List()))
  }
}

case class EnvironmentUpdateError(mod: Modification, msg: String) {
  override def toString = s"${mod.name}: $msg"
}

sealed class PreEnvironment protected (
    val declarations: Map[Name, Declaration],
    val reductions: ReductionMap,
    val proofObligations: List[Future[Option[EnvironmentUpdateError]]]) {

  def get(name: Name): Option[Declaration] =
    declarations.get(name)
  def apply(name: Name): Declaration =
    declarations(name)

  private def addDeclsFor(mod: CompiledModification): Map[Name, Declaration] =
    declarations ++ mod.decls.view.map(d => d.name -> d)

  def addWithFuture(mod: Modification)(
      implicit executionContext: ExecutionContext)
    : (Future[Option[EnvironmentUpdateError]], PreEnvironment) = {
    val compiled = mod.compile(this)
    val checkingTask = Future {
      Try(compiled.check()).failed.toOption.map(t =>
        EnvironmentUpdateError(mod, t.getMessage))
    }
    checkingTask -> new PreEnvironment(addDeclsFor(compiled),
                                       reductions ++ compiled.rules,
                                       checkingTask :: proofObligations)
  }

  def addNow(mod: Modification): PreEnvironment = {
    val compiled = mod.compile(this)
    compiled.check()
    new PreEnvironment(addDeclsFor(compiled),
                       reductions ++ compiled.rules,
                       proofObligations)
  }

  def add(mod: Modification)(
      implicit executionContext: ExecutionContext): PreEnvironment =
    addWithFuture(mod)._2

  def force(implicit executionContext: ExecutionContext)
    : Future[Either[Seq[EnvironmentUpdateError], Environment]] =
    Environment.force(this)
}

final class Environment private (declarations: Map[Name, Declaration],
                                 reductionMap: ReductionMap)
    extends PreEnvironment(declarations, reductionMap, Nil)
object Environment {
  def force(preEnvironment: PreEnvironment)(
      implicit executionContext: ExecutionContext)
    : Future[Either[Seq[EnvironmentUpdateError], Environment]] =
    Future.sequence(preEnvironment.proofObligations).map(_.flatten).map {
      case Nil =>
        Right(
          new Environment(preEnvironment.declarations,
                          preEnvironment.reductions))
      case exs => Left(exs)
    }

  def default = new Environment(Map(), ReductionMap())
}
