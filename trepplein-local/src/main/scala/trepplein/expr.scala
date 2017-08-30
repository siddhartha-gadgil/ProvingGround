package trepplein

import trepplein.Level._

import scala.annotation.tailrec
import scala.collection.mutable

sealed abstract class BinderInfo extends Product {
  def dump = s"BinderInfo.$productPrefix"
}
object BinderInfo {
  case object Default        extends BinderInfo
  case object Implicit       extends BinderInfo
  case object StrictImplicit extends BinderInfo
  case object InstImplicit   extends BinderInfo
}

case class Binding(prettyName: Name, ty: Expr, info: BinderInfo) {
  def abstr(off: Int, lcs: Vector[LocalConst]): Binding =
    copy(ty = ty.abstr(off, lcs))

  def instantiate(off: Int, es: Vector[Expr]): Binding =
    copy(ty = ty.instantiate(off, es))

  def instantiate(subst: Map[Param, Level]): Binding =
    copy(ty = ty.instantiate(subst))
  def instantiateCore(subst: Map[Param, Level]): Binding =
    copy(ty = ty.instantiateCore(subst))

  def dump(implicit lcs: mutable.Map[LocalConst.Name, String]) =
    s"Binding(${prettyName.dump}, ${ty.dump}, ${info.dump})"

  override val hashCode: Int = prettyName.hashCode + 37 * (ty.hashCode + 37 * info.hashCode)
}

sealed abstract class Expr(val varBound: Int, val hasLocals: Boolean)
    extends Product {
  def hasVar(i: Int): Boolean =
    this match {
      case _ if varBound <= i => false
      case Var(idx)           => idx == i
      case App(a, b)          => a.hasVar(i) || b.hasVar(i)
      case Lam(dom, body)     => dom.ty.hasVar(i) || body.hasVar(i + 1)
      case Pi(dom, body)      => dom.ty.hasVar(i) || body.hasVar(i + 1)
      case Let(dom, value, body) =>
        dom.ty.hasVar(i) || value.hasVar(i) || body.hasVar(i + 1)
    }

  def hasVars: Boolean = varBound > 0

  def abstr(lc: LocalConst): Expr = abstr(0, Vector(lc))
  def abstr(off: Int, lcs: Vector[LocalConst]): Expr =
    this match {
      case _ if !hasLocals => this
      case LocalConst(_, name, _) =>
        lcs.indexWhere(_.name == name) match {
          case -1 => this
          case i  => Var(i + off)
        }
      case App(a, b) =>
        App(a.abstr(off, lcs), b.abstr(off, lcs))
      case Lam(domain, body) =>
        Lam(domain.abstr(off, lcs), body.abstr(off + 1, lcs))
      case Pi(domain, body) =>
        Pi(domain.abstr(off, lcs), body.abstr(off + 1, lcs))
      case Let(domain, value, body) =>
        Let(domain.abstr(off, lcs),
            value.abstr(off, lcs),
            body.abstr(off + 1, lcs))
    }

  def instantiate(e: Expr): Expr = instantiate(0, Vector(e))
  def instantiate(off: Int, es: Vector[Expr]): Expr =
    this match {
      case _ if varBound <= off => this
      case Var(idx) =>
        if (off <= idx && idx < off + es.size) es(idx - off) else this
      case App(a, b) => App(a.instantiate(off, es), b.instantiate(off, es))
      case Lam(domain, body) =>
        Lam(domain.instantiate(off, es), body.instantiate(off + 1, es))
      case Pi(domain, body) =>
        Pi(domain.instantiate(off, es), body.instantiate(off + 1, es))
      case Let(domain, value, body) =>
        Let(domain.instantiate(off, es),
            value.instantiate(off, es),
            body.instantiate(off + 1, es))
    }

  def instantiate(subst: Map[Param, Level]): Expr =
    if (subst.forall(x => x._1 == x._2)) this else instantiateCore(subst)
  def instantiateCore(subst: Map[Param, Level]): Expr =
    this match {
      case v: Var              => v
      case Sort(level)         => Sort(level.instantiate(subst))
      case Const(name, levels) => Const(name, levels.map(_.instantiate(subst)))
      case LocalConst(of, name, value) =>
        LocalConst(of.instantiateCore(subst),
                   name,
                   value.map(_.instantiateCore(subst)))
      case App(a, b) => App(a.instantiateCore(subst), b.instantiateCore(subst))
      case Lam(domain, body) =>
        Lam(domain.instantiateCore(subst), body.instantiateCore(subst))
      case Pi(domain, body) =>
        Pi(domain.instantiateCore(subst), body.instantiateCore(subst))
      case Let(domain, value, body) =>
        Let(domain.instantiateCore(subst),
            value.instantiateCore(subst),
            body.instantiateCore(subst))
    }

  def foreach_(f: Expr => Boolean): Unit =
    if (f(this)) this match {
      case App(a, b) =>
        a.foreach_(f)
        b.foreach_(f)
      case Lam(domain, body) =>
        domain.ty.foreach_(f)
        body.foreach_(f)
      case Pi(domain, body) =>
        domain.ty.foreach_(f)
        body.foreach_(f)
      case Let(domain, value, body) =>
        domain.ty.foreach_(f)
        value.foreach_(f)
        body.foreach_(f)
      case _: Var | _: Const | _: Sort | _: LocalConst =>
    }

  def foreach(f: Expr => Unit): Unit =
    foreach_ { x =>
      f(x); true
    }

  private def buildSet[T](f: mutable.Set[T] => Unit): Set[T] = {
    val set = mutable.Set[T]()
    f(set)
    set.toSet
  }

  def univParams: Set[Param] =
    buildSet { ps =>
      foreach {
        case Sort(level)      => ps ++= level.univParams
        case Const(_, levels) => ps ++= levels.view.flatMap(_.univParams)
        case _                =>
      }
    }

  def constants: Set[Name] =
    buildSet { cs =>
      foreach {
        case Const(name, _) => cs += name
        case _              =>
      }
    }

  def -->:(that: Expr): Expr =
    Pi(Binding(Name.Anon, that, BinderInfo.Default), this)

  override def toString: String = pretty(this)

  def dump(implicit lcs: mutable.Map[LocalConst.Name, String] = null): String =
    this match {
      case _ if lcs eq null =>
        val lcs_ = mutable.Map[LocalConst.Name, String]()
        val d    = dump(lcs_)
        if (lcs_.isEmpty) d
        else {
          val decls = lcs.values.map { n =>
            s"val $n = new LocalConst.Name()\n"
          }.mkString
          s"{$decls$d}"
        }
      case Var(i)      => s"Var($i)"
      case Sort(level) => s"Sort(${level.dump})"
      case Const(name, levels) =>
        s"Const(${name.dump}, Vector(${levels.map(_.dump).mkString(", ")}))"
      case App(a, b)      => s"App(${a.dump}, ${b.dump})"
      case Lam(dom, body) => s"Lam(${dom.dump}, ${body.dump})"
      case Pi(dom, body)  => s"Pi(${dom.dump}, ${body.dump})"
      case LocalConst(of, name, value) =>
        val of1 =
          of.prettyName.toString.replace('.', '_').filter { _.isLetterOrDigit }
        val of2 = if (of1.isEmpty || !of1.head.isLetter) s"n$of1" else of1
        val n = lcs.getOrElseUpdate(
          name,
          Stream.from(0).map(i => s"$of2$i").diff(lcs.values.toSeq).head)
        s"LocalConst(${of.dump}, $n, ${value.map(_.dump)})"
      case Let(dom, value, body) =>
        s"Let(${dom.dump}, ${value.dump}, ${body.dump})"
    }
}
case class Var(idx: Int) extends Expr(varBound = idx + 1, hasLocals = false) {
  override def hashCode: Int = idx
}
case class Sort(level: Level) extends Expr(varBound = 0, hasLocals = false) {
  override val hashCode: Int = level.hashCode
}

case class Const(name: Name, levels: Vector[Level])
    extends Expr(varBound = 0, hasLocals = false) {
  override val hashCode: Int = 37 * name.hashCode
}
case class LocalConst(of: Binding,
                      name: LocalConst.Name = new LocalConst.Name,
                      value: Option[Expr] = None)
    extends Expr(varBound = 0, hasLocals = true) {
  override val hashCode: Int = 4 + 37 * (of.hashCode + 37 * value.hashCode) + name.hashCode
}
case class App(a: Expr, b: Expr)
    extends Expr(varBound = math.max(a.varBound, b.varBound),
                 hasLocals = a.hasLocals || b.hasLocals) {
  override val hashCode: Int = a.hashCode + 37 * b.hashCode
}
case class Lam(domain: Binding, body: Expr)
    extends Expr(varBound = math.max(domain.ty.varBound, body.varBound - 1),
                 hasLocals = domain.ty.hasLocals || body.hasLocals) {
  override val hashCode: Int = 1 + 37 * domain.hashCode + body.hashCode
}
case class Pi(domain: Binding, body: Expr)
    extends Expr(varBound = math.max(domain.ty.varBound, body.varBound - 1),
                 hasLocals = domain.ty.hasLocals || body.hasLocals) {
  override val hashCode: Int = 2 + 37 * domain.hashCode + body.hashCode
}
case class Let(domain: Binding, value: Expr, body: Expr)
    extends Expr(
      varBound = math.max(math.max(domain.ty.varBound, value.varBound),
                          body.varBound - 1),
      hasLocals = domain.ty.hasLocals || value.hasLocals || body.hasLocals) {
  override val hashCode: Int = 3 + 37 * (domain.hashCode + 37 * value.hashCode) + body.hashCode
}

object Sort {
  val Prop = Sort(Level.Zero)
}

object LocalConst {
  final class Name {
    override def toString: String = Integer.toHexString(hashCode()).take(4)
  }
}

trait Binder[T] {
  def apply(domain: Binding, body: Expr): T
  def apply(domain: LocalConst, body: Expr): T =
    apply(domain.of, body.abstr(domain))

  trait GenericUnapply {
    def unapply(e: Expr): Option[(Binding, Expr)]
  }
  val generic: GenericUnapply
}

trait Binders[T <: Expr] {
  protected val Single: Binder[T]

  def apply(domains: Iterable[LocalConst])(body: Expr): Expr =
    domains.foldRight(body)(Single.apply)

  def apply(domains: LocalConst*)(body: Expr): Expr =
    apply(domains)(body)

  def unapply(e: Expr): Some[(List[LocalConst], Expr)] =
    e match {
      case Single.generic(dom, expr) =>
        val lc = LocalConst(dom)
        unapply(expr.instantiate(lc)) match {
          case Some((lcs, head)) =>
            Some((lc :: lcs, head))
        }
      case _ => Some((Nil, e))
    }
}

object Lam extends Binder[Lam] {
  val generic: GenericUnapply = new GenericUnapply {
    def unapply(e: Expr) = e match {
      case e: Lam => Lam.unapply(e)
      case _      => None
    }
  }
}
object Lams extends Binders[Lam] {
  protected val Single = Lam
}

object Pi extends Binder[Pi] {
  val generic: GenericUnapply = new GenericUnapply {
    def unapply(e: Expr) = e match {
      case e: Pi => Pi.unapply(e)
      case _     => None
    }
  }
}
object Pis extends Binders[Pi] {
  protected val Single = Pi
}

object Apps {
  @tailrec
  private def decompose(e: Expr, as: List[Expr] = Nil): (Expr, List[Expr]) =
    e match {
      case App(f, a) => decompose(f, a :: as)
      case _         => (e, as)
    }

  def unapply(e: Expr): Some[(Expr, List[Expr])] =
    Some(decompose(e))

  def apply(fn: Expr, as: Iterable[Expr]): Expr =
    as.foldLeft(fn)(App)

  def apply(fn: Expr, as: Expr*): Expr =
    apply(fn, as)
}
