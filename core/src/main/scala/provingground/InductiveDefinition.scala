package provingground

import HoTT._
//import Families._
import math._
//import ScalaUniverses._
import scala.util._
import scala.language.existentials

import scala.language.implicitConversions

/**
  * @author gadgil
  * A recursive function definition, i.e., rec_{W,X}(d1)(d2)...
  */
@deprecated(
    "Use inducDef instead, with formal application outside for concrete method",
    "April 26, 2016")
trait InductiveDefinition[C <: Term with Subs[C], H <: Term with Subs[H]] {
  self =>

  /**
    * W in ind(W)(X)
    */
  val W: Typ[H]

  /**
    * Xs in ind(W)(Xs)
    */
  val Xs: Func[H, Typ[C]]

  /**
    * recursive definition, with offspring applying f.
    */
  def induction(f: => FuncLike[H, C]): FuncLike[H, C]

  /**
    * the function to use, applying itself to recursion
    */
  def func: FuncLike[H, C] = induction(func)

  def prependPair(cons: Constructor[C, H])(
      arg: cons.pattern.InducDataType): InductiveDefinition[C, H] = {
    type D = cons.pattern.InducDataType

    val caseFn: D => FuncLike[H, C] => FuncLike[H, C] => FuncLike[H, C] =
      (d) => (f) => (g) => cons.pattern.inducModify(cons.cons)(d)(f)(g)

    InducDefinitionCons(arg, caseFn, self)
  }

  import InductiveDefinition._

  def prepend(cons: Constructor[C, H], sym: AnySym) =
    prependPair(cons)(cons.pattern.inducDataTyp(W, Xs)(cons.cons).symbObj(sym))
}

/**
  * recursive definition with empty constructor, hence empty data
  */
@deprecated(
    "Use inducDef instead, with formal application outside for concrete method",
    "April 26, 2016")
case class InducDefinitionTail[C <: Term with Subs[C], H <: Term with Subs[H]](
    W: Typ[H], Xs: Func[H, Typ[C]])
    extends InductiveDefinition[C, H] {
  def induction(f: => FuncLike[H, C]) =
    new DepFuncDefn((a: H) => Xs(a).symbObj(ApplnSym(f, a)), W, Xs)
}

@deprecated(
    "Use inducDef instead, with formal application outside for concrete method",
    "April 26, 2016")
case class InducDefinitionCons[
    D <: Term with Subs[D], C <: Term with Subs[C], H <: Term with Subs[H]](
    arg: D,
    caseFn: D => FuncLike[H, C] => FuncLike[H, C] => FuncLike[H, C],
    tail: InductiveDefinition[C, H])
    extends InductiveDefinition[C, H] {

  lazy val W = tail.W

  lazy val Xs = tail.Xs

  def induction(f: => FuncLike[H, C]) = {
    def fn(x: H) = caseFn(arg)(f)(tail.induction(f))(x)
    new DepFuncDefn(fn, W, Xs)
  }
}

object InductiveDefinition {

  @deprecated(
      "Use inducDef instead, with formal application outside for concrete method",
      "April 26, 2016")
  def inducFn[C <: Term with Subs[C], H <: Term with Subs[H]](
      conss: List[Constructor[C, H]], W: Typ[H], Xs: Func[H, Typ[C]]) = {
    val namedConss = for (c <- conss) yield (c, NameFactory.get)

    def addCons(
        cn: (Constructor[C, H], String), defn: InductiveDefinition[C, H]) =
      defn.prepend(cn._1, cn._2)

    val init: InductiveDefinition[C, H] = InducDefinitionTail(W, Xs)
    val lambdaValue: Term = (namedConss :\ init)(addCons).func

    val variables: List[Term] = for ((c, name) <- namedConss) yield
      c.pattern.inducDataTyp(W, Xs)(c.cons).symbObj(name)

    (variables :\ lambdaValue)(lmbda(_)(_))
  }
}
