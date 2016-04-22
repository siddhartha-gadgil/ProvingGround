package provingground

import HoTT._
//import Families._
import math.max
//import ScalaUniverses._
//import scala.util._
import scala.language.existentials

import scala.language.implicitConversions

/**
 * @author gadgil
 * A recursive function definition, i.e., rec_{W,X}(d1)(d2)...
 */
trait RecursiveDefinition[C<: Term with Subs[C],  H<: Term with Subs[H]] {self =>
    /**
   * W in rec(W)(X)
   */
  val W: Typ[H]

  /**
   * X in rec(W)(X)
   */
   val X : Typ[C]

  /**
   * recursive definition, with offspring applying f.
   */
  def recursion(f : => Func[H, C]) : Func[H, C]

  /**
   * the function to use, applying itself to recursion
   */
  def func: Func[H, C] = recursion(func)

  def prependPair(cons: Constructor[C, H])(arg: cons.pattern.RecDataType) : RecursiveDefinition[C, H] = {
    type D = cons.pattern.RecDataType

    val caseFn : D => Func[H, C] => Func[H, C] => Func[H, C] =
         (d) => (f) => (g) => cons.pattern.recModify(cons.cons)(d)(f)(g)

    RecDefinitionCons(arg, caseFn, self)
  }

//  import RecursiveDefinition.{recFn}

  def prepend(cons: Constructor[C, H], sym: AnySym) =
    prependPair(cons)(cons.pattern.recDom(W, X).symbObj(sym))
}

/**
 * recursive definition with empty constructor, hence empty data
 */
case class RecDefinitionTail[C<: Term with Subs[C],  H<: Term with Subs[H]](
    W: Typ[H], X: Typ[C]) extends RecursiveDefinition[C, H]{
  def recursion(f : => Func[H, C]) : Func[H, C] =
    new Func[H, C]{
      def newobj = this
      
      def subs(x: Term, y: Term) = this
      
      val dom = W
      
      val codom = X
      
      val typ = W ->: X
      
      def act(w: H) = X.symbObj(ApplnSym(f, w))
  }
}

case class RecDefinitionCons[D<: Term with Subs[D], C <: Term with Subs[C],  H<: Term with Subs[H]](
    arg: D,
    caseFn : D => Func[H, C] => Func[H, C] => Func[H, C],
    tail: RecursiveDefinition[C, H]) extends RecursiveDefinition[C, H]{

  lazy val W = tail.W

  lazy val X = tail.X

  def recursion(f: => Func[H, C]) = {
    def fn(x: H) = caseFn(arg)(f)(tail.recursion(f))(x)
    new FuncDefn(fn, W, X)
  }

}

object RecursiveDefinition{



  def recFn[C <: Term with Subs[C],  H<: Term with Subs[H]](
      conss: List[Constructor[C, H]], W: Typ[H], X: Typ[C]) = {
    val namedConss = for (c <- conss) yield (c, NameFactory.get)

    def addCons( cn :(Constructor[C, H], String), defn : RecursiveDefinition[C, H]) =
      defn.prepend(cn._1, cn._2)

    val init : RecursiveDefinition[C, H] = RecDefinitionTail(W, X)
    val lambdaValue : Term = (namedConss :\ init)(addCons).func

    val variables : List[Term] = for ((c, name) <- namedConss) yield c.pattern.recDom(W, X).symbObj(name)

    (variables :\ lambdaValue)(lmbda(_)(_))
  }



}

trait RecursiveCaseDefinition[H<: Term with Subs[H], C <: Term with Subs[C]] extends Func[H, C]{self =>
  def caseFn(f : => Func[H, C])(arg: H) : Option[C]

  def act(arg: H) = {
    caseFn(self)(arg) getOrElse codom.symbObj(ApplnSym(self, arg))
  }

  def subs(x: Term, y: Term) : RecursiveCaseDefinition[H, C]
}

object RecursiveCaseDefinition{
  case class Empty[H<: Term with Subs[H], C<: Term with Subs[C]](
    dom: Typ[H], codom: Typ[C]) extends RecursiveCaseDefinition[H,C]{
      val typ = dom ->: codom

      def subs(x: Term, y: Term) = Empty(dom.replace(x, y), codom.replace(x, y))

      def newobj = Empty(dom.newobj, codom.newobj)

      def caseFn(f : => Func[H, C])(arg: H) : Option[C] = None
    }

  case class DataCons[
    H  <: Term with  Subs[H],
    C<: Term with Subs[C],
    D <: Term with Subs[D]](
      data: D,
      defn: D => Func[H, C] => H => Option[C],
    tail: RecursiveCaseDefinition[H, C]) extends RecursiveCaseDefinition[H, C]{
      val dom = tail.dom

      val codom = tail.codom

      val typ  = dom ->: codom

      def newobj = DataCons(data.newobj, defn, tail)

      def subs(x: Term, y: Term) =
        DataCons(data.replace(x, y), defn, tail.subs(x, y))


      def caseFn(f : => Func[H, C])(arg: H) : Option[C] =
        defn(data)(f)(arg) orElse(tail.caseFn(f)(arg))
    }

}
