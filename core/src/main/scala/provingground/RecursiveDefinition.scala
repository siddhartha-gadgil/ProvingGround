package provingground

import HoTT._
import Families._
import math._
//import ScalaUniverses._
import scala.util._
import scala.language.existentials

import scala.language.implicitConversions

/**
 * @author gadgil
 * A recursive function definition, i.e., rec_{W,X}(d1)(d2)...
 */
trait RecursiveDefinition[C<: Term with Subs[C]] {self =>
    /**
   * W in rec(W)(X)
   */
  val W: Typ[Term]

  /**
   * X in rec(W)(X)
   */
   val X : Typ[C]
  
  /**
   * recursive definition, with offspring applying f.
   */
  def recursion(f : => Func[Term, C]) : Func[Term, C]
  
  /**
   * the function to use, applying itself to recursion
   */ 
  def func: Func[Term, C] = recursion(func)
  
  def prependPair(cons: Constructor[C])(arg: cons.pattern.RecDataType) : RecursiveDefinition[C] = {
    type D = cons.pattern.RecDataType
    
    val caseFn : D => Func[Term, C] => Func[Term, C] => Func[Term, C] =
         (d) => (f) => (g) => cons.pattern.recModify(cons.cons)(d)(f)(g)
    
    RecDefinitionCons(arg, caseFn, self)
  }
  
  import RecursiveDefinition._
  
  def prepend(cons: Constructor[C]) = {
        type D = cons.pattern.RecDataType
        val dom = cons.pattern.recDom(W, X)
  }
}

/**
 * recursive definition with empty constructor, hence empty data
 */
case class RecDefinitionTail[C<: Term with Subs[C]](W: Typ[Term], X: Typ[C]) extends RecursiveDefinition[C]{
  def recursion(f : => Func[Term, C]) = 
    new FuncDefn((a: Term) => X.symbObj(ApplnSym(f, a)), W, X)
}

case class RecDefinitionCons[D<: Term with Subs[D], C <: Term with Subs[C]](
    arg: D,
    caseFn : D => Func[Term, C] => Func[Term, C] => Func[Term, C],
    tail: RecursiveDefinition[C]) extends RecursiveDefinition[C]{
  
  lazy val W = tail.W
  
  lazy val X = tail.X
  
  def recursion(f: => Func[Term, C]) = {
    def fn(x: Term) = caseFn(arg)(f)(tail.recursion(f))(x)
    new FuncDefn(fn, W, X)
  }
  
}

object RecursiveDefinition{
  case class RecFunc[C <: Term with Subs[C]](defn: RecursiveDefinition[C]) extends FuncDefn(defn.func, defn.W, defn.X)
  
  /*
  def recFn[C <: Term with Subs[C]](W: Typ[Term], X: Typ[C]) : List[Constructor[C]] => Term = {
    case List() => RecDefinitionTail(W, X).func
    case x :: ys =>
      {
        type D = x.pattern.RecDataType
        val dom = x.pattern.recDom(W, X)
        val tail = recFn(W, X)(ys)
        val caseFn : D => Func[Term, C] => Func[Term, C] => Func[Term, C] =
         (d) => (f) => (g) => x.pattern.recModify(x.cons)(d)(f)(g)
        new FuncDefn((arg: D) => RecDefinitionCons(arg, caseFn, tail).func, dom, X)
        recFn(W, X)(ys)
      }
       
      
  }*/
}