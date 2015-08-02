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
  
  def prepend(cons: Constructor[C], sym: AnySym) = 
    prependPair(cons)(cons.pattern.recDom(W, X).symbObj(sym))
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
  
  
  
  def recFn[C <: Term with Subs[C]](conss: List[Constructor[C]], W: Typ[Term], X: Typ[C]) = {
    val namedConss = for (c <- conss) yield (c, NameFactory.get)
    
    def addCons[C<: Term with Subs[C]]( cn :(Constructor[C], String), defn : RecursiveDefinition[C]) = 
      defn.prepend(cn._1, cn._2)
      
    val init : RecursiveDefinition[C] = RecDefinitionTail(W, X)
    val lambdaValue : Term = (namedConss :\ init)(addCons).func
    
    val variables : List[Term] = for ((c, name) <- namedConss) yield c.pattern.recDom(W, X).symbObj(name)
    
    (variables :\ lambdaValue)(lmbda(_)(_))
  }
       
      
  
}
