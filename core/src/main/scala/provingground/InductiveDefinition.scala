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
trait InductiveDefinition[C<: Term with Subs[C]] {self =>
    /**
   * W in ind(W)(X)
   */
  val W: Typ[Term]

  /**
   * Xs in ind(W)(Xs)
   */
   val Xs : Func[Term, Typ[C]]
  
  /**
   * recursive definition, with offspring applying f.
   */
  def induction(f : => FuncLike[Term, C]) : FuncLike[Term, C]
  
  /**
   * the function to use, applying itself to recursion
   */ 
  def func: FuncLike[Term, C] = induction(func)
  
  def prependPair(cons: Constructor[C])(arg: cons.pattern.InducDataType) : InductiveDefinition[C] = {
    type D = cons.pattern.InducDataType
    
    val caseFn : D => FuncLike[Term, C] => FuncLike[Term, C] => FuncLike[Term, C] =
         (d) => (f) => (g) => cons.pattern.inducModify(cons.cons)(d)(f)(g)
    
    InducDefinitionCons(arg, caseFn, self)
  }
  
  import InductiveDefinition._
  
  def prepend(cons: Constructor[C], sym: AnySym) = 
    prependPair(cons)(cons.pattern.inducDom(W, Xs)(cons.cons).symbObj(sym))
}

/**
 * recursive definition with empty constructor, hence empty data
 */
case class InducDefinitionTail[C<: Term with Subs[C]](W: Typ[Term], Xs: Func[Term, Typ[C]]) extends InductiveDefinition[C]{
  def induction(f : => FuncLike[Term, C]) = 
    new DepFuncDefn((a: Term) => Xs(a).symbObj(ApplnSym(f, a)), W, Xs)
}

case class InducDefinitionCons[D<: Term with Subs[D], C <: Term with Subs[C]](
    arg: D,
    caseFn : D => FuncLike[Term, C] => FuncLike[Term, C] => FuncLike[Term, C],
    tail: InductiveDefinition[C]) extends InductiveDefinition[C]{
  
  lazy val W = tail.W
  
  lazy val Xs = tail.Xs
  
  def induction(f: => FuncLike[Term, C]) = {
    def fn(x: Term) = caseFn(arg)(f)(tail.induction(f))(x)
    new DepFuncDefn(fn, W, Xs)
  }
  
}

object InductiveDefinition{
  
  
  
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
