package provingground
import HoTT._
import Families._
import math._
//import ScalaUniverses._
import scala.util._
import scala.language.existentials

import RecFunction._

case class RecFn[C<: Term with Subs[C]](W: Typ[Term], X: Typ[C]) extends AnySym

/**
 * rec(W)(X) is the value, defined recursively.
 * @tparam C codomain (scala) type
 * @tparam F full type of rec
 */
trait RecFunction[C<: Term with Subs[C]]{self =>
  /**
   * W in rec(W)(X)
   */
  val W: Typ[Term]

  /**
   * X in rec(W)(X)
   */
//   val X : Typ[C]


  /**
   * (scala) type of rec(W)(X)
   */
  type FullType <: Term with Subs[FullType]

  def fullTyp(x : Typ[C]): Typ[FullType]

  /**
   * induced change to function of the type of rec(W)(X) given change on function W->X;
   * @param transform function W -> X by which we change functions W -> X, trying the case first.
   * @return induced changed function.
   */
  def pullback(X: Typ[C])(transform: Func[Term, C] => Func[Term, C]): FullType => FullType

  /**
   * given value for rec(W)(X) corresponding to earlier patterns, returns one including the new case.
   */
  def recursion(X: Typ[C])(f: => FullType): FullType

  /**
   * prepend a constructor
   */
   def prepend[U <: Term with Subs[U]](cons: Constructor[C, U]) = {
    val recdom = (x: Typ[C]) => cons.pattern.recDom(cons.W, x)
    type D = cons.pattern.RecDataType
    val caseFn : D => Func[Term, C] => Func[Term, C] =
       (d) => (f) => cons.pattern.recModify(cons.cons)(d)(f)
    RecFunctionCons[D, C](recdom, caseFn, this)
  }

  def fn(x: Typ[C]) : FullType = recursion(x)(fn(x))

}

object RecFunction{
def recFunction[C <: Term with Subs[C], U <: Term with Subs[U]](conss: List[Constructor[C, U]], W: Typ[Term]) = {
  val init : RecFunction[C] = RecTail[C](W)
  (init /: conss)(_ prepend _)
}

def recFn[C <: Term with Subs[C], U <: Term with Subs[U]](conss: List[Constructor[C, U]], W: Typ[Term], X: Typ[C]) =
  recFunction(conss, W).fn(X)

}
/*
case class RecProxy[C <: Term](W: Typ[Term], X : Typ[C]) extends AnySym{
  override def toString = s"rec($W)($X)"
}
*/

/**
 * container for rec(W)(X) in the case of no constructors.
 * rec(W)(X) is defined to be formal application of itself.
 * Lazy lambda is used to avoid infinite loops
 */
case class RecTail[C <: Term with Subs[C]](W: Typ[Term]) extends RecFunction[C]{
  type FullType = Func[Term, C]

  def fullTyp(x: Typ[C]) : Typ[FullType] = W ->: x

  private lazy val a = W.Var

  def recursion(X: Typ[C])(f: => FullType) = new LazyLambdaFixed(a, X.symbObj(ApplnSym(f, a)))

  def pullback(X: Typ[C])(transform: Func[Term, C] => Func[Term, C]) = (g : Func[Term, C]) => transform(g)
}

/**
 * cons for recursion function, i.e., adding a new constructor
 * @param dom domain
 * @param caseFn given (previous?) rec(W)(X) and function in domain (to be applied to value) matches pattern
 * @param tail previously added constructors
 */
case class RecFunctionCons[D<: Term with Subs[D], C <: Term with Subs[C]](
    recdom: Typ[C] => Typ[D],
    caseFn : D => Func[Term, C] => Func[Term, C],
    tail: RecFunction[C]) extends RecFunction[C]{
  val W = tail.W

//  val X = tail.X

  type FullType = Func[D, tail.FullType]

  def fullTyp(x: Typ[C]) : Typ[FullType] = FuncTyp(recdom(x), tail.fullTyp(x))


  def pullback(X: Typ[C])(transform: Func[Term, C] => Func[Term, C]) = (g) =>
    {
     val a = recdom(X).Var
    new LazyLambdaFixed(a, tail.pullback(X)(transform)(g(a)))
    }

  def recursion(X: Typ[C])(f: => FullType) ={
    val a = recdom(X).Var
    def fn(x: D) = tail.pullback(X)(caseFn(x))(tail.recursion(X)(f(x)))
    new LazyLambdaFixed(a, fn(a))
  }
}
