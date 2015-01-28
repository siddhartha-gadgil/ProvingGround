package provingground.functionfinder

import provingground.HoTT._
import scala.reflect.runtime.universe.{ Try => UnivTry, Function => FunctionUniv, _ }
import ScalaRep._
import BoolType._

object IntTypes {

  trait IntTyp extends SmallTyp {
    lazy val rep = dsl.i[Long](this)

    lazy val oprep = rep -->: rep -->: rep

    lazy val sum = oprep((a: Long) => (b: Long) => a + b)

    lazy val prod = oprep((a: Long) => (b: Long) => a * b)

    lazy val zero = rep(0: Long)

    lazy val binrelrep = rep -->: rep -->: boolrep

    lazy val lt = binrelrep((a: Long) => (b: Long) => (a < b))

    lazy val gt = binrelrep((a: Long) => (b: Long) => (a > b))

    lazy val eql = binrelrep((a: Long) => (b: Long) => (a == b))
  }

  case object N extends IntTyp{
    lazy val one =rep(1: Long)
    
    lazy val succ = sum(one)
  }

  case object Z extends IntTyp

  @annotation.tailrec def inducFn[U <: Term](f0: U, g: Long => U => U, n: Long,
    thenApply: U => U = (u: U) => u): U = {
    if (n > 0) (inducFn(f0, g, n - 1, (u: U) => g(n)(thenApply(u))))
    else { assert(n == 0, "induction at" + n); thenApply(f0) }
  }

  private val A = "A" :: __



  val recN = {
    val init = "a" :: A
    val f = "f" :: (N ->: A ->: A)
    Lambda(A,
      LambdaFixed(init, LambdaFixed(f, {
        val dfn = (n: Long) => inducFn(init, (k: Long) => f(N.rep(k)), n)
        val codrep = N.rep :--> A
        codrep(dfn)
      })))
  }
  
  val inducN = {
    val P = "P" :: N ->: __
    val k = "k" :: N
    val init = "a" :: P(N.zero)
    val steptyp = PiTyp(lmbda(k)(P(k) ->: P(N.succ(k))))
    val step = "step" :: steptyp
    
    lambda(P)(
        lmbda(init)(lmbda(step)({
            val resultrep = N.rep ~~>: P
            val recfn = (m: Long) => (t: Term) => step(N.rep(m))(t)
            val dfn = (n: Long) => inducFn(init, recfn, n)
            resultrep(dfn)
            }
        )))
    
    
  }

  def induccurry[U <: Term]: U => (Long => U => U) => (Long => U) = {
    (f0: U) => g: (Long => U => U) => (n: Long) => inducFn(f0, g, n)
  }

  def recursion[U <: Term with Subs[U]](u: Typ[U]) = {
    val rep = u -->: (n -->: u -->: u) -->: (n -->: u)
    rep(induccurry)
  }

  def induction[U <: Term with Subs[U]](us: FuncObj[Term, Typ[U]])(implicit suu: ScalaUniv[U]) = {
    val stepfmlyrep = (n -->: __)
    val stepfmly = stepfmlyrep((k: Long) => us(n(k)) ->: us(n(k + 1)))
    val steprep = n ~~>: stepfmly
    val stpfm = (k: Long) => us(n(k)) -->: us(n(k + 1))
    val steprp = n ~~>: stpfm
    val rep = us(n(0)) -->: steprp -->: (n ~~>: us)
    rep(induccurry)
  }

  //  val recN = depFunc(__, (u: Typ[Term]) => recursion(u))

  
//  val inducN = depFunc(N ->: __, (us: FuncObj[Term, Typ[Term]]) => induction(us))

  case class Fin(n: Long) extends IntTyp

  val Nfmly = n -->: __

  val FinFn = Nfmly((n: Long) => Fin(n))

  val SimpleFinRep = n ~~>: FinFn

  val finrep = RepSection((n: Long) => (Fin(n)))

  val FinRep = N.rep ~~>: (finrep)

  val NFinRep = n -->: FinRep

  val kmodn = NFinRep((k: Long) => (n: Long) => Fin(n).rep(k % n))

  private val n = N.rep

}