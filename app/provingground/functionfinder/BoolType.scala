package provingground.functionfinder
import provingground.HoTT._
import ScalaRep._
import scala.reflect.runtime.universe.{Try => UnivTry, Function => FunctionUniv, _}

object BoolType {
  case object Bool extends SmallTyp
  
  lazy val boolrep = dsl.i[Boolean](Bool)
  
  private val b = boolrep
  
  lazy val not = {
    val rep = b -->: b
    rep((x: Boolean) => !x)
  }
  
  private val binrep = b -->: b -->: b
  
  lazy val and = binrep((x: Boolean) => (y: Boolean) => x && y)
  
  lazy val or = binrep((x: Boolean) => (y: Boolean) => x || y)
  
  lazy val boolFmly = b -->: __
  
  lazy val isTrue = boolFmly((x: Boolean) => if (x) One else Zero)
  
  

  
  
 
  // Most of the cod below is deprecated.
  case class isTrueTyp(value: Boolean) extends SmallTyp
  
  //  lazy val isTrue = boolFmly((x: Boolean) => isTrueTyp(x))
  
  case object yes extends ConstTerm[Boolean]{
    val value = true
    
    val typ = isTrueTyp(true)
    
    override def toString = "true"
  }
  
    case object notnot extends ConstTerm[Boolean]{
    val value = true
    
    val typ = isTrueTyp(false) ->: Zero
    
    override def toString = "true"
  }
 
  
  
  
  
  def iteFunc[U <: Term : TypeTag](u: Typ[U]) = {
    val rep = b -->: u -->: u -->: u
    rep((cond: Boolean) => (yes: U) => (no : U) => if (cond) yes else no)
  }
  
  lazy val ite = depFunc(__, iteFunc[Term])
  
  private type FnFn = FuncObj[Term, FuncObj[Term, Term]]
  
  // TODO write this in a simpler way, using lambdas or ~~>: reps
  def iteDepFunc(u: Typ[Term], v : Typ[Term])(implicit fnfn : ScalaUniv[FnFn]) = {
    val t = u ->: v ->: u
    def restyp = (c: Boolean) => if (c) (u ->: v ->: u) else (u ->: v ->: v)

    val uni = fnfn.univ
    val typrep = b -->: uni
    
    val typfmly = typrep(restyp)

    val yesrep : ScalaRep[FnFn, FnFn] = IdRep(u ->: v ->: u)
    val norep : ScalaRep[FnFn, FnFn] = IdRep(u ->: v ->: v)
    val ifrep = (c: Boolean) => if (c) yesrep else norep
    
    val yestermrep = (u -->: v -->: u)
    val yes = yestermrep((u: Term) => (v: Term) => u)
    
    val notermrep = (u -->: v -->: u)
    val no = yestermrep((u: Term) => (v: Term) => u)
    
    val rep = DepFuncRep(b, ifrep, typfmly)
    
    rep((c: Boolean) => if (c) yes else no)
  }
  
  lazy val itedep = depFunc(__, (u: Typ[Term]) => depFunc(__, (v: Typ[Term]) => iteDepFunc(u, v)))

}