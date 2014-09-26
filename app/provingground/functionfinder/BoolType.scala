package provingground.functionfinder
import provingground.HoTT._
import ScalaRep._
import scala.reflect.runtime.universe.{Try => UnivTry, Function => FunctionUniv, _}

object BoolType {
  case object Bool extends SmallTyp
  
  val boolrep = dsl.i[Boolean](Bool)
  
  private val b = boolrep
  
  val not = {
    val rep = b -->: b
    rep((x: Boolean) => !x)
  }
  
  private val binrep = b -->: b -->: b
  
  val and = binrep((x: Boolean) => (y: Boolean) => x && y)
  
  val or = binrep((x: Boolean) => (y: Boolean) => x || y)
  
  case class isTrueTyp(value: Boolean) extends SmallTyp
  
  case object tt extends ConstTerm[Boolean]{
    val value = true
    
    val typ = isTrueTyp(true)
    
    override def toString = "true"
  }
  
  val boolFmly = b -->: __
  
  val isTrue = boolFmly((x: Boolean) => isTrueTyp(x))
  
  case object Fail extends SmallTyp
  
  case object failure extends ConstTerm[Unit]{
    val value = ()
    
    val typ = Fail
  }
  
  def iteFunc[U <: Term : TypeTag](u: Typ[U]) = {
    val rep = b -->: u -->: u -->: u
    rep((cond: Boolean) => (yes: U) => (no : U) => if (cond) yes else no)
  }
  
  val ite = depFunc(__, iteFunc[Term])
  
  private type FnFn = FuncObj[Term, FuncObj[Term, Term]]
  
  
  def iteDepFunc(u: Typ[Term], v : Typ[Term])(implicit fnfn : ScalaUniv[FnFn]) = {
    val t = u ->: v ->: u
    def restyp = (c: Boolean) => if (c) (u ->: v ->: u) else (u ->: v ->: v)
//    val uni = MiniVerse(u ->: v ->: u)
    val uni = fnfn.univ
    val typrep = b -->: uni
    
  //  type FnFn = FuncObj[Term, FuncObj[Term, Term]]
    
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
  
  val itedep = depFunc(__, (u: Typ[Term]) => depFunc(__, (v: Typ[Term]) => iteDepFunc(u, v)))
  
  val check = "check" :: Bool
  
  val verify = lambda(check)(itedep(isTrueTyp(true))(Fail)(check)(tt)(failure))
}