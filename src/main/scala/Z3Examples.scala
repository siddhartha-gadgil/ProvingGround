import com.microsoft.z3._
import scala.collection.JavaConversions._

object Z3Examples{
  
  /** Injectivity of f(x_0, x_2, ..., x_{n-1}); 
   *  checking two points with different i-th coordinate do not map to the same point;
   *  equivalently, we can map the image to the ith co-ordinate  
   * formula (Boolean Expression) for this in terms of one-sided inverse
   */
  def InjAxiom(ctx: Context, f: FuncDecl, i: Int): Option[BoolExpr] ={
    val domain = f.Domain /* An array giving the  domains of the co-ordinates*/
    val sz = f.DomainSize /* Number of co-ordinates */
    
    if (i >= sz) {
      println("Not enough co-ordinates")
      None
      } 
    else {
    /** declare the i-th inverse of f: finv */
    val finvDomain = f.Range
    val finvRange = domain(i)
    val finv = ctx.MkFuncDecl("f_fresh", finvDomain, finvRange)
    
    val types = domain take sz /* Should be just domain */
    val names : Array[Symbol] = (for (j <- 0 until sz) yield (ctx.MkSymbol("x_"+ j.toString))).toArray
    val xs = (for (j <- 0 until sz) yield (ctx.MkBound(j, types(j)))).toArray /** Variables x_i */
    
    val xi= xs(i)
    
    /* create f(x_0, ..., x_i, ..., x_{n-1}) */
    val fxs = f.Apply(xs)
    
    /* create f_inv(f(x_0, ..., x_i, ..., x_{n-1})) */
    val finvfxs = finv.Apply(fxs)
    
    /* create finv(f(x_0, ..., x_i, ..., x_{n-1})) = x_i */
    val eq = ctx.MkEq(finvfxs, xi)
    
    /* use f(x_0, ..., x_i, ..., x_{n-1}) as the pattern for the quantifier */
    val p = ctx.MkPattern(Array(fxs))
    
    /* create & assert quantifier */
    val q : BoolExpr = ctx.MkForall(types, names, eq, 1, Array(p), null, null, null)
    
    Some(q)
    }
  }
  
  def SimpleExample{
    println("Simple Example")
    
    val ctx= new Context
    ctx.Dispose()
   
  }
  
  def Check(ctx: Context, f: BoolExpr, sat: Status) = {
    val s = ctx.MkSolver
    s.Assert(f)
    if (s.Check != sat) None
    else {
      if (sat == Status.SATISFIABLE) Some(s.Model) else null
    }
  }
  
  def Prove(ctx: Context, f: BoolExpr, useMBQI : Boolean, assumptions: Array[BoolExpr]) ={
    val s = ctx.MkSolver
    val p = ctx.MkParams
    p.Add("mbqi", useMBQI)
    s.setParameters(p)
    for (a <- assumptions) s.Assert(a)
    s.Check    
  }
  
}
