package provingground
import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context
import HoTT._

/**
 * @author gadgil
 */
object DSL {
  def self(n : Int) : Int = macro selfImpl
  
  def selfImpl(c : Context)(n : c.Expr[Int]) : c.Expr[Int] = {
    import c.universe._
    val q"${m : Int}" = n.tree
    c.Expr[Int] (q"$m")
  }
  /*
  implicit class MacroTyp(typ: Typ[Term]){
    def ::(term : Term): Term = macro checkImpl
  }
  */
  
  
  def checkTyp(term: Term, typ: Typ[Term]): Term = macro checkImpl
  
  def checkImpl(c: Context)(term: c.Expr[Term], typ: c.Expr[Typ[Term]]) : c.Expr[Term] ={
    import c.universe._
    val t = c.eval(term) !: c.eval(typ)
    c.Expr[Term](reify(t).tree)
  }
  /*
  def castImpl(c: Context)(trm: c.Expr[Term])(tp : c.Expr[Typ[Term]]) : c.Expr[Term] = {
    import c.universe._
    val q"${term: Term}" = trm.tree
    val q"${typ: Typ[Term]}" = tp.tree
      assert(term.typ == this," Expected "+typ.toString+"but found "+term.typ.toString)
      q"${term}"
  }
  */
}