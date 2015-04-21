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
    
    val inp = term
    
    val tterm = c.Expr[Term](c.untypecheck(term.tree.duplicate))
    
    val ttyp = c.Expr[Typ[Term]](c.untypecheck(typ.tree.duplicate))
    
    val trm = c.eval(tterm) 
    
    val tp = c.eval(ttyp)
    
    println(trm, tp)
    
    println("check")
    
    println(trm !: tp)
    
    println(showRaw(c.Expr[Term](reify(trm !: tp).tree)))
    
    assert(trm.typ == tp, "type mismatch in HoTT")
    
    inp
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