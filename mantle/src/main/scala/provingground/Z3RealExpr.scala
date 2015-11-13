package provingground

import spire.math._
import spire.algebra._

import com.microsoft.z3._


object Z3RealExpr {
  val ctx =  new Context()
  
  import ctx._
  
  val r = mkRealSort
  
  def realVar(name: String) : ArithExpr = mkRealConst(mkSymbol(name))
  
  implicit object Z3RealField extends Field[ArithExpr]{
    def negate(x: ArithExpr): ArithExpr = mkUnaryMinus(x)

    def zero : ArithExpr = mkReal(0, 1)

    def one : ArithExpr = mkReal(1, 1)

    def plus(x: ArithExpr, y: ArithExpr) = mkAdd(x, y)

    def times(x: ArithExpr, y: ArithExpr) = mkMul(x, y)

    def div(x: ArithExpr, y: ArithExpr) = mkDiv(x, y)
    
    def gcd(a: ArithExpr,b: ArithExpr): ArithExpr = one 
    
    def mod(a: ArithExpr,b: ArithExpr): ArithExpr = zero 
    
    def quot(a: ArithExpr,b: ArithExpr): ArithExpr = div(a, b)
    
  }
}