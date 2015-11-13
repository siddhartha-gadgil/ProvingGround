package provingground

import z3.scala._

import spire.math._
import spire.algebra._

object Z3RealsAST {
  val ctx =  new Z3Context(new Z3Config("MODEL" -> true))
  
  import ctx._
  
  val r = mkRealSort
  
  def realVar(name: String) = mkConst(mkStringSymbol(name), r)
  
  implicit object Z3RealField extends Field[Z3AST]{
    def negate(x: Z3AST): Z3AST = mkUnaryMinus(x)

    def zero : Z3AST = mkReal(0, 1)

    def one : Z3AST = mkReal(1, 1)

    def plus(x: Z3AST, y: Z3AST) = mkAdd(x, y)

    def times(x: Z3AST, y: Z3AST) = mkMul(x, y)

    def div(x: Z3AST, y: Z3AST) = mkDiv(x, y)
    
    def gcd(a: z3.scala.Z3AST,b: z3.scala.Z3AST): z3.scala.Z3AST = one 
    
    def mod(a: z3.scala.Z3AST,b: z3.scala.Z3AST): z3.scala.Z3AST = zero 
    
    def quot(a: z3.scala.Z3AST,b: z3.scala.Z3AST): z3.scala.Z3AST = div(a, b)
    
  }
}