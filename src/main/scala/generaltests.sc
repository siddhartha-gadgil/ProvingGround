package src.main.scala
import provingGround.Logic._
import scala.language.implicitConversions



object generaltests {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  implicit def varSym(s: String) = VarSym(s)      //> varSym: (s: String)provingGround.Logic.VarSym

  implicit def binRelSym(s: String) = BinOp(s)    //> binRelSym: (s: String)provingGround.Logic.BinOp

	val a = VarSym("x") * VarSym("y")         //> a  : provingGround.Logic.Term = x*y
	val b = "x" / "y"                         //> b  : provingGround.Logic.Term = x/y
	
	val c = "x" << "y"                        //> c  : provingGround.Logic.Formula = BinRel(<)(x, y)
	
	println(c.toString)                       //> BinRel(<)(x, y)
	
	2 + 3                                     //> res0: Int(5) = 5
	
}