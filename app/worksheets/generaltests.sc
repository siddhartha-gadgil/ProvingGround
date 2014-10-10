package worksheets
import provingground.Logic._
import scala.language.implicitConversions



object generaltests {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  implicit def varSym(s: String) = VarSym(s)      //> varSym: (s: String)provingground.Logic.VarSym

  implicit def binRelSym(s: String) = BinOp(s)    //> binRelSym: (s: String)provingground.Logic.BinOp

	val a = VarSym("x") * VarSym("y")         //> a  : provingground.Logic.Term = x*y
	val b = "x" / "y"                         //> b  : provingground.Logic.Term = x/y
	
	val c = "x" << "y"                        //> c  : provingground.Logic.Formula = BinRel(<)(x, y)
	
	println(c.toString)                       //> BinRel(<)(x, y)
	
	2 + 3                                     //> res0: Int(5) = 5
	
	import provingground.HoTT.UnicodeSyms._
	
	println(Arrow)                            //> ⟶
}