package provingground.functionfinder
import provingground.HoTT._
import ScalaRep._
import scala.reflect.runtime.universe.{Try => UnivTry, Function => FunctionUniv, _}


object PlusTypInduc {
	import PlusTyp.{FirstIncl, ScndIncl}
  
	case class PlusExtendedFunction[V <: Term : TypeTag](
	    first: Typ[Term], second: Typ[Term], codom: Typ[V] with Subs[Typ[V]], firstfn: FuncObj[Term, V], 
	    scndfn: FuncObj[Term, V]) extends FuncObj[Term, V] with Subs[PlusExtendedFunction[V]]{
    
	  val dom = pair(first, second)
	  
	  val typ = dom ->: codom
	  
	  def apply(u : Term) = u match {
	    case FirstIncl(`first`, a) => firstfn(a)
	    case ScndIncl(`second`, b) => scndfn(b)
	    case _ => codom.symbObj(ApplnSym(this, u))
	  }
	  
	  val domobjtpe: reflect.runtime.universe.Type = typeOf[Term]
	  
	  val codomobjtpe: reflect.runtime.universe.Type = typeOf[V]
	  
	  
	  def subs(x: provingground.HoTT.Term,y: provingground.HoTT.Term) = PlusExtendedFunction(
	      first.subs(x,y), second.subs(x,y), codom.subs(x, y), firstfn.subs(x,y), scndfn.subs(x,y))	  
  }
	
	val A ="A" :: __
  
	val B = "B" :: __
  
	val C = "C" :: __
	
	val f = "f" :: A ->: C
	
	val g = "g " :: A ->: C
	
	val rec = lambda(A)(
      lambda(B)(
          lambda(C)(
              lambda(f)(
            		  lambda(g)(
            		      PlusExtendedFunction(A, B, C, f, g) )
            		      ))))
}