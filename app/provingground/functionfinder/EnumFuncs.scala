package provingground.functionfinder
import provingground.HoTT._
import EnumType._
import scala.reflect.runtime.universe.{Try => UnivTry, Function => FunctionUniv, _}

object EnumFuncs {
	def allMaps[U, V](dom: List[U], codom: List[V]) : List[Map[U, V]] = dom match {
	  case List() => List(Map())
	  case x :: List() => for (y <- codom) yield Map(x-> y)
	  case x :: zs => for (y <- codom; m <- allMaps(zs, codom)) yield (m + (x -> y))
	}
	
	def allFunc(dom: EnumTerm[Term], codom: EnumTerm[Term]) = {
	  val maps = for (f <- allMaps(dom.value, codom.value)) yield FuncDefn(f, dom.elemTyp, codom.elemTyp)
	  EnumTerm(maps, dom.elemTyp ->: codom.elemTyp)
	}
}