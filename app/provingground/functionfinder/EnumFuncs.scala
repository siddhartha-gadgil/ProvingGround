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
	
	def allFunc(domenum: EnumTerm[Term])(codomenum: EnumTerm[Term]) = {
	  val maps = for (f <- allMaps(domenum.value, codomenum.value)) yield FuncDefn(f, domenum.elemTyp, codomenum.elemTyp)
	  EnumTerm(maps, domenum.elemTyp ->: codomenum.elemTyp)
	}
	
	def EnumFunc(dom: Typ[Term], codom: Typ[Term]) = {
	  val rep = EnumRep(dom) -->: EnumRep(codom) -->: EnumRep(dom ->: codom)
	  rep(allFunc)
	}
	
	val enumFn = depFunc(__, (u: Typ[Term]) => depFunc(__, (v: Typ[Term]) => EnumFunc(u, v)))
}