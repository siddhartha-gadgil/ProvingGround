package provingground.functionfinder
import provingground.HoTT._
import EnumType._
import ScalaRep._
import scala.reflect.runtime.universe.{Try => UnivTry, Function => FunctionUniv, _}

object EnumFuncs {
	def allMaps[U, V](dom: List[U], codom: List[V]) : List[Map[U, V]] = dom match {
	  case List() => List(Map())
	  case x :: List() => for (y <- codom) yield Map(x-> y)
	  case x :: zs => for (y <- codom; m <- allMaps(zs, codom)) yield (m + (x -> y))
	}
	
	def allFunc[U <: Term with Subs[U] : TypeTag, V <: Term with Subs[V]: TypeTag](domenum: EnumTerm[U])(codomenum: EnumTerm[V]) = {
	  val maps = for (f <- allMaps(domenum.value, codomenum.value)) yield FuncDefn(f, domenum.elemTyp, codomenum.elemTyp)
	  EnumTerm(maps, domenum.elemTyp ->: codomenum.elemTyp)
	}
	
	def EnumFunc[U <: Term with Subs[U] : TypeTag, V <: Term with Subs[V]: TypeTag](dom: Typ[U], codom: Typ[V]) = {
	  val rep = EnumRep(dom) -->: EnumRep(codom) -->: EnumRep(dom ->: codom)
	  rep(allFunc)
	}
	
	val enumFn = depFunc(__, (u: Typ[Term]) => depFunc(__, (v: Typ[Term]) => EnumFunc(u, v)))
	
	
	def allSecMaps[U, V](dom: List[U], codoms: U => List[V]) : List[Map[U, V]] = dom match {
	  case List() => List(Map())
	  case x :: List() => for (y <- codoms(x)) yield Map(x-> y)
	  case x :: zs => for (y <- codoms(x); m <- allSecMaps(zs, codoms)) yield (m + (x -> y))
	}
	
	def allSec[U <: Term with Subs[U] : TypeTag, V <: Term with Subs[V]: TypeTag](
	    domenum: EnumTerm[U])(codomenums: U =>EnumTerm[V])(implicit sv: ScalaUniv[V]) = {
	  val maps = for (f <- allSecMaps(domenum.value, (u: U) => codomenums(u).value)) yield depFunc(domenum.elemTyp, f)
	  val fibre =typFamily(domenum.elemTyp, (u: U) => codomenums(u).elemTyp)
	  EnumTerm(maps, PiTyp(fibre))
	}
	
	def EnumSec[U <: Term with Subs[U] : TypeTag, V <: Term with Subs[V]: TypeTag](
	    dom: Typ[U], codoms: U => Typ[V])(implicit su: ScalaUniv[U], sv: ScalaUniv[V]) = {
	  val fibre =typFamily(dom, codoms)
	  val rep = EnumRep(dom) -->: (dom ~~>: ((u: U) =>  EnumRep(codoms(u)))) -->: EnumRep(PiTyp(fibre))
	  rep(allSec)
	}
	
	
	private val A = "A" :: __
	
	private val Bs = "B" :: A ->:  __
	
	
	val enumSec = 
	  lambda(A)(lambda(Bs)(
	    EnumSec(A, Bs)))
}