package provingground
import provingground.HoTT._
import EnumType._
import ScalaRep._
import scala.reflect.runtime.universe.{Try => UnivTry, Function => FunctionUniv, _}
import provingground.ScalaUniverses._

object EnumFuncs {
	def allMaps[U, V](dom: List[U], codom: List[V]) : List[Map[U, V]] = dom match {
	  case List() => List(Map())
	  case x :: List() => for (y <- codom) yield Map(x-> y)
	  case x :: zs => for (y <- codom; m <- allMaps(zs, codom)) yield (m + (x -> y))
	}

	def allSecMaps[U, V](dom: List[U], codoms: U => List[V]) : List[Map[U, V]] = dom match {
	  case List() => List(Map())
	  case x :: List() => for (y <- codoms(x)) yield Map(x-> y)
	  case x :: zs => for (y <- codoms(x); m <- allSecMaps(zs, codoms)) yield (m + (x -> y))
	}

	def allSecMapsOpt[U, V](dom: List[U], codoms: U => Option[List[V]]) : Option[List[Map[U, V]]] = dom match {
	  case List() => Some(List(Map()))
	  case x :: List() =>
	    codoms(x) map ((l) => for (y <-l) yield Map(x-> y))
	  case x :: zs =>
	    val codopt = codoms(x)
	    val tailsecsopt = allSecMapsOpt(zs, codoms)
	    for (l <- codopt; tail <- tailsecsopt) yield (
	        for (y<- l; m <- tail) yield (m + (x -> y) ))
	}

	def pairs[U <: Term with Subs[U], V <: Term with Subs[V]](
	    first: List[U], second: List[V]) = for (x <- first;y <- second) yield PairObj(x, y)

	def allPairs[U <: Term with Subs[U], V <: Term with Subs[V]](
	    dom: List[U], cods: U =>  Option[List[V]]) : Option[List[AbsPair[U, V]]] = dom match {
	      case List() => Some(List())
	      case x :: List() =>
	        for (l <- cods(x)) yield (for (y<-l) yield PairObj(x, y))
	      case x :: zs => {
	        val heads = for (l <- cods(x)) yield (for (y<-l) yield PairObj(x, y))
	        for (h <- heads; t <- allPairs(zs, cods)) yield (h ++ t)
	      }

	    }

	def allFunc[U <: Term with Subs[U] , V <: Term with Subs[V]](domenum: EnumTerm[U])(codomenum: EnumTerm[V]) = {
	  val maps = for (f <- allMaps(domenum.value, codomenum.value)) yield new FuncDefn(f, domenum.elemTyp, codomenum.elemTyp)
	  EnumTerm(maps, domenum.elemTyp ->: codomenum.elemTyp)
	}

	def EnumFunc[U <: Term with Subs[U] , V <: Term with Subs[V]](dom: Typ[U], codom: Typ[V]) = {
	  val rep = EnumRep(dom) -->: EnumRep(codom) -->: EnumRep(dom ->: codom)
	  rep(allFunc)
	}

	val enumFn = lambda("u" :: Type)(
      lambda("v" :: Type)(
          EnumFunc("u" :: Type, "v" :: Type)))
    //depFunc(Type, (u: Typ[Term]) => depFunc(Type, (v: Typ[Term]) => EnumFunc(u, v)))




	def allSec[U <: Term with Subs[U] , V <: Term with Subs[V]](
	    domenum: EnumTerm[U])(codomenums: U =>EnumTerm[V])(implicit sv: ScalaUniv[V]) = {
	  val maps = for (f <- allSecMaps(domenum.value, (u: U) => codomenums(u).value)) yield depFunc(domenum.elemTyp, f)
	  val fibre =typFamily(domenum.elemTyp, (u: U) => codomenums(u).elemTyp)
	  EnumTerm(maps, PiTyp(fibre))
	}

	def EnumSec[U <: Term with Subs[U] , V <: Term with Subs[V]](
	    dom: Typ[U], codoms: U => Typ[V])(implicit su: ScalaUniv[U], sv: ScalaUniv[V]) = {
	  val fibre =typFamily(dom, codoms)
	  val rep = EnumRep(dom) -->: (dom ~~>: ((u: U) =>  EnumRep(codoms(u)))) -->: EnumRep(PiTyp(fibre))
	  rep(allSec)
	}


	private val A = "A" :: Type

	private val Bs = "B" :: A ->:  Type


	val enumSec =
	  lambda(A)(lambda(Bs)(
	    EnumSec(A, Bs)))
}
