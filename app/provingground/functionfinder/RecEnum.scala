package provingground.functionfinder
import provingground.HoTT._
import EnumType._
import EnumFuncs._
import EnumFin._
import IntTypes._
import BoolType._
//import provingground.ScalaUniverses._

object RecEnum {
	lazy val recEnumList : Typ[Term]  => Option[List[Term]] = {
	  case Fin(n) => Some(enumFinList(n))
	  case One => Some(List(Star))
	  case Zero => Some(List())
	  case Bool => Some(List(boolrep(true), boolrep(false)))
	  case IdentityTyp(dom : Typ[_], lhs : Term, rhs: Term) if lhs == rhs => Some(List(Refl(dom, lhs)))
	  case PairTyp(first : Typ[_], second : Typ[_]) => 
	    for (x <- recEnumList(first); y <- recEnumList(second)) yield pairs(x, y)
	  case FuncTyp(dom, codom) =>
	    for (x <- recEnumList(dom); y <- recEnumList(codom)) yield (
	       for (m <- allMaps(x, y)) yield FuncDefn(m, dom, codom) )
	  case PiTyp(fiber) =>
	    val dom = fiber.dom.asInstanceOf[Typ[Term]]
	    val domlistopt = recEnumList(dom) 
	    domlistopt flatMap (
	        (domlist) => {
	          val codoms = (x: Term)  => recEnumList(fiber(x).asInstanceOf[Typ[Term]])
	          val maps = allSecMapsOpt(domlist, codoms)
	          for (l <- maps) yield (
	              for (m <- l) yield lambda("x" :: dom)(m("x" :: dom)))
	          })
	  case SigmaTyp(fiber) =>
	    val dom = fiber.dom.asInstanceOf[Typ[Term]]
	    val domlistopt = recEnumList(dom) 
	    domlistopt flatMap (
	        (domlist) => {
	          val codoms = (x: Term)  => recEnumList(fiber(x).asInstanceOf[Typ[Term]])
	          val maps = allSecMapsOpt(domlist, codoms)
	          for (l <- maps) yield (
	              for (m <- l) yield lambda("x" :: dom)(m("x" :: dom)))
	          })
	  case _ => None
	}
  
}