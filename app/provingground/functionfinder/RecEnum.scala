package provingground.functionfinder
import provingground.HoTT._
import EnumType._
import EnumFuncs._
import EnumFin._
import IntTypes._

object RecEnum {
	lazy val recEnumList : Typ[Term]  => Option[List[Term]] = {
	  case Fin(n) => Some(enumFinList(n))
	  case PairTyp(first : Typ[Term], second : Typ[Term]) => 
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
	              for (m <- l) yield depFunc(dom, m))
	          })
	  case SigmaTyp(fiber) =>
	    val dom = fiber.dom.asInstanceOf[Typ[Term]]
	    val domlistopt = recEnumList(dom) 
	    domlistopt flatMap (
	        (domlist) => {
	          val codoms = (x: Term)  => recEnumList(fiber(x).asInstanceOf[Typ[Term]])
	          val maps = allSecMapsOpt(domlist, codoms)
	          for (l <- maps) yield (
	              for (m <- l) yield depFunc(dom, m))
	          })
	  case _ => None
	}
  
}