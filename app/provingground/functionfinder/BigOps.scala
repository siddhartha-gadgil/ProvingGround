package provingground.functionfinder
import provingground.HoTT._
import ScalaRep._
import IntTypes._
import EnumType._
import ListType._
import scala.reflect.runtime.universe.{Try => UnivTry, Function => FunctionUniv, _}

object BigOps {
  case class EnumFin(n: Long) extends EnumTerm[Term]{
    val elemTyp = Fin(n)
    
    val value = (0 to (n-1).toInt).toList map ((k: Int) => dsl.i[Long](Fin(n))(k))
  }
  
  val A = "A" :: __ // a type symbol
  
  val f = "f" :: A ->: N
  
  val en = "enumeration" :: EnumTyp(A)
  
  val bigsum = {
    lambda(A)(  
      lambda(en)(
          lambda(f)({
              val lst = lmap(A)(N)(f)(enumlist(A)(en))
              fold(N)(N)(lst)(Nsum)}
              )
              )
      )
  }  
  
}