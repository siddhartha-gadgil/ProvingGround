package provingground

import HoTT._

object IntTypes {
	object Z extends SmallTyp
	
	case class Zcnst(n : Long) extends Constant{
	  val typ = Z
	}
}