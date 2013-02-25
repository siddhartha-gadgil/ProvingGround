package provingGround

import provingGround.Logic._

object MetaLogic{
	trait Assertion{
		val claim: Formula
	}

	trait Assumptions{
		val axioms : Seq[Formula]

		def have
	}

	
}
