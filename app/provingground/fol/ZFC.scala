package provingground.fol

import provingground.fol.Logic._
import provingground.Structures._
import provingground.fol.Theory._

object ZFC{

  trait AbsSet extends Const{
    def contains(that: AbsSet): Formula = BinRel("in")(this, that)
    def <::(that: AbsSet): Formula = BinRel("in")(that, this)
    }

  val belongs = BinRel("in")

  case class FiniteUnion(sets: List[AbsSet]) extends AbsSet{
//    val freeVars: Set[Var] = Set.empty
    def this(ss: AbsSet*) = this(ss.toList)
//    def subs(xt: Var=> Term): Term = this
  }

  case class SetUnion(sets: AbsSet) extends AbsSet

  case class FiniteProd(sets: List[AbsSet]) extends AbsSet{
    def this(ss: AbsSet*) = this(ss.toList)
    def this(s: AbsSet, n:Int) = this(((1 to n) map (_ => s)).toList)
    }

  case class PowerSet(s: AbsSet) extends AbsSet

  case class RestSet(s: AbsSet, p: Formula) extends AbsSet

  case class FiniteSet(ss: AbsSet*) extends AbsSet

  case class IntSet(ss: Stream[Int]) extends AbsSet

  val NatNums = IntSet(Stream.from (1))

  abstract class AbsFunc extends AbsSet{
    val domain: AbsSet
    val codom: AbsSet
    }

  abstract class AbsBinRel extends AbsSet{
    val domain: AbsSet
    }

  case class AbsFuncSym(name: String, domain: AbsSet, codom: AbsSet) extends AbsFunc

  case class AbsBinRelSym(name: String, domain: AbsSet) extends AbsBinRel
}
