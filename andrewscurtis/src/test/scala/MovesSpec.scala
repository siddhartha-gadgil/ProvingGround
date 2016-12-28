import provingground.Collections._
import provingground.andrewscurtis._
import provingground.andrewscurtis.Moves._
import provingground.andrewscurtis.FreeGroups._
import org.scalatest._
import provingground._
import FiniteDistribution._

class AtomicMoveSpec extends FlatSpec {
  val id: AtomicMove = Id

  "AtomicMove" should "act on Moves" in {
    val moves  = Moves(List(Inv(1), LftMult(1, 0)))
    val result = Some(Moves(List(Inv(0), Inv(1), LftMult(1, 0))))
    assert(Inv(0)(moves) === result)
  }

  /*
  it should "act on FiniteDistribution[Moves]" in {
    val fdv = FiniteDistribution.uniform(List(Moves(List(Inv(0))), Moves(List(Inv(1)))))
    val result = FiniteDistribution.uniform(List(Moves(List(Inv(0), Inv(0))), Moves(List(Inv(0), Inv(1)))))
    assert(Inv(0)(fdv) === result)
  }

  it should "act on FiniteDistribution[Presentation]" in {
    val fdp = FiniteDistribution.uniform(List(Presentation(2, "a", "b"), Presentation(2, "ab", "ba")))
    val result = FiniteDistribution.uniform(List(Presentation(2, "a!", "b"), Presentation(2, "b!a!", "ba")))
    assert(Inv(0).actOnPres(fdp) === result)
  }*/

  it should "compose with other AtomicMove" in {
    val that   = Inv(1)
    val result = Moves(List(Inv(0), Inv(1)))
    assert((Inv(0) compose that) === result)
  }

  it should "convert to a function of the type Presentation => Option[Presentation]" in {
    val pres    = Presentation(2, "a", "b")
    val result1 = Some(Presentation(2, "a!", "b"))
    val result2 = Some(Presentation(2, "a", "b!"))
    val result3 = None
    val func1   = Inv(0).toFunc
    val func2   = Inv(1).toFunc
    val func3   = Inv(2).toFunc
    assert(func1(pres) === result1)
    assert(func2(pres) === result2)
    assert(func3(pres) === result3)
  }

  "Id" should "act on a Presentation to give back the same" in {

    val pres   = Presentation(3, "a", "aabaa!", "caba!a!b!")
    val OpPres = Some(pres)
    val result = Some(pres)
    assert(id(pres) === result)
    assert(id(OpPres) === result)
  }

  it should "act on Moves to return the same Moves" in {
    val moves  = Moves(List(Inv(0), RtMult(0, 1), LftMult(1, 0)))
    val result = Some(moves)
    assert(id(moves) === result)
  }

  "Inv" should "act on a Presentation and invert a relator" in {
    val pres    = Presentation(2, "a", "aba!b!", "bab!a")
    val OpPres  = Some(pres)
    val result1 = Some(Presentation(2, "a", "aba!b!", "a!ba!b!"))
    val result2 = None
    assert(Inv(2)(pres) === result1)
    assert(Inv(3)(pres) === result2)
    assert(Inv(2)(OpPres) === result1)
    assert(Inv(3)(OpPres) === result2)
  }

  "RtMult" should "right multiply two relators of a Presentation" in {
    val pres    = Presentation(2, "a", "b")
    val OpPres  = Some(pres)
    val result1 = Some(Presentation(2, "ab", "b"))
    val result2 = Some(Presentation(2, "a", "ba"))
    val result3 = None
    assert(RtMult(0, 1)(pres) === result1)
    assert(RtMult(1, 0)(pres) === result2)
    assert(RtMult(2, 1)(pres) === result3)
    assert(RtMult(0, 2)(pres) === result3)
    assert(RtMult(0, 1)(OpPres) === result1)
    assert(RtMult(1, 0)(OpPres) === result2)
    assert(RtMult(2, 1)(OpPres) === result3)
    assert(RtMult(0, 2)(OpPres) === result3)
  }

  "LftMult" should "left multiply two relators of a Presentation" in {
    val pres    = Presentation(2, "a", "b")
    val OpPres  = Some(pres)
    val result1 = Some(Presentation(2, "ba", "b"))
    val result2 = Some(Presentation(2, "a", "ab"))
    val result3 = None
    assert(LftMult(0, 1)(pres) === result1)
    assert(LftMult(1, 0)(pres) === result2)
    assert(LftMult(2, 1)(pres) === result3)
    assert(LftMult(0, 2)(pres) === result3)
    assert(LftMult(0, 1)(OpPres) === result1)
    assert(LftMult(1, 0)(OpPres) === result2)
    assert(LftMult(2, 1)(OpPres) === result3)
    assert(LftMult(0, 2)(OpPres) === result3)
  }

  "Conj" should "conjugate relators by generators" in {
    val pres    = Presentation(2, "a", "b")
    val OpPres  = Some(pres)
    val result1 = Some(Presentation(2, "b!ab", "b"))
    val result2 = Some(Presentation(2, "bab!", "b"))
    val result3 = None
    assert(Conj(0, 2)(pres) === result1)
    assert(Conj(0, -2)(pres) === result2)
    assert(Conj(0, 3)(pres) === result3)
    assert(Conj(3, 2)(pres) === result3)
    assert(Conj(0, 2)(OpPres) === result1)
    assert(Conj(0, -2)(OpPres) === result2)
    assert(Conj(0, 3)(OpPres) === result3)
    assert(Conj(3, 2)(OpPres) === result3)
  }
}

class AtomicMoveObjectSpec extends FlatSpec {
  val id: AtomicMove = Id
  "AtomicMove companion object" should "read a string and convert it to an AtomicMove" in {
    val inp1    = "id"
    val inp2    = "2!"
    val inp3    = "12!"
    val inp4    = "12!!"
    val inp5    = "12->3"
    val inp6    = "12->3a"
    val inp7    = "12<-3"
    val inp8    = "6^a"
    val inp9    = "6^a!"
    val result1 = Some(id)
    val result2 = Some(Inv(2))
    val result3 = Some(Inv(12))
    val result5 = Some(LftMult(3, 12))
    val result7 = Some(RtMult(12, 3))
    val result8 = Some(Conj(6, 1))
    val result9 = Some(Conj(6, -1))
    assert(AtomicMove.fromString(inp1) === result1)
    assert(AtomicMove.fromString(inp2) === result2)
    assert(AtomicMove.fromString(inp3) === result3)
    assert(AtomicMove.fromString(inp4) === None)
    assert(AtomicMove.fromString(inp5) === result5)
    assert(AtomicMove.fromString(inp6) === None)
    assert(AtomicMove.fromString(inp7) === result7)
    assert(AtomicMove.fromString(inp8) === result8)
    assert(AtomicMove.fromString(inp9) === result9)
  }

  it should "be able to invert fromString with toString" in {
    val inp1 = id
    val inp2 = Inv(12)
    val inp3 = LftMult(3, 6)
    val inp4 = RtMult(3, 6)
    val inp5 = Conj(0, 2)
    assert(AtomicMove(inp1.toString) === inp1)
    assert(AtomicMove(inp2.toString) === inp2)
    assert(AtomicMove(inp3.toString) === inp3)
    assert(AtomicMove(inp4.toString) === inp4)
    assert(AtomicMove(inp5.toString) === inp5)
  }
}

class MovesSpec extends FlatSpec {
  "Moves" should "reduce to functions of type Presentation => Option[Presentation]" in {
    val moves   = Moves(List(Inv(0), Inv(1), Inv(2)))
    val pres1   = Presentation(2, "a", "b")
    val pres2   = Presentation(3, "a", "b", "c")
    val result1 = None
    val result2 = Some(Presentation(3, "a!", "b!", "c!"))
    val fn      = moves.reduce
    assert(fn(pres1) === result1)
    assert(fn(pres2) === result2)
  }

  it should "compose with other Moves" in {
    val moves1 = Moves(List(Inv(0)))
    val moves2 = Moves(List(Inv(1)))
    val result = Moves(List(Inv(0), Inv(1)))
    assert((moves1 compose moves2) === result)
  }

  it should "compose with functions of the type Presentation => Option[Presentation]" in {
    val fn      = Inv(0).toFunc
    val moves   = Moves(List(Inv(1), Inv(2)))
    val new_fn  = moves(fn)
    val pres1   = Presentation(3, "a", "b", "c")
    val pres2   = Presentation(2, "a", "b")
    val result1 = Some(Presentation(3, "a!", "b!", "c!"))
    val result2 = None
    assert(new_fn(pres1) === result1)
    assert(new_fn(pres2) === result2)
  }

  it should "act on AtomicMove" in {
    val moves  = Moves(List(Inv(0)))
    val am     = Inv(1)
    val result = Moves(List(Inv(0), Inv(1)))
    assert(moves(am) === result)
  }

  it should "have a well defined length" in {
    val moves  = Moves(List(Inv(0), Inv(1)))
    val result = 2
    assert(moves.length === result)
  }

  it should "act on a trivial Presentation" in {
    val moves   = Moves(List(Inv(0), Inv(1)))
    val result1 = Some(Presentation(2, "a!", "b!"))
    val result2 = Some(Presentation(3, "a!", "b!", "c"))
    assert(moves.actOnTriv(2) === result1)
    assert(moves.actOnTriv(3) === result2)
  }
}

class MovesObjectSpec extends FlatSpec {
  "fromString" should "parse a list of strings into Moves" in {
    val seqMoves = Seq("id", "4!", "2->3", "4<-3", "6^b!")
    val result =
      Some(Moves(List(Id, Inv(4), LftMult(3, 2), RtMult(4, 3), Conj(6, -2))))
    assert(Moves.fromString(seqMoves) === result)
  }

  "liftOption" should "take a function of type A => Option[A] and return Option[A] => Option[A]" in {
    val f        = ((x: Int) => Some(x * x + 2 * x + 1))
    val lifted_f = liftOption(f)
    val input1   = Some(2)
    val input2   = Some(-1)
    val input3   = None
    val result1  = Some(9)
    val result2  = Some(0)
    val result3  = None
    assert(lifted_f(input1) === result1)
    assert(lifted_f(input2) === result2)
    assert(lifted_f(input3) === result3)
  }

  "liftResult" should "take a function of type A => A and return A => Option[A]" in {
    val f      = ((x: Int) => x * 2)
    val g      = liftResult(f)
    val input  = 2
    val result = Some(4)
    assert(g(input) === result)
  }
}
