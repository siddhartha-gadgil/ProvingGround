import provingground.andrewscurtis._
import provingground.andrewscurtis.FreeGroups._
import org.scalatest._

class WordSpec extends FlatSpec {
  "A Word" should "reduce to the simplest form" in {
    val a = Word(List(1,2,-2,3,-1,1,-3,1))
    val result = Word(List(1, 1))
    assert(a.reduce === result)
  }

  it should "allow concatenation of letters" in {
    val a = Word(List(1,2,-2,3,-1,1,-3,1))
    val let = -1
    val result = Word(List(-1,1,2,-2,3,-1,1,-3,1))
    assert(a.::(let) === result)
  }

  it should "have an inverse" in {
    val a = Word(List(1,2,3,2,-1,-3))
    val result = Word(List(3,1,-2,-3,-2,-1))
    assert(a.inv === result)
  }

  it should "raise to power" in {
    val a = Word(List(1,2,2,-1, 2))
    val power0 = Word(Nil)
    val power2 = Word(List(1,2,2,-1,2,1,2,2,-1, 2))
    val inversepower2 = power2.inv
    assert(a.pow(0) === power0)
    assert(a.pow(1) === a)
    assert(a.pow(2) === power2)
    assert(a.pow(-2) === inversepower2)
  }

  
}
