import provingground.andrewscurtis._
import provingground.andrewscurtis.FreeGroups._
import org.scalatest._

class WordSpec extends FlatSpec {
  "A Word" should "reduce to the simplest form" in {
    val a = Word(List(1,2,-2,3,-1,1,-3,1))
    val result = Word(List(1, 1))
    assert(a.reduce === result)
  }

  it should "check whether it's reduced" in {
    val a = Word(List(1,2,3))
    val b = Word(List(1,-2,3,-3,2))
    assert(a.isReduced === true)
    assert(b.isReduced === false)
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

  it should "multiply with another word" in {
    val a = Word(List(1,2,3,2))
    val b = Word(List(-2,-1,1,3))
    val result = Word(List(1,2,3,3))
    assert(a*b === result)
  }

  it should "conjugate with another word" in {
    val a = Word(List(1,2))
    val b = Word(List(1,3,-2))
    val result = Word(List(-2, 3, -2, 1, 2))
    assert(b.conj(a) === result)
  }

  it should "conjugate with a single letter" in {
    val a = Word(List(1,2,3,-2))
    val r = 2
    val result = Word(List(-2,1,2,3))
    assert(a.conjGen(r) === result)
  }

  it should "have a well defined number of generators" in {
    val a = Word(List(1,2,3,4,5,-8,-4,-2))
    val result = 8
    assert(a.maxgen === result)
  }

  it should "allow removal of letters" in {
    val a = Word(List(1,2,3,-4,-2,-1,3))
    val r = 3
    val result = Word(List(1,2,-2,-1))
    assert(a.rmvtop(r) === result)
  }
}
