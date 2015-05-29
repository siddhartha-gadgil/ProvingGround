import provingground.normalform._
import provingground.normalform.TopLevel._
import org.scalatest._

class NormSpec extends FlatSpec {
  "A variable" should "add with a variable" in {
    val x = Variable("x")
    val y = Variable("y")
    val result1 = Sigma(CommTerm(Map((Variable("x"), 2))))
    val result2 = Sigma(CommTerm(Map((Variable("x"), 1), (Variable("y"), 1))))
    assert(x+x === result1)
    assert(x+y === result2)
  }

  it should "multiply with a variable" in {
    val x = Variable("x")
    val y = Variable("y")
    val result1 = Pi(CommTerm(Map((Variable("x"), 2))))
    val result2 = Pi(CommTerm(Map((Variable("x"), 1), (Variable("y"), 1))))
    assert(x*x === result1)
    assert(x*y === result2)
  }

  it should "add with a Sigma" in {
    val x = Variable("x")
    val sigma = Sigma(CommTerm(Map((Variable("x"), 2), (Variable("y"), 1))))
    val result = Sigma(CommTerm(Map((Variable("x"), 3), (Variable("y"), 1))))
    assert(x+sigma === result)
  }
}
