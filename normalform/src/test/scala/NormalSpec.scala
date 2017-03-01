import provingground.normalform._
import provingground.normalform.NormalForm._
import org.scalatest._

class MainStuctureSpec extends FlatSpec {
  "A variable" should "add with a variable" ignore {
    val x = Variable("x")
    val y = Variable("y")
    val result1 = Sigma(CommTerm(Map((Variable("x"), 2))))
    val result2 = Sigma(CommTerm(Map((Variable("x"), 1), (Variable("y"), 1))))
    assert(x + x === result1)
    assert(x + y === result2)
  }

  it should "multiply with a variable" ignore {
    val x = Variable("x")
    val y = Variable("y")
    val result1 = Pi(CommTerm(Map((Variable("x"), 2))))
    val result2 = Pi(CommTerm(Map((Variable("x"), 1), (Variable("y"), 1))))
    assert(x * x === result1)
    assert(x * y === result2)
  }

  it should "add with a Sigma" ignore {
    val x = Variable("x")
    val sigma = Sigma(CommTerm(Map((Variable("x"), 2), (Variable("y"), 1))))
    val result = Sigma(CommTerm(Map((Variable("x"), 3), (Variable("y"), 1))))
    assert(x + sigma === result)
  }

  it should "multiply with a Pi" ignore {
    val x = Variable("x")
    val pi = Pi(CommTerm(Map((Variable("x"), 2), (Variable("y"), 1))))
    val result = Pi(CommTerm(Map((Variable("x"), 3), (Variable("y"), 1))))
    assert(x * pi === result)
  }

  "A number" should "add with a number" ignore {
    val a = NumTerm(4)
    val b = NumTerm(5.5)
    val result = NumTerm(9.5)
    assert(a + b === result)
    assert(
        Sigma(CommTerm(Map((a, 1)))) + Sigma(CommTerm(Map((b, 1)))) === result)
    assert(Sigma(AssocTerm(List(a))) + Sigma(CommTerm(Map((b, 1)))) === result)
  }

  it should "multiply with a number" ignore {
    val a = NumTerm(3)
    val b = NumTerm(1.5)
    val result = NumTerm(4.5)
    assert(a * b === result)
    assert(Pi(CommTerm(Map((a, 1)))) + Pi(CommTerm(Map((b, 1)))) === result)
    assert(Pi(AssocTerm(List(a))) + Pi(CommTerm(Map((b, 1)))) === result)
  }

  it should "add with a variable" ignore {
    val a = NumTerm(3)
    val b = Variable("b")
    val result = Sigma(CommTerm(Map((NumTerm(3), 1), (Variable("b"), 1))))
    assert(a + b === result)
  }

  it should "multiply with a variable" ignore {
    val a = NumTerm(3)
    val b = Variable("b")
    val result = Pi(CommTerm(Map((NumTerm(3), 1), (Variable("b"), 1))))
    assert(a * b === result)
  }

  it should "add with a sigma" ignore {
    val a = NumTerm(4)
    val b = Sigma(CommTerm(Map((NumTerm(3), 1), (Variable("b"), 1))))
    val result = Sigma(CommTerm(Map((NumTerm(7), 1), (Variable("b"), 1))))
    assert(a + b === result)
  }

  it should "multiply with a sigma" ignore {
    val a = NumTerm(4)
    val b = Sigma(AssocTerm(List(NumTerm(3), Variable("x"))))
    val result = Sigma(AssocTerm(
            List(NumTerm(12),
                 Pi(CommTerm(Map((NumTerm(4), 1), (Variable("x"), 1)))))))
    assert(a * b === result)
  }

  it should "add with a pi" ignore {
    val a = NumTerm(4)
    val b = Pi(AssocTerm(List(NumTerm(2), Variable("x"))))
    val c = Pi(AssocTerm(List(NumTerm(2))))
    val d = Pi(AssocTerm(List(NumTerm(2), NumTerm(3))))
    val resultb = Sigma(CommTerm(Map((a, 1), (b, 1))))
    val resultc = NumTerm(6)
    val resultd = NumTerm(10)
    assert(a + b === resultb)
    assert(a + c === resultc)
    assert(a + d === resultd)
  }

  it should "multiply with a pi" ignore {
    val a = NumTerm(4)
    val b = Pi(AssocTerm(List(NumTerm(2), Variable("x"))))
    val c = Pi(AssocTerm(List(NumTerm(2))))
    val resultb1 = Pi(AssocTerm(List(NumTerm(8), Variable("x"))))
    val resultb2 = Pi(AssocTerm(List(NumTerm(2), Variable("x"), NumTerm(4))))
    val resultc = NumTerm(8)
    assert(a * b === resultb1)
    assert(b * a === resultb2)
    assert(a * c === resultc)
  }

  "A sigma" should "add with a sigma" ignore {
    val a = Sigma(CommTerm(Map((Variable("x"), 1), (Variable("y"), 1))))
    val b = Sigma(CommTerm(Map((Variable("x"), 1), (Variable("z"), 1))))
    val result1 = Sigma(CommTerm(
            Map((Variable("x"), 2), (Variable("z"), 1), (Variable("y"), 1))))
    assert(a + b === result1)
  }

  it should "multiply with a sigma" ignore {
    val a = Sigma(CommTerm(Map((Variable("x"), 1), (Variable("y"), 1))))
    val x = Variable("x")
    val y = Variable("y")
    val result = Sigma(
        CommTerm(Map((Pi(CommTerm(Map((x, 2)))), 1),
                     (Pi(CommTerm(Map((x, 1), (y, 1)))), 2),
                     (Pi(CommTerm(Map((y, 2)))), 1))))
    assert(a * a === result)
  }

  it should "add with a pi" ignore {
    val a = Sigma(CommTerm(Map((Variable("x"), 1), (Variable("y"), 1))))
    val b = Pi(CommTerm(Map((Variable("x"), 1), (Variable("z"), 1))))
    val result = Sigma(
        CommTerm(
            Map((Variable("x"), 1),
                (Variable("y"), 1),
                (Pi(CommTerm(Map((Variable("x"), 1), (Variable("z"), 1)))),
                 1))))
    assert(a + b === result)
  }

  it should "multiply with a pi" ignore {
    val a = Sigma(CommTerm(Map((Variable("x"), 1), (Variable("y"), 1))))
    val b = Pi(CommTerm(Map((Variable("x"), 1), (Variable("z"), 1))))
    val result = Sigma(
        CommTerm(
            Map((Pi(CommTerm(Map((Variable("x"), 2), (Variable("z"), 1)))), 1),
                (Pi(CommTerm(Map((Variable("x"), 1),
                                 (Variable("y"), 1),
                                 (Variable("z"), 1)))),
                 1))))
    assert(a * b === result)
  }

  "A Pi" should "multiply with a Pi" ignore {
    val a = Pi(CommTerm(Map((Variable("x"), 1), (Variable("z"), 1))))
    val b = Pi(CommTerm(Map((Variable("x"), 1), (Variable("y"), 1))))
    val result = Pi(CommTerm(
            Map((Variable("x"), 2), (Variable("z"), 1), (Variable("y"), 1))))
    assert(a * b === result)
  }
}

class AuxiliaryFunctionSpec extends FlatSpec {
  "listToMap" should "take a list and create a map by counting occurences" ignore {
    val a = List()
    val b = List(1, 2, 3, 4)
    val c = List(1, 1, 1)
    val d = List(1, 2, 3, 2, 1)

    val resulta = Map()
    val resultb = Map((1, 1), (2, 1), (3, 1), (4, 1))
    val resultc = Map((1, 3))
    val resultd = Map((1, 2), (2, 2), (3, 1))

    assert(listToMap(a) == resulta)
    assert(listToMap(b) == resultb)
    assert(listToMap(c) == resultc)
    assert(listToMap(d) == resultd)
  }

  "toSigma" should "take a term and return a singleton Sigma containing it" ignore {
    val a = NumTerm(4)
    val resulta = Sigma(CommTerm(Map((a, 1))))
    assert(toSigma(a) === resulta)
  }

  "toPi" should "take a term and return a singleton Pi containing it" ignore {
    val a = NumTerm(4)
    val resulta = Pi(CommTerm(Map((a, 1))))
    assert(toPi(a) === resulta)
  }

  "numTermMatch" should "return true for NumTerm and false for everything else" ignore {
    val a = NumTerm(4)
    val b = Variable("x")
    val c = Sigma(AssocTerm(List(Variable("y"))))
    assert(numTermMatch(a) === true)
    assert(numTermMatch(b) === false)
    assert(numTermMatch(c) === false)
  }

  "clumpTerms" should "clump together adjacent items ignore a list based on whether they satisfy a predicate" ignore {
    val pred = ((x: Int) => x % 2 == 0)
    val clump = ((x: List[Int]) => clumpTerms(x, pred))
    val list1 = List(1, 2, 3, 4, 5, 6)
    val list2 = List(2, 4, 6, 8, 10, 12)
    val list3 = List(1, 3, 5, 7, 9, 11)
    val list4 = List(1, 2, 4, 3, 5, 7, 6, 8, 10)
    val result1 = List(List(1), List(2), List(3), List(4), List(5), List(6))
    val result2 = List(List(2, 4, 6, 8, 10, 12))
    val result3 = List(List(1, 3, 5, 7, 9, 11))
    val result4 = List(List(1), List(2, 4), List(3, 5, 7), List(6, 8, 10))
    assert(clump(list1) === result1)
    assert(clump(list2) === result2)
    assert(clump(list3) === result3)
    assert(clump(list4) === result4)
  }

  "commReduce" should "eagerly reduce all the numeric terms ignore a CommRep[Term] to a single Term" ignore {
    val a = CommTerm(Map((NumTerm(3), 2), (NumTerm(2), 1)))
    val b = CommTerm(Map((Variable("x"), 2), (Variable("y"), 3)))
    val c =
      CommTerm(Map((NumTerm(18), 1), (Variable("x"), 2), (Variable("y"), 3)))
    val resulta = CommTerm(Map((NumTerm(18), 1)))
    val resultb = b
    val resultc = resulta combine b
    assert(commReduce(mulOp, a) === resulta)
    assert(commReduce(mulOp, b) === resultb)
    assert(commReduce(mulOp, c) === resultc)
  }

  "assocReduce" should "eagerly reduce all adjacent numeric terms ignore an AssocRep[Term] to as few terms as possible" ignore {
    val a = AssocTerm(List(NumTerm(4), NumTerm(2), Variable("x"), NumTerm(6)))
    val b = AssocTerm(List(NumTerm(4), NumTerm(2), NumTerm(6)))
    val c = AssocTerm(List(Variable("x"), Variable("z")))
    val resulta = AssocTerm(List(NumTerm(8), Variable("x"), NumTerm(6)))
    val resultb = AssocTerm(List(NumTerm(48)))
    assert(assocReduce(mulOp, a) === resulta)
    assert(assocReduce(mulOp, b) === resultb)
    assert(assocReduce(mulOp, c) === c)
  }

  "semiReduce" should "reduce a term to the simplest form it can" ignore {
    val number = NumTerm(4)
    val symbol = Variable("x")
    val singleSigma = toSigma(number)
    val singlePi = toPi(number)
    val sigma = Sigma(AssocTerm(List(number, number, symbol, number)))
    val pi = Pi(AssocTerm(List(number, number, symbol, number)))
    val resultsigma = Sigma(AssocTerm(List(NumTerm(8), symbol, number)))
    val resultpi = Pi(AssocTerm(List(NumTerm(16), symbol, number)))
    assert(semiReduce(number) === number)
    assert(semiReduce(symbol) === symbol)
    assert(semiReduce(singleSigma) === number)
    assert(semiReduce(singlePi) === number)
    assert(semiReduce(sigma) === resultsigma)
    assert(semiReduce(pi) === resultpi)
  }
}
