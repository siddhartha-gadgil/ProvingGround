import provingground.andrewscurtis._
import provingground.andrewscurtis.FreeGroups._
import org.scalatest._

class WordSpec extends FlatSpec {
  "A Word" should "reduce to the simplest form" in {
    val a      = Word(Vector(1, 2, -2, 3, -1, 1, -3, 1))
    val result = Word(Vector(1, 1))
    assert(a.reduce === result)
  }

  it should "check whether it's reduced" in {
    val a = Word(Vector(1, 2, 3))
    val b = Word(Vector(1, -2, 3, -3, 2))
    assert(a.isReduced === true)
    assert(b.isReduced === false)
  }

  it should "allow concatenation of letters" in {
    val a      = Word(Vector(1, 2, -2, 3, -1, 1, -3, 1))
    val let    = -1
    val result = Word(Vector(-1, 1, 2, -2, 3, -1, 1, -3, 1))
    assert(a :+ (let) === result)
  }

  it should "have an inverse" in {
    val a      = Word(Vector(1, 2, 3, 2, -1, -3))
    val result = Word(Vector(3, 1, -2, -3, -2, -1))
    assert(a.inv === result)
  }

  it should "raise to power" in {
    val a             = Word(Vector(1, 2, 2, -1, 2))
    val power0        = Word(Vector())
    val power2        = Word(Vector(1, 2, 2, -1, 2, 1, 2, 2, -1, 2))
    val inversepower2 = power2.inv
    assert(a.pow(0) === power0)
    assert(a.pow(1) === a)
    assert(a.pow(2) === power2)
    assert(a.pow(-2) === inversepower2)
  }

  it should "multiply with another word" in {
    val a      = Word(Vector(1, 2, 3, 2))
    val b      = Word(Vector(-2, -1, 1, 3))
    val result = Word(Vector(1, 2, 3, 3))
    assert(a * b === result)
  }

  it should "conjugate with another word" in {
    val a      = Word(Vector(1, 2))
    val b      = Word(Vector(1, 3, -2))
    val result = Word(Vector(-2, 3, -2, 1, 2))
    assert(b.conj(a) === result)
  }

  it should "conjugate with a single letter" in {
    val a      = Word(Vector(1, 2, 3, -2))
    val r      = 2
    val result = Word(Vector(-2, 1, 2, 3))
    assert(a.conjGen(r) === result)
  }

  it should "have a well defined number of generators" in {
    val a      = Word(Vector(1, 2, 3, 4, 5, -8, -4, -2))
    val result = 8
    assert(a.maxgen === result)
  }

  it should "allow removal of letters" in {
    val a      = Word(Vector(1, 2, 3, -4, -2, -1, 3))
    val r      = 3
    val result = Word(Vector(1, 2, -2, -1))
    assert(a.rmvtop(r) === result)
  }
}

class WordObjectSpec extends FlatSpec {
  "Object Word" should "convert a list of Chars to an appropriate list of Ints" in {
    val a = Vector(1, 2, -3, 4, -2, -1)
    assert(a === Word.listFromChars(Word(a).toString.toVector))
  }

  it should "convert a string to a word" in {
    val a = Word(Vector(1, 2, -3, -3, -1, 3, -1))
    assert(a === Word.fromString(a.toString))
  }

  it should "convert apply to a string and return a Word" in {
    val a = Word(Vector(1, 2, -3, -3, -1, 3, -1))
    assert(a === Word(a.toString))
  }
}

class PresentationSpec extends FlatSpec {
  "A Presentation" should "have a size" in {
    val a            = Word(Vector(1, 2, -1))
    val b            = Word(Vector(2, 1, -2))
    val presentation = Presentation(Vector(a, b), 2)
    val result       = 2
    assert(presentation.sz === 2)
  }

  it should "have a well defined defect" in {
    val a  = Word(Vector(1, 2, -1))
    val p1 = Presentation(Vector(a, a), 2)
    val p2 = Presentation(Vector(a, a, a), 2)
    val p3 = Presentation(Vector(a), 2)
    val p4 = Presentation(Vector(a, a), 2)
    assert(p1.defect === 0)
    assert(p2.defect === -1)
    assert(p3.defect === 1)
    assert(p4.defect === 0)
  }

  it should "have a maximum number of generators" in {
    val a      = Word(Vector(1, 2, 3, -5, 4, 2))
    val b      = Word(Vector(3, -2, 6, 1))
    val p      = Presentation(Vector(a, b), 6)
    val result = 6
    assert(p.maxgen === result)
  }

  it should "allow inverting a relation" in {
    val a      = Word(Vector(1, 2))
    val b      = Word(Vector(3, 2))
    val c      = Word(Vector(1, 3, -1))
    val p      = Presentation(Vector(a, b, c), 3)
    val result = Presentation(Vector(a, Word(Vector(-2, -3)), c), 3)
    assert(p.inv(1) === result)
    assert(Presentation.inv(p, 1) === result)
  }

  it should "allow right multiplication by a relation" in {
    val a      = Word(Vector(1, 2))
    val b      = Word(Vector(3, 2))
    val c      = Word(Vector(1, 3, -1))
    val ca     = Word(Vector(1, 3, 2))
    val p      = Presentation(Vector(a, b, c), 3)
    val result = Presentation(Vector(a, b, ca), 3)
    assert(p.rtmult(2, 0) === result)
    assert(Presentation.rtmult(p, 2, 0) === result)
  }

  it should "allow left multiplication by a relation" in {
    val a      = Word(Vector(1, 2))
    val b      = Word(Vector(3, 2))
    val c      = Word(Vector(1, 3, -1))
    val ca     = Word(Vector(1, 3, 2))
    val p      = Presentation(Vector(a, b, c), 3)
    val result = Presentation(Vector(ca, b, c), 3)
    assert(p.lftmult(0, 2) === result)
    assert(Presentation.lftmult(p, 0, 2) === result)
  }

  it should "allow conjugation by a generator" in {
    val a      = Word(Vector(1, 2))
    val b      = Word(Vector(3, 2))
    val c      = Word(Vector(1, 3, -1))
    val d      = Word(Vector(-1, 3, 2, 1))
    val p      = Presentation(Vector(a, b, c), 3)
    val result = Presentation(Vector(a, d, c), 3)
    assert(p.conj(1, 1) === result)
    assert(Presentation.conj(p, 1, 1) === result)
  }

  it should "allow conjugation by relators" in {
    val a      = Word(Vector(1, 2))
    val b      = Word(Vector(3, 2))
    val c      = Word(Vector(1, 3, -1))
    val d      = Word(Vector(-2, -1, 3, 2, 1, 2))
    val p      = Presentation(Vector(a, b, c), 3)
    val result = Presentation(Vector(a, d, c), 3)
    assert(p.conjRelators(1, 0) === result)
    assert(Presentation.conjRelators(p, 1, 0) === result)
  }

  it should "allow Andrews Curtis stablization, i.e, adding another generator" in {
    val a      = Word(Vector(1, 2))
    val b      = Word(Vector(3, 2))
    val c      = Word(Vector(1, 3, -1))
    val d      = Word(Vector(4))
    val p      = Presentation(Vector(a, b, c), 3)
    val result = Presentation(Vector(d, a, b, c), 4)
    assert(p.ACstab === result)
    assert(Presentation.ACstab(p) === result)
  }

  it should "allow Tietze stablization, i.e, adding the identity word" in {
    val a      = Word(Vector(1, 2))
    val b      = Word(Vector(3, 2))
    val c      = Word(Vector(1, 3, -1))
    val p      = Presentation(Vector(a, b, c), 3)
    val result = Presentation(Vector(Word(Vector()), a, b, c), 3)
    assert(Presentation.ttzStab(p) === result)
  }

  it should "tell whether it's Andrews Curtis stabilized or not" in {
    val a  = Word(Vector(1, 2))
    val b  = Word(Vector(2, 3))
    val c  = Word(Vector(3))
    val d  = Word(Vector(4))
    val p1 = Presentation(Vector(a, b, c), 4)
    val p2 = Presentation(Vector(a, b, d), 4)
    assert(p1.ACstabilized === false)
    assert(p2.ACstabilized === true)
  }
}

class PresentationObject extends FlatSpec {
  "Object Presentation" should "interpret a string as a presentation" in {
    val a  = Word(Vector(1, 2))
    val b  = Word(Vector(2, 3))
    val c  = Word(Vector(3))
    val p1 = Presentation(Vector(a, b, c), 4)
    assert(p1 === Presentation.fromString(p1.toString))
  }

  it should "apply on a string and return a presentation" in {
    val a  = Word(Vector(1, 2, -3))
    val p1 = Presentation(Vector(a), 3)
    assert(p1 === Presentation(3, a.toString))
  }

  it should "have a trivial Presentation" in {
    val p = Presentation((1 to 4).toVector map ((x: Int) => Word(Vector(x))), 4)
    assert(Presentation.trivial(4) === p)
  }
}
