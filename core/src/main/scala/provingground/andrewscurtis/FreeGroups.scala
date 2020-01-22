package provingground.andrewscurtis

import provingground.translation.StringParse._

import cats.kernel._

import upickle.default.{ReadWriter => RW, macroRW}
/*
 * Free group in n generators
 * An element is represented as a word in integers, together with rank of the corresponding group
 * The negative of a number represents the inverse generator
 *
 * Perhaps it would be wise to make the recursive
 * calls tail recursive.
 */
object FreeGroups {

  /**
    * String for a letter, e.g. a, a! (for a inverse)
    */
  def letterString(n: Int): String =
    if (n > 0) ('a' + n - 1).toChar.toString + "."
    else ('a' - n - 1).toChar.toString + "!."

  /**
    * unicode string for a letter, e.g. "a" or "\bar{a}"
    */
  def letterUnic(n: Int): String =
    if (n > 0) ('a' + n - 1).toChar.toString
    else ('a' - n - 1).toChar.toString + '\u0305'.toString

  object Word {
    implicit val freeGroup: Group[Word] = new Group[Word] {
      val empty                     = Word(Vector())
      def combine(x: Word, y: Word): Word = x * y
      def inverse(x: Word): Word = x.inv
    }

    implicit def rw: RW[Word] = macroRW

    /**
      * sanity checker for listFromChars.
      * to add further checks later.
      */
    def isParsable(s: Vector[Char]): Boolean = {
      if (s.isEmpty) true
      else if ((s.head == '\u0305') || (s.head == '!')) false
      else true
    }

    /**
      * helper for fromString
      */
    def listFromChars(s: Vector[Char]): Vector[Int] = {
      require(
        isParsable(s),
        "The list of characters is not well formed and should not be parsed.")
      s match {
        case Vector() => Vector()
        case x +: '\u0305' +: tail =>
          (-(x - 'a' + 1)) +: listFromChars(tail)
        case x +: '!' +: tail =>
          (-(x - 'a' + 1)) +: listFromChars(tail)
        case x +: tail =>
          (x - 'a' + 1) +: listFromChars(tail)
      }
    }

    /**
      * word from a string.
      */
    def fromString(s: String): Word =
      if (s == "1") Word(Vector())
      else
        Word(
          listFromChars(
            s.replace("!", "\u0305")
              .replace(" ", "")
              .replace(".", "")
              .toVector))

    /**
      * word from a string.
      */
    def apply(w: String): Word = fromString(w)

    /**
      * the identity
      */
    val e = Word(Vector())
  }

  /**
    * A word in a free group.
    * @param ls letters of the words represented as integers; 1 represents a, -1 represents `a^{-1}`
    */
  case class Word(ls: Vector[Int]) extends AnyVal {

    /**
      * returns reduced form of a word
      */
    def reduce: Word = {
      ls match {
        case x +: ys =>
          Word(ys).reduce match{
            case Word(head +: tail) if head == -x => Word(tail)
            case that => x +: that
          }
        case _ => this
      }
    }

    def isReduced: Boolean = (this == reduce)

    /**
      * string representation
      */
    def toPlainString: String =
      ((ls map (letterString(_))).foldLeft("")(_ + _)).dropRight(1)

    override def toString: String = if (ls.isEmpty) "1" else toUnicode

    def ++(that: Word): Word = Word(ls ++ that.ls).reduce

    /**
      * unicode representation.
      */
    def toUnicode: String = ((ls map (letterUnic(_))).foldLeft("")(_ + _))

    /**
      * letter prepended to word, not reduced
      */
    def +:(let: Int): Word = Word(let +: ls)

    def :+(let: Int): Word = Word(ls :+ let)

    /**
      * inverse
      */
    def inv: Word = Word(ls.reverse map ((n) => -n))

    /**
      * inverse
      */
    def ! : Word = inv

    /**
      * returns this to kth power.
      */
    def pow: Int => Word = {
      case 0          => Word(Vector())
      case k if k > 0 => Word(Vector.fill(k)(ls).flatten).reduce
      case k if k < 0 => this.inv.pow(-k)
    }

    /**
      * raise to nth power.
      */
    def ^(n: Int): Word = pow(n)

    /**
      * multiply and reduce
      */
    def *(that: Word): Word = Word(ls ++ that.ls).reduce

    /**
      * conjugate
      */
    def conj(that: Word): Word = (that.inv * this * that).reduce

    /**
      * conjugate
      */
    def ^(that: Word): Word = conj(that)

    /**
      * conjugate by a generator (or its inverse)
      */
    def conjGen(k: Int): Word = Word((-k) +: (ls :+ (k))).reduce

    /**
      * conjugate by a generator (or its inverse).
      * @param k index of generator, starting at 1.
      */
    def ^^(k: Int): Word = conjGen(k)

    /**
      * largest generator in the free group.
      */
    def maxgen: Int = {
      if (ls.isEmpty) 0
      else (ls map ((x: Int) => x.abs)).max
    }

    /**
      * remove generators of rank and above.
      */
    def rmvtop(rank: Int): Word = Word(ls filter (_.abs < rank))
  }

  /**
    * weight of a word, for a generation process where we extend with some probability, picking letters at random.
    */
  def wordWeight(w: Word, wrdCntn: Double, rank: Double): Double =
    (1 - wrdCntn) * math.pow(wrdCntn / (2 * rank), w.ls.length)

  /*
   * Objects involved in Andrews-Curtis evolution : Presentations and Moves
   */

  /**
    * Finite presentation of a group
    *
    * @param rels relations
    * @param rank number of generators.
    */
  case class Presentation(rels: Vector[Word], rank: Int) {
    require(maxgen <= rank, "There are more generators than the rank allows")

    /**
      * number of relations
      */
    val sz: Int = rels.length

    /**
      * string without  unicode.
      */
    def toPlainString: String = {
      val gens =
        (for (j <- 0 to rank - 1) yield ('a' + j).toChar.toString).toVector
          .mkString(",")
      val relstring =
        (for (rel <- rels) yield rel.toPlainString).toVector.mkString(",")
      s"<$gens; $relstring>"
    }

    /**
      * unicode string
      */
    def toUnicode: String = {
      val gens =
        (for (j <- 0 to rank - 1) yield ('a' + j).toChar.toString).toVector
          .mkString(",")
      val relstring =
        (for (rel <- rels) yield rel.toUnicode).toVector.mkString(",")
      s"<$gens; $relstring>"
    }

    /**
      * unicode string
      */
    override def toString: String = toUnicode

    /**
      * defect of the presentation.
      */
    val defect: Int = rank - sz

    /**
      * largest generator appearing in relation.
      */
    def maxgen: Int = {
      if (rels.isEmpty) 0
      else (rels map ((x: Word) => x.maxgen)).max
    }

    /**
      * returns presentation with ith element inverted.
      */
    def inv(k: Int): Presentation = {
      val result =
        (0 to sz - 1) map { (i) =>
          if (i == k) rels(i).inv else rels(i)
        }
      Presentation(result.toVector, rank)
    }

    /**
      * presentation with kth relation multiplied on the right by the lth relation.
      */
    def rtmult(k: Int, l: Int): Presentation = {
      val result =
        (0 to sz - 1) map { (i) =>
          if (i == k) rels(k) * rels(l) else rels(i)
        }
      Presentation(result.toVector, rank)
    }

    def rtmultinv(k: Int, l: Int): Presentation = {
      val result =
        (0 to sz - 1) map { (i) =>
          if (i == k) rels(k) * (rels(l).inv) else rels(i)
        }
      Presentation(result.toVector, rank)
    }

    /**
      * presentation with kth relation multiplied on the right by the ith relation.
      */
    def lftmult(k: Int, l: Int): Presentation = {
      val result =
        (0 to sz - 1) map { (i) =>
          if (i == k) rels(l) * rels(k) else rels(i)
        }
      Presentation(result.toVector, rank)
    }

    def lftmultinv(k: Int, l: Int): Presentation = {
      val result =
        (0 to sz - 1) map { (i) =>
          if (i == k) (rels(l).inv) * rels(k) else rels(i)
        }
      Presentation(result.toVector, rank)
    }

    def transpose(k: Int, l: Int): Presentation = {
      def flipped: Int => Word = {
        case `k` => rels(l)
        case `l` => rels(k)
        case j   => rels(j)
      }

      val result = (0 to sz - 1) map (flipped)
      Presentation(result.toVector, rank)
    }

    /**
      * presentation with kth relation conjugated by generator with index l.
      */
    def conj(k: Int, l: Int): Presentation = {
      val result =
        0 until sz map { (i) =>
          if (i == k) rels(k) ^^ l else rels(i)
        }
      Presentation(result.toVector, rank)
    }

    /**
      * presentation with the kth relation conjugated by the lth relation
      */
    def conjRelations(k: Int, l: Int): Presentation = {
      val result =
        0 until sz map { (i) =>
          if (i == k) rels(k) ^ rels(l) else rels(i)
        }
      Presentation(result.toVector, rank)
    }

    /**
      * Andrews-Curtis stabilization
      */
    def acStab: Presentation = Presentation(Word(Vector(rank + 1)) +: rels, rank + 1)

    /**
      * Tietze stabilization.
      */
    def ttzStab: Presentation = Presentation(Word(Vector()) +: rels, rank)

    /**
      * returns whether Andrews-Curtis stabilized.
      */
    def acStabilized: Boolean = rels contains (Word(Vector(rank)))

    /**
      * (unsafe) Andrews-Curtis destabilization.
      */
    def acDestabilized: Presentation = {
      val newrels =
        rels filter ((w: Word) => w != Word(Vector(rank + 1))) map
          (_.rmvtop(rank))
      Presentation(newrels, rank - 1)
    }
  }

  object Presentation {

    implicit def rw: RW[Presentation] = macroRW

    /**
      *  parses string to a presentation.
      */
    def fromString(s: String): Presentation = {
      val ss                      = s.replaceAll("[ <>]", "")
      val genWord +: relWord +: _ = ss.split(";").toVector
      val rank                    = genWord.split(",").length
      val rels                    = relWord.split(",") map (Word.fromString(_))
      Presentation(rels.toVector, rank)
    }

    /**
      * gives presentation from rank and strings for words.
      */
    def apply(rank: Int, ws: String*): Presentation =
      Presentation(ws.toVector map (Word.fromString), rank)

    def balanced(ws: String*): Presentation = {
      val rels = ws.toVector map (Word.fromString(_))
      Presentation(rels, rels.length)
    }

    val empty = Presentation(Vector(), 0)

    def trivial(n: Int) =
      Presentation((1 to n).toVector map ((i) => Word(Vector(i))), n)

    /**
      * moves implemented as functions
      */
    def id(pres: Presentation): Presentation = pres
    def inv(pres: Presentation, k: Int): Presentation = pres.inv(k)
    def rtmult(pres: Presentation, k: Int, l: Int): Presentation = pres.rtmult(k, l)
    def lftmult(pres: Presentation, k: Int, l: Int): Presentation = pres.lftmult(k, l)
    def conj(pres: Presentation, k: Int, l: Int): Presentation = pres.conj(k, l)
    def conjRelators(pres: Presentation, k: Int, l: Int): Presentation =
      pres.conjRelations(k, l)
    def acStabilized(pres: Presentation): Presentation = pres.acStab
    def ttzStab(pres: Presentation): Presentation = pres.ttzStab
    def acDestabilized(pres: Presentation): Presentation = pres.acDestabilized

    /**
      * weight where number of relations is fixed.
      */
    def weight(wrdCntn: Double): Presentation => Double =
      (pres: Presentation) => {
        val wordwts = pres.rels map (wordWeight(_, wrdCntn, pres.rank))
        (wordwts.foldRight(1.0)(_ * _))
      }
  }

  def presentationWeight(pres: Presentation,
                         presCntn: Double,
                         wrdCntn: Double): Double = {
    val wordwts = pres.rels map (wordWeight(_, wrdCntn, pres.rank))
    (1 - presCntn) * math.pow(presCntn, pres.sz) * (wordwts.foldRight(1.0)(_ * _))
  }

  /*
   * Empty presentation
   */
  val nullpres: Presentation = Presentation(Vector(), 0)

  implicit def writeWord: WriteString[FreeGroups.Word] =
    WriteString.simple[Word]

  implicit def writePres: WriteString[FreeGroups.Presentation] =
    WriteString.simple[Presentation]

  implicit def readWord: ReadString[FreeGroups.Word] =
    ReadString(Word.fromString)

  implicit def readPres: ReadString[FreeGroups.Presentation] =
    ReadString(Presentation.fromString)
}
