package provingground.andrewscurtis

import provingground.StringParse._

// Play Json imports
//import play.api.libs.json._
//import play.api.libs.iteratee._
//import play.api.libs.json.Json.toJsFieldJsValueWrapper

/*
 * Free group in n generators
 * An element is represented as a word in integers, together with rank of the corresponding group
 * The negative of a number represents the inverse generator.
 */
object FreeGroups{
  /**
   * String for a letter, e.g. a, a! (for a inverse)
   */
  def letterString(n : Int) = if (n > 0) ('a' + n -1).toChar.toString +"." else ('a' - n -1).toChar.toString+"!."
  /**
   * unicode string for a letter, e.g. "a" or "\bar{a}"
   */
  def letterUnic(n : Int) = if (n > 0) ('a' + n -1).toChar.toString else ('a' - n -1).toChar.toString+ '\u0305'.toString

  /**
   * A word in a free group.
   * @param ls letters of the words represented as integers; 1 represents a, -1 represents a^{-1}
   */
  case class Word(ls: List[Int]) extends AnyVal{
    /**
     * returns reduced form of a word
     */
    def reduce : Word = ls match {
      case x :: y :: zs if x == -y => Word(zs).reduce
      case x :: ys => x :: Word(ys).reduce
      case _ => this
    }

    /**
     * string representation
     */
    def toPlainString = ((ls map (letterString(_))).foldLeft("")(_+_)).dropRight(1)

    override def toString = toUnicode

    /**
     * unicode representation.
     */
    def toUnicode = ((ls map (letterUnic(_))).foldLeft("")(_+_))

    /**
     * letter prepended to word
     */
    def ::(let : Int) = Word(let :: ls)
    
    /**
     * inverse
     */
    def inv = Word(ls.reverse map ((n) => -n))

    /**
     * inverse
     */
    def ! = inv

    /**
     * returns this to kth power.
     */
    def pow : Int => Word = {
      case 0 => Word(List())
      case k if k >0 => Word(List.fill(k)(ls).flatten)
      case k if k <0 => this.inv.pow(-k)
    }

    /**
     * raise to nth power.
     */
    def ^(n: Int) = pow(n)

    /**
     * multiply and reduce
     */
    def *(that: Word) = Word(ls ++ that.ls).reduce

    /**
     * conjugate
     */
    def conj(that: Word) = that.inv * this * that

    /**
     * conjugate
     */
    def ^(that: Word) = conj(that)

    /**
     * conjugate by a generator (or its inverse)
     */
    def conjGen(k: Int) = Word((-k) :: (ls :+ k)).reduce

    /**
     * conjugate by a generator (or its inverse).
     */
    def ^^(k: Int)  = conjGen(k)

    /**
     * largest generator in the free group.
     */
    def maxgen = (ls map (_.abs)).max

    /**
     * remove generators of rank and above.
     */
    def rmvtop(rank : Int) = Word (ls filter (_.abs < rank))
  }

  object Word{
    def fromString(s: String) : Word = {
      val ss = s.replace("!", "\u0305").replace(" ", "").replace(".", "")
      ss.toList match {
      case Nil => Word(Nil)
      case x :: '\u0305' :: tail =>
        {// println(ss)
         // println("here  ")
        //  println(tail)
         // println(tail.length)
          println(tail.headOption)
          println("inverse")
          println(tail.length)
  //        println(tail)
          Word((-(x - 'a' + 1)) :: fromString(tail.toString).ls)
          
        }
      case x :: tail => {
       // println(ss)
       // println("here")
       // println(tail)
       // println(tail.length)
        println(tail.headOption)
        println("no inverse")
        println(tail.length)
  //      println(tail)
        Word((x - 'a' + 1) :: fromString(tail.toString).ls)
      }
    }
    }
    
    def apply(w: String) = fromString(w)
  }

  def wordWeight(w : Word, wrdCntn: Double, rank : Double) = (1 - wrdCntn) * math.pow(wrdCntn / (2 * rank), w.ls.length)

  /*
   * Objects involved in Andrews-Curtis evolution : Presentations and Moves
   */
//  trait ACobject

  case class Presentation(rels : List[Word], rank : Int){
    val sz = rels.length

    def toPlainString = {
      val gens = (for (j <- 0 to rank-1) yield ('a'+j).toChar.toString).foldLeft("")((x ,y) => x + ","+ y)
      val relstring = (for (rel <- rels) yield rel.toString).foldLeft("")((x ,y) => x + ", "+ y)
      "<"+gens.drop(1)+"; "+relstring.drop(2)+">"
    }

    def toUnicode = {
      val gens = (for (j <- 0 to rank-1) yield ('a'+j).toChar.toString).foldLeft("")((x ,y) => x + ","+ y)
      val relstring = (for (rel <- rels) yield rel.toUnicode).foldLeft("")((x ,y) => x + ", "+ y)
      "<"+gens.drop(1)+"; "+relstring.drop(2)+">"
    }

    override def toString = toUnicode

    val defect = rank - sz

    def maxgen = (rels map (_.maxgen)).max

    def inv (k : Int) = {
      val result = (0 to sz -1) map {(i) => if (i == k) rels(i).inv else rels(i)}
      Presentation(result.toList, rank)
    }

    def rtmult (k : Int, l : Int) = {
      val result = (0 to sz -1) map {(i) => if (i == k) rels(k) * rels(l) else rels(i)}
      Presentation(result.toList, rank)
    }

    def lftmult (k : Int, l : Int) = {
      val result = (0 to sz -1) map {(i) => if (i == k) rels(l) * rels(k) else rels(i)}
      Presentation(result.toList, rank)
    }

    def conj (k : Int, l : Int) = {
      val result = (0 to sz -1) map {(i) => if (i == k) rels(k)^^l else rels(i)}
      Presentation(result.toList, rank)
    }

    def ACstab = Presentation(Word(List(rank+1)) :: rels, rank +1)

    def ttzStab = Presentation(Word(List()) :: rels, rank)

    def ACstabilized = rels contains ((w : Word) => w == Word(List(rank+1)))

    def ACdestab = {
      val newrels = rels filter ((w : Word) => w != Word(List(rank+1))) map (_.rmvtop(rank))
      Presentation(newrels, rank -1)
    }
/*
    def toJson = {
      val listlist = rels map (_.ls)
      Json.obj("rank" -> rank, "words" -> listlist)
    }*/
  }
  
  object Presentation{
    def fromString(s: String) = {
      val ss = s.replaceAll("[ <>]", "")
      val genWord :: relWord :: _ = ss.split(";").toList
      val rank = genWord.split(",").length
      val rels = relWord.split(",") map (Word.fromString(_))
      Presentation(rels.toList, rank)
    }
    
    def apply(rank: Int, ws: String*) : Presentation = 
      Presentation(ws.toList map (Word.fromString), rank)
      
    val empty = Presentation(List(), 0)
    
    def trivial(n: Int) = 
      Presentation((1 to n).toList map ((i) => Word(List(i))), n)
  }
  
  def presentationWeight(pres : Presentation, presCntn : Double, wrdCntn : Double) = {
    val wordwts = pres.rels map (wordWeight(_, wrdCntn, pres.rank))
    (1 - presCntn) * math.pow(presCntn, pres.sz) * ((wordwts :\ 1.0)(_ * _))
  }

  /*
   * Empty presentation
   */
  val nullpres = Presentation(List(), 0)

  implicit def writeWord = WriteString.simple[Word]
  
  implicit def writePres = WriteString.simple[Presentation]
  
  implicit def readWord = ReadString(Word.fromString)
  
  implicit def readPres = ReadString(Presentation.fromString)
}
