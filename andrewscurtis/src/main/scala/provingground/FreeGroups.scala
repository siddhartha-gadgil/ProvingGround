package provingground.andrewscurtis

/*
 * Free group in n generators
 * An element is represented as a word in integers, together with rank of the corresponding group
 * The negative of a number represents the inverse generator
 *
 * Perhaps it would be wise to make the recursive
 * calls tail recursive.
 */
object FreeGroups{
  def letterString(n : Int) = if (n > 0) ('a' + n -1).toChar.toString +"." else ('a' - n -1).toChar.toString+"!."

  def letterUnic(n : Int) = if (n > 0) ('a' + n -1).toChar.toString else ('a' - n -1).toChar.toString+ '\u0305'.toString

  case class Word(ls: List[Int]) extends AnyVal {

    def reduce : Word = {
      ls match {
        case x :: y :: zs if x == -y => Word(zs).reduce
        case x :: ys => 
          if (Word(ys).isReduced) 
            x :: Word(ys).reduce
          else (x :: Word(ys).reduce).reduce
        case _ => this
      }
    }

    def isReduced = (this == reduce)

    override def toString = toUnicode

    def toUnicode = ((ls map (letterUnic(_))).foldLeft("")(_+_))

    def ::(let : Int) = Word(let :: ls)

    def inv = Word(ls.reverse map ((n) => -n))

    def ! = inv

    def pow : Int => Word = {
      case 0 => Word(List())
      case k if k > 0 => Word(List.fill(k)(ls).flatten)
      case k if k < 0 => this.inv.pow(-k)
    }

    def ^(n: Int) = pow(n)

    def *(that: Word) = Word(ls ++ that.ls).reduce

    def conj(that: Word) = that.inv * this * that

    def ^(that: Word) = conj(that)

    def conjGen(k: Int) = Word((-k) :: (ls :+ k)).reduce

    def ^^(k: Int)  = conjGen(k)

    def maxgen: Int = {
      if(ls.isEmpty)
        0
      else
        (ls map ((x:Int) => x.abs)).max
    }

    def rmvtop(rank : Int) = Word (ls filter (_.abs < rank))
  }

  def wordWeight(w : Word, wrdCntn: Double, rank : Double) = (1 - wrdCntn) * math.pow(wrdCntn / (2 * rank), w.ls.length)

  /*
   * Objects involved in Andrews-Curtis evolution : Presentations and Moves
   */

  case class Presentation(rels : List[Word], rank : Int){
    require(maxgen<=rank, "There are more generators than the rank allows")

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

    def maxgen:Int = {
      if(rels.isEmpty)
        0
      else
        (rels map ((x:Word) => x.maxgen)).max
    }

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

    def conjRelators (k : Int, l : Int) = {
      val result = (0 to sz -1) map {(i) => if (i == k) rels(k)^rels(l) else rels(i)}
      Presentation(result.toList, rank)
    }

    def ACstab = Presentation(Word(List(rank+1)) :: rels, rank +1)

    def ttzStab = Presentation(Word(List()) :: rels, rank)

    def ACstabilized = rels contains Word(List(rank))

    def ACdestab = {
      val newrels = rels filter ((w : Word) => w != Word(List(rank+1))) map (_.rmvtop(rank))
      Presentation(newrels, rank -1)
    }
  }

  def presentationWeight(pres : Presentation, presCntn : Double, wrdCntn : Double) = {
    val wordwts = pres.rels map (wordWeight(_, wrdCntn, pres.rank))
    (1 - presCntn) * math.pow(presCntn, pres.sz) * ((wordwts :\ 1.0)(_ * _))
  }

  /*
   * Empty presentation
   */
  val nullpres = Presentation(List(), 0)

}
