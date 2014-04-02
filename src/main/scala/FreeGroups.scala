package provingGround

/* 
 * Free group in n generators
 * An element is represented as a word in integers, together with rank of the corresponding group
 * The negative of a number represents the inverse generator
 */
object FreeGroups{
  case class Word(ls: List[Int]) extends AnyVal{
    def reduce : Word = ls match {
      case x :: y :: zs if x == -y => Word(zs).reduce
      case x :: ys => x :: Word(ys).reduce
      case _ => this
    } 
    
    def ::(let : Int) = Word(let :: ls)
    
    def inv = Word(ls.reverse map ((n) => -n))
    
    def ! = inv
    
    def pow : Int => Word = {
      case 0 => Word(List())
      case k if k >0 => Word(List.fill(k)(ls).flatten)
      case k if k <0 => this.inv.pow(-k)
    }
      
    def ^(n: Int) = pow(n)
    
    def *(that: Word) = Word(ls ++ that.ls).reduce
    
    def conj(that: Word) = that * this * (that.inv)
    
    def ^(that: Word) = conj(that)
    
    def conjGen(k: Int) = Word(k :: (ls :+ (-k)))
    
    def ^^(k: Int)  = conjGen(k)
    
    def maxgen = (ls map (_.abs)).max
    
    def rmvtop(rank : Int) = Word (ls filter (_.abs < rank))
  }
  
  def wordWeight(w : Word, wrdCntn: Double, rank : Double) = (1 - wrdCntn) * math.pow(wrdCntn / (2 * rank), w.ls.length)
  
  /*
   * Objects involved in Andrews-Curtis evolution : Presentations and Moves
   */
  trait ACobject
  
  case class Presentation(rels : List[Word], rank : Int) extends ACobject{
    val sz = rels.length
    
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
  }
  
  def presentationWeight(pres : Presentation, presCntn : Double, wrdCntn : Double) = {
    val wordwts = pres.rels map (wordWeight(_, wrdCntn, pres.rank))
    (1 - presCntn) * math.pow(presCntn, pres.rank) * ((wordwts :\ 1.0)(_ * _))
  }
  
}