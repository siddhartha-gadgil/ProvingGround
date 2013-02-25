package provingGround
import provingGround.Structures._
import provingGround.StackedEvolver._

/** Tangles defined and various constructions
  *
  * Example: (Cup | Cup) * (Cap | Cap) gives the two component unlink 
  */
object Tangle{
  /** Tangles */
  trait Tangle extends Groupoid[Tangle]{
    /** Domain */
    val dom: Int
    /** Codomain */
    val codom:Int
    /** Height, i.e., number of pure tangles */
    val height: Int
    /** The pure tangles that make up a tangle */
    def pts: List[PureTangle]
    
    /** Concatenate tangles - always defined but possibly meaningless */
    def concat(that:Tangle):Tangle
    
    /** Partial Product, multiplication */
    val * : PartialFunction[Tangle,Tangle] = {
      case that if that.dom == codom => concat(that)
    }

    /** Number of base tangles in a tangle */
    val weight : Int = this match {
      case b:BaseTangle => 1
      case PureTangle(bts) => bts.length
      case ConTangle(head, tail) => head.weight + tail.weight
    }
  }
  
  /** Pure tangle */
  case class PureTangle(bts: List[BaseTangle]) extends Tangle{   
    def concat(that: Tangle): Tangle = that match {
      case ThinTangle(_) => this
      case t: Tangle => ConTangle(this, t)
    }
    val height = 1
//    lazy val head = this
    lazy val pts = List(this)
    val dom :Int = (bts map (_.dom)).sum
    val codom:Int = (bts map (_.codom)).sum    
    def |(that: PureTangle) = PureTangle(bts ::: that.bts)
    }  
   
  /** A tangle with no rows (in particular an identity */  
  case class ThinTangle(dom: Int) extends Tangle{
    val codom = dom
    val height = 0
    lazy val pts = List()
    def concat(that: Tangle) = that
  }
  
  /** Base tangle */  
  trait BaseTangle extends Tangle{
    def concat(that: Tangle): Tangle = PureTangle(List(this)) concat that
    val bts = List(this)
    val height = 1
    lazy val head = PureTangle(List(this))
    lazy val pts = List(head)
  }
  
  /** A base tangle is implicitly converted to a puretangle */
  implicit def PT(bt: BaseTangle): PureTangle = PureTangle(List(bt))
  
  /** Cup tangle*/
  case object Cup extends BaseTangle{
    val dom = 0
    val codom = 2
  }
  
  /** Cap tangle */
  case object Cap extends BaseTangle{
    val dom = 2
    val codom = 0
  }
  
  /** OverCrossing */
  case object Over extends BaseTangle{
    val dom = 2
    val codom = 2
  }
  
  /** UnderCrossing */
  case object Under extends BaseTangle{
    val dom = 2
    val codom = 2
  }
  
  /** Vetical Tangle */
  case object Vert extends BaseTangle{
    val dom = 1
    val codom = 1
  }  
  

  /** Composite tangle */
  case class ConTangle(head: PureTangle, tail: Tangle) extends Tangle{
    val dom = head.dom
    val codom = head.codom
    val height:Int = 1 + tail.height
    
    def concat(that:Tangle):Tangle = ConTangle(head, tail concat that)
    lazy val pts: List[PureTangle] = head :: tail.pts
    
  }

  /** Parametrized Pure Tangle */
  trait ParamPureTangle[A] extends PureTangle{
    val tangleFn: PartialFunction[A, PureTangle]
    def apply(a: A) = tangleFn(a)
  }
  
  
  /** Stream of all Pure Tangles */
  lazy val pureTangleStream = (wordStream(Set(Vert, Under, Over, Cup, Cap))) map (PureTangle(_))
  
  /** Tangle Evolver */
  lazy val TangleEvolver = new GroupoidEvolver(pureTangleStream : Stream[Tangle])
  
  /** Stream of all tangles */
  lazy val TangleStream = TangleEvolver.flow
}

