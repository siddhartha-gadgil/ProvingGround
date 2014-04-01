package provingGround

import provingGround.FreeGroups._
import provingGround.RandomWords._
import provingGround.Collections._

object AndrewsCurtis{
  
  trait MoveType extends ACobject
  
  case object ACStabMv extends MoveType
  case object ACDeStabMv extends MoveType
  case object RtMultMv extends MoveType
  case object LftMultMv extends MoveType
  case object ConjMv extends MoveType
  case object InvMv extends MoveType
  
  val MoveTypeList = List(ACStabMv, ACDeStabMv, RtMultMv, LftMultMv, ConjMv, InvMv)
  
  def multiplicity(rk: Int) : MoveType => Long = {
    case ACStabMv => 1 : Long
    case ACDeStabMv => (1 : Long) // can be zero in practise
    case RtMultMv => (rk : Long) * (rk : Long)
    case LftMultMv => (rk : Long) * (rk : Long)
    case ConjMv => (rk : Long) * (rk : Long)
    case InvMv => (rk : Long)
  }
  
  def allMoves(pres : Presentation): MoveType => List[Move] = {
    case ACStabMv => List(ACStab)
    case ACDeStabMv => 
      if (pres.ACstabilized) List(ACDeStab) else List()
    case RtMultMv => 
      val n = pres.rank
      (for (j <-0 to n-1; k <- 0 to n-1) yield RtMult(j, k)).toList
    case LftMultMv => 
      val n = pres.rank
      (for (j <-0 to n-1; k <- 0 to n-1) yield LftMult(j, k)).toList
    case ConjMv => 
      val n = pres.rank
      (for (j <-0 to n-1; k <- 0 to n-1) yield Conj(j, k)).toList
    case InvMv => 
      val n = pres.rank
      (for (j <-0 to n-1) yield Inv(j)).toList
  }
  
  
  
  trait Move extends (Presentation => Presentation){
    val mvType : MoveType
  }
  
  case object ACStab extends Move{
    def apply(p: Presentation) = p.ACstab
    
    val mvType = ACStabMv
  }
  
  case object ACDeStab extends Move{
    def apply(p: Presentation) = p.ACdestab
    
    val mvType = ACStabMv
  }
  
  case class RtMult(k : Int, l : Int) extends Move{
    def apply(p: Presentation) = p.rtmult(k, l)
    
    val mvType = ACStabMv
  }
  
  case class LftMult(k : Int, l : Int) extends Move{
    val mvType = ACStabMv
    
    def apply(p: Presentation) = p.lftmult(k, l)
  }
  
  case class Conj(k : Int, l : Int) extends Move{
    def apply(p: Presentation) = p.conj(k, l)
    
    val mvType = ACStabMv
  }
  
  case class Inv(k : Int) extends Move{
    def apply(p: Presentation) = p.inv(k)
    
    val mvType = ACStabMv
  }
  
  trait ACchain{
    val head: Presentation
    
    def prob(d: FiniteDistribution[ACobject], p: Double) : Double
  }
  
  case class ACstart(head : Presentation) extends ACchain{
    def prob(d: FiniteDistribution[ACobject], p: Double) = d(head) * (1 - p)
  }
  
  case class ACextend(start: ACchain, move: Move) extends ACchain{
    val head = move(start.head)
    
    def prob(d: FiniteDistribution[ACobject], p: Double) = start.prob(d, p) * d(move.mvType) * p / multiplicity(head.rank)(move.mvType) 
  }
  
  object ACchain{
	  def offspring(chain : ACchain) = for (mvTyp <- MoveTypeList; mv <- allMoves(chain.head)(mvTyp)) yield ACextend(chain, mv)
	  
	  def fullnextgen(chains: Seq[ACchain]) = (chains flatMap (offspring(_))) ++ chains
	  
	  def prunednextgen(chains : Seq[ACchain], prune : ACchain => Boolean) = (chains filter (prune(_)) flatMap (offspring(_))) ++ chains
  }
}