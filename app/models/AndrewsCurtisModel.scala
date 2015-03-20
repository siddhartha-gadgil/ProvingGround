package models
//import provingground.AndrewsCurtis.{ACMoveType, allMoves, MoveTypeList, Move}
import provingground.Collections._
import provingground.andrewscurtis.FreeGroups._
import scala.language.reflectiveCalls
import provingground.MoveLearner

import scala.language.implicitConversions

import play.api.data._
import play.api.data.Forms._

import play.api.libs.json._

import math._



//import play.api.libs.iteratee._

//import play.api.libs.EventSource - move this away from this object, so that this can be used in worksheets.


import math.pow

object AndrewsCurtisModel{
    /*
   * The type of moves
   */
  type MoveType = ACMoveType

  type Vert = Presentation

  /*
   * Andrews-Curtis moves
   */
  trait ACMoveType

  case object ACStabMv extends ACMoveType
  case object ACDeStabMv extends ACMoveType
  case object RtMultMv extends ACMoveType
  case object LftMultMv extends ACMoveType
  case object ConjMv extends ACMoveType
  case object InvMv extends ACMoveType


  object ACMoveType{
    def fromString(mv : String) : ACMoveType = (MoveTypeList find (_.toString == mv)).get
  }

  val MoveTypeList : List[ACMoveType] = List(ACStabMv, ACDeStabMv, RtMultMv, LftMultMv, ConjMv, InvMv)

  implicit object ACMoveFreqFormat extends Format[FiniteDistribution[ACMoveType]]{
    def reads(js : JsValue) : JsResult[FiniteDistribution[ACMoveType]] = {
      val pmf = for ((x, y) <- js.as[Map[String, Double]]) yield Weighted(ACMoveType.fromString(x), y)
      JsSuccess(FiniteDistribution(pmf.toSeq))
    }

    def writes(fd : FiniteDistribution[ACMoveType]): JsValue = {
      Json.toJson((for (Weighted(x, y) <- fd.pmf) yield (x.toString, y)).toMap)
    }
  }

    trait Move extends (Vert => Vert){
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




  /*
   * Multiplicity of moves of a given type given a presentation. In general this should be a function of the presentation.
   */
  def multiplicity(rk: Int) : MoveType => Long = {
    case ACStabMv => 1 : Long
    case ACDeStabMv => (1 : Long) // can be zero in practice
    case RtMultMv => (rk : Long) * ((rk : Long) - 1)
    case LftMultMv => (rk : Long) * ((rk : Long) - 1)
    case ConjMv => (rk : Long) * ((rk : Long) * 2 - 1)
    case InvMv => (rk : Long)
  }

  def allMoves(pres : Presentation): MoveType => List[Move] = {
    case ACStabMv => List(ACStab)
    case ACDeStabMv =>
      if (pres.ACstabilized) List(ACDeStab) else List()
    case RtMultMv =>
      val n = pres.rank
      (for (j <-0 to n-1; k <- 0 to n-1 if j != k) yield RtMult(j, k)).toList
    case LftMultMv =>
      val n = pres.rank
      (for (j <-0 to n-1; k <- 0 to n-1 if j != k) yield LftMult(j, k)).toList
    case ConjMv =>
      val n = pres.rank
      (for (j <-0 to n-1; k <- -n to n if k != 0) yield Conj(j, k)).toList
    case InvMv =>
      val n = pres.rank
      (for (j <-0 to n-1) yield Inv(j)).toList
  }

    val moves = (pres: Presentation, mvtyp : ACMoveType) => allMoves(pres)(mvtyp).toSet map ((mv : Move) => mv(pres))

    val learner = new MoveLearner(MoveTypeList, moves)

    import learner._

    case class ACparams(cutoffexp: Int = 7, stableLevelexp : Int = 7, stablSteps: Int = 25, outerSteps: Int = 25,
        epsilonexp: Int = 7,
        presCntnSc : Int = 70, wrdCntnSc : Int = 70){
      val cutoff = pow(2, -cutoffexp)
      val stableLevel = pow(2,-stableLevelexp)
      val epsilon = pow(2, -epsilonexp)
      val presCntn = 0.01 * presCntnSc
      val wrdCntn = 0.01 * wrdCntnSc



      def baseweights = presentationWeight(_ : Presentation, presCntn : Double, wrdCntn : Double)

      implicit def feedback(fd: FiniteDistribution[Presentation]) = fd.feedback(baseweights)

      lazy val learnLoop = LearningLoop(cutoff: Double, stableLevel : Double,
          stablSteps, outerSteps: Int, epsilon: Double)(feedback)

      /*
      def updateGen(gen: Generator[DynDst] = basegen) = {
        gen.dyn = learnLoop.replearn(_)
        gen.isStable = (fst, scnd) => (fst -- scnd).norm < stableLevel * epsilon
      }*/
    }

    val unifmoves = FiniteDistribution.uniform(MoveTypeList)

    val defaultdstbn =  DynDst(FiniteDistribution(Seq(Weighted(nullpres, 1.0))), unifmoves, 0.7)
    /*
    val basegen = ACparams().learnLoop.gen(defaultdstbn)

    val ACenum = basegen.enumerator
    */
//    val dynDstToJson = Enumeratee.map((dstbn : DynDst) => dstbnJson(dstbn.vrtdst))

//    val ACsource = ACenum &> dynDstToJson &> EventSource()

    val ACform = Form(
        mapping(
            "cutoff"  -> number,
            "stable-level" -> number,
            "inner-steps" -> number,
            "outer-steps" -> number,
            "epsilon" -> number,
            "pres-cntn" -> number,
            "word-cntn" -> number
            )(ACparams.apply)(ACparams.unapply))

   private def roundoff(t: (Double, Double, Int, Int, Double)) = (
       round(t._1).toInt, round(t._2).toInt : Int, t._3, t._4, round(t._5).toInt)

   //Note: Uses import learner, should have this as a parameter while abstracting.
   def learnerForm(fd: Feedback) = Form(
       mapping(
            "cutoff"  -> number,
            "stable-level" -> number,
            "inner-steps" -> number,
            "outer-steps" -> number,
            "epsilon" -> number
            )((a: Int, b: Int, c: Int, d: Int, e: Int) => LearningLoop(a, b,c, d, e)(fd))(
                (x) => LearningLoop.unapply(x).map(roundoff)))

    case class PresentationGen(presCntnSc : Int = 70, wrdCntnSc : Int = 70){
      val presCntn = 0.01 * presCntnSc

      val wrdCntn = 0.01 * wrdCntnSc

      def baseweights = presentationWeight(_ : Presentation, presCntn : Double, wrdCntn : Double)

      implicit def feedback(fd: FiniteDistribution[Presentation]) = fd.feedback(baseweights)
    }

    val presentationGenForm = Form(
        mapping(
            "pres-cntn" -> number,
            "word-cntn" -> number
            )(PresentationGen.apply)(PresentationGen.unapply))
}
