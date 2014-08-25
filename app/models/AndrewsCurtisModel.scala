package provingground
import provingground.AndrewsCurtis.{ACMoveType, allMoves, MoveTypeList, Move}
import provingground.Collections._
import provingground.FreeGroups._
import provingground.DynInterface._
import scala.language.reflectiveCalls

import scala.language.implicitConversions

import play.api.data._
import play.api.data.Forms._
import math._

//import play.api.libs.iteratee._

//import play.api.libs.EventSource - move this away from this object, so that this can be used in worksheets.


import math.pow

object AndrewsCurtisModel{
    
    
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

