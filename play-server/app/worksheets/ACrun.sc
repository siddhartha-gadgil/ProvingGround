package worksheets

import provingground.Collections._
import provingground.FreeGroups.nullpres
import provingground.MoveLearner._
import provingground.MoveLearner
import provingground.AndrewsCurtisModel._

object ACrun {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
//  val presGen = PresentationGen()
  
  val np = nullpres                               //> np  : provingground.FreeGroups.Presentation = <; >
  
  val presdest = FiniteDistribution(Set(Weighted(np, 1.0)))
                                                  //> presdest  : provingground.Collections.FiniteDistribution[provingground.FreeG
                                                  //| roups.Presentation] = [<; > : 1.0]
  
  val mt = MoveTypeList                           //> mt  : List[provingground.AndrewsCurtisModel.ACMoveType] = List(ACStabMv, ACD
                                                  //| eStabMv, RtMultMv, LftMultMv, ConjMv, InvMv)
  
  val mvs = moves                                 //> mvs  : (provingground.FreeGroups.Presentation, provingground.AndrewsCurtisMo
                                                  //| del.ACMoveType) => scala.collection.immutable.Set[provingground.AndrewsCurti
                                                  //| sModel.Vert] = <function2>
  
  
  val l= new MoveLearner(MoveTypeList, moves)     //> l  : provingground.MoveLearner[provingground.AndrewsCurtisModel.Vert,proving
                                                  //| ground.AndrewsCurtisModel.ACMoveType] = provingground.MoveLearner@4c4f7cf7
    
    
  
  val ll = learner                                //> ll  : provingground.MoveLearner[provingground.AndrewsCurtisModel.Vert,provin
                                                  //| gground.AndrewsCurtisModel.ACMoveType] = provingground.MoveLearner@76956344
  
  
  val dst = l.DynDst(FiniteDistribution(Set(Weighted(nullpres, 1.0))), unifmoves, 0.7).norm
                                                  //> dst  : Double = 2.7

 
}