import andrewscurtis._
import SimpleAcEvolution._
import Moves._
import FreeGroups._
import DiffStructure._
import FiniteDistribution._
import FiniteDistributionLearner._
val p = Path.init(2, 2)
val imFB = p.imageFeedbackV
val i = Inv(2)
imFB
imFB(Id)
imFB(i)
p.initV
p.presFn
p.evolved.fdV
p.evolved.fdV(Id)
unifMoves(2)
unifMoves(2)(Id)
val mvs = unifMoves(2)
allMoves(2)
p.evolved
p.evolved.fdV
p.evolved.fdV(Id)
p.evolved.fdV(i)
import DiffStructure._
import FiniteDistributionLearner._
val idFn = extendM(genWtDyn(Id))
idFn((p.current.fdM, p.current.fdV))
idFn((p.current.fdM, p.current.fdV))._2
val invFn= extendM(genWtDyn(i))
invFn((p.current.fdM, p.current.fdV))
invFn((p.current.fdM, p.current.fdV))._2
