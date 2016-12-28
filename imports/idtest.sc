import andrewscurtis._
import SimpleAcEvolution._
import Moves._
import FreeGroups._
import DiffStructure._
import FiniteDistribution._
import FiniteDistributionLearner._
val p = Path.init(2, 2)
val imFbV = p.imageFeedbackV
val i = Inv(2)
val mvs = unifMoves(2)
val idFn = extendM(genWtDyn(Id))
val invFn= extendM(genWtDyn(i))
