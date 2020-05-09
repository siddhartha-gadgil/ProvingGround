package provingground.learning

import provingground._, HoTT._
import TypedPostResponse._
import monix.eval._
import LocalQueryable._
import monix.execution.Scheduler.Implicits.{global => monixglobal}
import scala.concurrent._
import TermData._
import shapeless._
import scala.collection.SeqView
import scala.reflect.runtime.universe._
import HoTTMessages._
import provingground.induction.ExstInducDefn

/**
  * Better building of post webs, without depending on separate lists for implicits and history (history to be done).
  */
class HoTTPostWeb {
  import HoTTPostWeb._

  implicit val global: GlobalID[ID] = new CounterGlobalID()

  import global.postGlobal

  var equationNodes: Set[EquationNode] = Set()

  var extraTerms: Set[Term] = Set()

  def equations = Equation.group(equationNodes)

  def terms = ExpressionEval.terms(equationNodes) union extraTerms

  def addEqns(eqs: Set[EquationNode]): Unit = {
    equationNodes ++= eqs
  }

  def addTerms(terms: Set[Term]) : Unit = {
    extraTerms ++= terms
  }

  val polyBuffer =
    PostBuffer.build[FunctionForGoal, ID]() ::
      PostBuffer.build[FromAll, ID]() ::
      PostBuffer.build[Consequence, ID]() ::
      PostBuffer.build[UseLemma, ID]() ::
      PostBuffer.build[UseLemmaDistribution, ID]() ::
      PostBuffer.build[Lemmas, ID]() ::
      PostBuffer.build[SeekGoal, ID]() ::
      PostBuffer.build[Instance, ID]() ::
      PostBuffer.build[SeekInstances, ID]() ::
      PostBuffer.build[InitState, ID]() ::
      ErasablePostBuffer.build[FinalState, ID]() ::
      ErasablePostBuffer.build[TermResult, ID]() ::
      ErasablePostBuffer.build[GeneratedEquationNodes, ID]() ::
      PostBuffer.build[Proved, ID]() ::
      PostBuffer.build[Contradicted, ID]() ::
      PostBuffer.build[LocalTangentProver, ID]() ::
      PostBuffer.build[ExpressionEval, ID]() ::
      PostBuffer.build[ChompResult, ID]() ::
      PostBuffer.build[LocalProver, ID]() ::
      PostBuffer.build[Weight, ID]() ::
      PostDiscarder.build[Unit, ID]((0, 0)) ::
      PostDiscarder.build[HNil, ID]((0, 0)) :: HNil

  val polyBuffer2 = 
    ErasablePostBuffer.build[RepresentationMap, ID]() ::
    PostBuffer.build[ExstInducDefn, ID]() ::
    PostBuffer.build[OptimalInitial, ID]() ::
    PostBuffer.build[NarrowOptimizeGenerators, ID]() ::
    PostBuffer.build[FromAny, ID]() ::
    PostBuffer.build[ConsiderInductiveTypes, ID]() :: HNil

  def snapShot : WebState[HoTTPostWeb,HoTTPostWeb.ID] = HoTTPostWeb.history.snapShot(this)
}

object HoTTPostWeb {
  type ID = (Int, Int)

  val polyImpl = BuildPostable.get((w: HoTTPostWeb) => w.polyBuffer)

  implicit val (b22 ::
    b21 :: b20 :: b19 :: b18 :: b17 :: b16 :: b15 :: b14 :: b13 :: b12 :: b11 :: b10 ::
    b9 :: b8 :: b7 :: b6 :: b5 :: b4 :: b3 :: b2 :: b1 :: HNil) =
    polyImpl

  val polyImpl2 = BuildPostable.get((w: HoTTPostWeb) => w.polyBuffer2)

  implicit val (b28 :: b27 :: b26 :: b25 :: b24 :: b23:: HNil) = polyImpl2


  implicit val history: PostHistory[HoTTPostWeb, ID] =
    HistoryGetter.get((w: HoTTPostWeb) => w.polyBuffer :: w.polyBuffer2)

  implicit def equationNodeQuery: Queryable[Set[EquationNode], HoTTPostWeb] =
    Queryable.simple(_.equationNodes)

  implicit def equationQuery: Queryable[Set[Equation], HoTTPostWeb] =
    Queryable.simple(_.equations)

  implicit def termSetQuery: Queryable[Set[Term], HoTTPostWeb] =
    Queryable.simple(_.terms)

  val tw = implicitly[Postable[WithWeight[Proved], HoTTPostWeb, ID]] // a test
}
