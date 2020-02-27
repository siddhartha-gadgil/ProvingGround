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

/**
  * Better building of post webs, without depending on separate lists for implicits and history (history to be done).
  */
class HoTTPostWeb {
  import HoTTPostWeb._

  implicit val global: GlobalID[ID] = new CounterGlobalID()

  import global.postGlobal

  var equationNodes: Set[EquationNode] = Set()

  def equations = Equation.group(equationNodes)

  def terms = ExpressionEval.terms(equationNodes)

  def addEqns(eqs: Set[EquationNode]): Unit = {
    equationNodes ++= eqs
  }

  val polyBuffer =
    PostBuffer.build[Lemmas, ID] :: PostBuffer
      .build[InitState, ID] ::
      ErasablePostBuffer.build[FinalState, ID] ::
      ErasablePostBuffer.build[TermResult, ID] ::
      ErasablePostBuffer.build[GeneratedEquationNodes, ID] ::
      PostBuffer.build[Proved, ID] ::
      PostBuffer.build[Contradicted, ID] ::
      PostBuffer.build[LocalTangentProver, ID] ::
      PostBuffer.build[ExpressionEval, ID] ::
      PostBuffer.build[ChompResult, ID] ::
      PostBuffer.build[LocalProver, ID] :: HNil
}

object HoTTPostWeb {
  type ID = (Int, Int)

  val polyImpl = BuildPostable.get((w: HoTTPostWeb) => w.polyBuffer)
  implicit val (b11:: b10:: b9 :: b8 :: b7 :: b6 :: b5 :: b4 :: b3 :: b2 :: b1 :: HNil) =
    polyImpl

  implicit val history: PostHistory[HoTTPostWeb, ID] =
    HistoryGetter.get((w: HoTTPostWeb) => w.polyBuffer)

  implicit def equationNodeQuery: Queryable[Set[EquationNode], HoTTPostWeb] =
    Queryable.simple(_.equationNodes)

  implicit def equationQuery: Queryable[Set[Equation], HoTTPostWeb] =
    Queryable.simple(_.equations)

  implicit def termSetQuery: Queryable[Set[Term], HoTTPostWeb] =
    Queryable.simple(_.terms)
}
