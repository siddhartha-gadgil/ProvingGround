package provingground.learning

import org.tensorflow._
import org.tensorflow.op._, linalg.MatMul
import org.tensorflow.types._
import scala.util.Using
import org.tensorflow.ndarray._
import org.tensorflow.framework.optimizers._
import provingground._, HoTT._
import org.tensorflow.op.core.{Shape => _, _}
import ProofScorePredictor._
import TensorFlowSyntax._
import scala.jdk.CollectionConverters._
import TFLayers._
//  The first attempt at predicting scores of ingredients in proofs on statements.

case class TermStatementParameters(
    statementProb: Double,
    termProb: Double,
    termScoreInStatement: Double,
    termProofScore: Double,
    termRep: Vector[Double],
    termRepFuzziness: Double,
    statementRep: Vector[Double],
    statementRepFuzziness: Double
) {
  val statementProbT: TFloat32 = TFloat32.scalarOf(statementProb.toFloat)
  val termProbT: TFloat32      = TFloat32.scalarOf(termProb.toFloat)
  val termScoreInStatementT: TFloat32 =
    TFloat32.scalarOf(termScoreInStatement.toFloat)
  val termProofScoreT: TFloat32 = TFloat32.scalarOf(termProofScore.toFloat)

  val termRepresentationT: TFloat32 = TFloat32.tensorOf(
    StdArrays.ndCopyOf((termRepFuzziness +: termRep).map(_.toFloat).toArray)
  )
  val statementRepresentationT: TFloat32 = TFloat32.tensorOf(
    StdArrays
      .ndCopyOf((statementRepFuzziness +: statementRep).map(_.toFloat).toArray)
  )
}

case class TermStatementVariables(
    statementProbFanInit: Array[Float],
    termProbFanInit: Array[Float],
    termProofScoreFanInit: Array[Float],
    termScoreInStatementFanInit: Array[Float]
)

object TermStatementVariables {
  def random(dim: Int): TermStatementVariables =
    TermStatementVariables(
      randomArray(dim),
      randomArray(dim),
      randomArray(dim),
      randomArray(dim)
    )
}

object ProofScorePredictor {
  val rnd = new scala.util.Random()
}

trait ProofScorePredictor[ParamVars, TermRepVars, StatRepVars, TopVars]
    extends TFVars[
      (TermStatementVariables, ParamVars, TermRepVars, StatRepVars, TopVars)
    ] {
  val graph: Graph
  val representationDimension: Int
  val paramLayerDimension: Int

  implicit lazy val tf: Ops = Ops.create(graph)

  // the probabilities and scores that go into prediction

  /**
    * generation probability of type whose inhabitants we seek
    */
  lazy val statementProb: PlaceholderWithDefault[TFloat32] =
    tf.placeholderWithDefault(tf.constant(0.0f), Shape.of())

  /**
    * generation probability of term whose score as ingredient we seek
    */
  lazy val termProb: PlaceholderWithDefault[TFloat32] =
    tf.placeholderWithDefault(tf.constant(0.0f), Shape.of())

  /**
    * score of term as ingredient of statement
    *
    * @return placeholder for term score
    */
  lazy val termScoreInStatement: PlaceholderWithDefault[TFloat32] =
    tf.placeholderWithDefault(tf.constant(0.0f), Shape.of())

  /**
    * score of the term as a proof of its type, essentially non-triviality
    */
  lazy val termProofScore: PlaceholderWithDefault[TFloat32] =
    tf.placeholderWithDefault(tf.constant(0.0f), Shape.of())

  // representations, first entry should be the "fuzziness"
  lazy val termRepresentation: PlaceholderWithDefault[TFloat32] =
    tf.placeholderWithDefault(
      tf.constant(Array.fill(representationDimension)(0.0f)),
      Shape.of(representationDimension)
    )

  lazy val statementRepresentation: PlaceholderWithDefault[TFloat32] =
    tf.placeholderWithDefault(
      tf.constant(Array.fill(representationDimension)(0.0f)),
      Shape.of(representationDimension)
    )

  def paramFanOut: core.Variable[TFloat32] =
    tf.variable(
      tf.constant(Array.fill(paramLayerDimension)(rnd.nextGaussian().toFloat))
    )

  val fanInit: TermStatementVariables

  import fanInit._

  lazy val statementProbFan = tf.variable(tf.constant(statementProbFanInit))

  lazy val termProbFan = tf.variable(tf.constant(termProbFanInit))

  lazy val termProofScoreFan = tf.variable(tf.constant(termProofScoreFanInit))

  lazy val termScoreInStatementFan =
    tf.variable(tf.constant(termScoreInStatementFanInit))

  lazy val paramMergedVector =
    (statementProbFan * statementProb) +
      (termProbFan * termProb) +
      (termProofScoreFan * termProofScore) +
      (termScoreInStatementFan + termScoreInStatement)

  val paramLayers: TFLayers[ParamVars]

  val termRepLayers: TFLayers[TermRepVars]

  val statementRefLayers: TFLayers[StatRepVars]

  val mergedVector = paramLayers.output(
    tf.reshape(paramMergedVector, tf.constant(Array(paramLayerDimension, 1)))
  ) + termRepLayers
    .output(termRepresentation) + statementRefLayers.output(
    statementRepresentation
  )

  // output except the final sigmoid
  val preTopLayers: TFLayers[TopVars]

  val prediction = tf.reshape(
    preTopLayers.output(mergedVector),
    tf.constant(Array.emptyIntArray)
  )

  def runnerWithData(
      data: TermStatementParameters,
      session: Session
  ): Session#Runner =
    session
      .runner()
      .feed(statementProb, data.statementProbT)
      .feed(termProb, data.termProbT)
      .feed(termScoreInStatement, data.termScoreInStatementT)
      .feed(termProofScore, data.termProofScoreT)
      .feed(termRepresentation, data.termRepresentationT)
      .feed(statementRepresentation, data.statementRepresentationT)

  def predict(data: TermStatementParameters) = Using(new Session(graph)) {
    session =>
      val tData = runnerWithData(data, session).fetch(prediction).run()
      tData.get(0).asInstanceOf[TFloat32].getFloat()
  }

  def ownVars(session: Session): TermStatementVariables = {
    val dataVec = session
      .runner()
      .fetch(statementProbFan)
      .fetch(termProbFan)
      .fetch(termProofScoreFan)
      .fetch(termScoreInStatementFan)
      .run()
      .asScala
      .toVector
      .map(_.asInstanceOf[TFloat32])

    val v = dataVec.map(
      tfl => Array.tabulate(paramLayerDimension)(j => tfl.getFloat(j))
    )
    TermStatementVariables(v(0), v(1), v(2), v(3))
  }

  def getVars(
      session: Session
  ): (TermStatementVariables, ParamVars, TermRepVars, StatRepVars, TopVars) = {
    (
      ownVars(session),
      paramLayers.getVars(session),
      termRepLayers.getVars(session),
      statementRefLayers.getVars(session),
      preTopLayers.getVars(session)
    )
  }

}

trait ProofScoreLearner[ParamVars, TermRepVars, StatRepVars, TopVars]
    extends ProofScorePredictor[ParamVars, TermRepVars, StatRepVars, TopVars] {
  val trueScore = tf.placeholderWithDefault(tf.constant(0f), Shape.of())

  val loss = tf.math.add(
    tf.math
      .sub(
        tf.math.maximum(prediction, tf.constant(0f)),
        tf.math.mul(prediction, trueScore)
      ),
    tf.math.log(
      tf.math
        .add(tf.constant(1f), tf.math.exp(tf.math.neg(tf.math.abs(prediction))))
    )
  )

  val optimizer = new Adam(graph)

  val minimize = optimizer.minimize(loss)
}

