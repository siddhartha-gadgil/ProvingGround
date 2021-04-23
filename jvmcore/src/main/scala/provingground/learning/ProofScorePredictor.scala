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
import scala.util.Try
//  The first attempt at predicting scores of ingredients in proofs on statements.

case class TermStatementParameters(
    statementProb: Double,
    termProb: Double,
    termScoreInStatement: Double,
    termProofScore: Double,
    termRep: Seq[Double],
    termRepFuzziness: Double,
    statementRep: Seq[Double],
    statementRepFuzziness: Double
) {
  val statementProbT: TFloat32 = TFloat32.scalarOf(statementProb.toFloat)
  val termProbT: TFloat32      = TFloat32.scalarOf(termProb.toFloat)
  val termScoreInStatementT: TFloat32 =
    TFloat32.scalarOf(termScoreInStatement.toFloat)
  val termProofScoreT: TFloat32 = TFloat32.scalarOf(termProofScore.toFloat)

  val termRecSeq = (termRepFuzziness +: termRep).map(_.toFloat)

  val termRepresentationT: TFloat32 = TFloat32.tensorOf(
    StdArrays.ndCopyOf(termRecSeq.toArray)
  )

  val statRepSeq =
    (statementRepFuzziness +: statementRep).map(_.toFloat)

  val statementRepresentationT: TFloat32 = TFloat32.tensorOf(
    StdArrays
      .ndCopyOf(statRepSeq.toArray)
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
  val batchSize: Int

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

  /**
    * generation probability of type whose inhabitants we seek
    */
  lazy val statementProbBatch: PlaceholderWithDefault[TFloat32] =
    tf.placeholderWithDefault(
      tf.constant(Array.fill(batchSize, 1)(0.0f)),
      Shape.of(batchSize, 1)
    )

  lazy val termProbBatch: PlaceholderWithDefault[TFloat32] =
    tf.placeholderWithDefault(
      tf.constant(Array.fill(batchSize, 1)(0.0f)),
      Shape.of(batchSize, 1)
    )

  lazy val termScoreInStatementBatch: PlaceholderWithDefault[TFloat32] =
    tf.placeholderWithDefault(
      tf.constant(Array.fill(batchSize, 1)(0.0f)),
      Shape.of(batchSize, 1)
    )

  lazy val termProofScoreBatch: PlaceholderWithDefault[TFloat32] =
    tf.placeholderWithDefault(
      tf.constant(Array.fill(batchSize, 1)(0.0f)),
      Shape.of(batchSize, 1)
    )

  // representations, first entry should be the "fuzziness"
  lazy val termRepresentation: PlaceholderWithDefault[TFloat32] =
    tf.placeholderWithDefault(
      tf.constant(Array.fill(representationDimension + 1)(0.0f)),
      Shape.of(representationDimension + 1)
    )

  lazy val statementRepresentation: PlaceholderWithDefault[TFloat32] =
    tf.placeholderWithDefault(
      tf.constant(Array.fill(representationDimension + 1)(0.0f)),
      Shape.of(representationDimension + 1)
    )

  lazy val termRepresentationBatch: PlaceholderWithDefault[TFloat32] =
    tf.placeholderWithDefault(
      tf.constant(Array.fill(representationDimension + 1, batchSize)(0.0f)),
      Shape.of(representationDimension + 1, batchSize)
    )

  lazy val statementRepresentationBatch: PlaceholderWithDefault[TFloat32] =
    tf.placeholderWithDefault(
      tf.constant(Array.fill(representationDimension + 1, batchSize)(0.0f)),
      Shape.of(representationDimension + 1, batchSize)
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

  def row(op: Operand[TFloat32]) = tf.reshape(op, tf.constant(Array(batchSize)))

  lazy val paramMergedSeq =
    (statementProbFan * statementProb) +
      (termProbFan * termProb) +
      (termProofScoreFan * termProofScore) +
      (termScoreInStatementFan + termScoreInStatement)

  lazy val paramMergedMatrix = tf.linalg
    .matMul(row(statementProbFan), statementProbBatch) +
    tf.linalg.matMul(row(termProbFan), termProbBatch) +
    tf.linalg.matMul(row(termProofScoreFan), termProofScoreBatch) +
    tf.linalg.matMul(row(termScoreInStatementFan), termScoreInStatementFan)

  val paramLayers: TFLayers[ParamVars]

  val termRepLayers: TFLayers[TermRepVars]

  val statementRefLayers: TFLayers[StatRepVars]

  val mergedSeq = paramLayers.output(
    tf.reshape(paramMergedSeq, tf.constant(Array(paramLayerDimension, 1)))
  ) + termRepLayers
    .output(termRepresentation) + statementRefLayers.output(
    statementRepresentation
  )

  val mergedMatrix = paramLayers.output(paramMergedMatrix) + termRepLayers
    .output(termRepresentationBatch) + statementRefLayers.output(
    statementRepresentationBatch
  )

  // output except the final sigmoid
  val preTopLayers: TFLayers[TopVars]

  val prediction = tf.reshape(
    preTopLayers.output(mergedSeq),
    tf.constant(Array.emptyIntArray)
  )

  val predictionColumn = preTopLayers.output(mergedMatrix)

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

  def predict(data: TermStatementParameters): Try[Float] =
    Using(new Session(graph)) { session =>
      val tData = runnerWithData(data, session).fetch(prediction).run()
      tData.get(0).asInstanceOf[TFloat32].getFloat()
    }

  def predictAll(
      fullData: Seq[TermStatementParameters]
  ): Try[Seq[Float]] =
    Using(new Session(graph)) { session =>
      fullData.map { data =>
        val tData = runnerWithData(data, session).fetch(prediction).run()
        tData.get(0).asInstanceOf[TFloat32].getFloat()
      }
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

  def fit(data: TermStatementParameters, trueValue: Float) =
    Using(new Session(graph)) { session =>
      val tData = runnerWithData(data, session)
        .feed(trueScore, TFloat32.scalarOf(trueValue))
        .addTarget(minimize)
        .fetch(loss)
        .run()
      tData.get(0).asInstanceOf[TFloat32].getFloat()
    }

  def fitAll(trainingData: Seq[(TermStatementParameters, Float)]) =
    Using(new Session(graph)) { session =>
      trainingData.foreach {
        case (data, trueValue) =>
          runnerWithData(data, session)
            .feed(trueScore, TFloat32.scalarOf(trueValue))
            .addTarget(minimize)
            .run()
      }
    }
}
