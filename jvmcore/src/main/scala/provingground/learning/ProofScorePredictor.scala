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

object ProofScorePredictor {
  val rnd = new scala.util.Random()
}

trait ProofScorePredictor {
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

  lazy val statementProbFan = paramFanOut

  lazy val termProbFan = paramFanOut

  lazy val termProofScoreFan = paramFanOut

  lazy val termScoreInStatementFan = paramFanOut

  lazy val paramMergedVector =
    (statementProbFan * statementProb) +
      (termProbFan * termProb) +
      (termProofScoreFan * termProofScore) +
      (termScoreInStatementFan + termScoreInStatement)

  val paramLayers: TFLayers

  val termRepLayers: TFLayers

  val statementRefLayers: TFLayers

  val mergedVector = paramLayers.output(paramMergedVector) + termRepLayers
    .output(termRepresentation) + statementRefLayers.output(
    statementRepresentation
  )

  // output except the final sigmoid
  val preTopLayers: TFLayers

  val prediction = tf.reshape(
    preTopLayers.output(mergedVector),
    tf.constant(Array.emptyIntArray)
  )

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

  def runnerWithData(data: TermStatementParameters, session: Session): Session#Runner =
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

}

abstract class TFLayers(implicit tf: Ops) {
  def output(input: Operand[TFloat32]): Operand[TFloat32]

  def |(that: TFLayers) = TFLayers.Composition(this, that)
}

object TFLayers {
  case class Composition(first: TFLayers, second: TFLayers)(implicit tf: Ops)
      extends TFLayers {
    def output(input: Operand[TFloat32]): Operand[TFloat32] =
      first.output(second.output(input))
  }

  class LinearLayer(inDim: Int, outDim: Int)(implicit tf: Ops)
      extends TFLayers {
    val matrix = tf.variable(
      tf.constant(
        Array.fill(outDim, inDim)(rnd.nextGaussian().toFloat / inDim.toFloat)
      )
    )

    def output(input: Operand[TFloat32]): Operand[TFloat32] =
      tf.reshape(
        tf.linalg
          .matMul(matrix, tf.reshape(input, tf.constant(Array(inDim, 1)))),
        tf.constant(Array(outDim))
      )
  }

  class SigmoidLayer(inDim: Int, outDim: Int)(implicit tf: Ops)
      extends TFLayers {
    val matrix = tf.variable(
      tf.constant(
        Array.fill(outDim, inDim)(rnd.nextGaussian().toFloat / inDim.toFloat)
      )
    )

    def output(input: Operand[TFloat32]): Operand[TFloat32] =
      tf.math.sigmoid(
        tf.reshape(
          tf.linalg
            .matMul(matrix, tf.reshape(input, tf.constant(Array(inDim, 1)))),
          tf.constant(Array(outDim))
        )
      )
  }

  def sigmoidStack(dims: Int*)(implicit tf: Ops): TFLayers = {
    val dimVec = dims.toVector
    val layers = dimVec.zip(dimVec.tail).map {
      case (inDim, outDim) => new SigmoidLayer(inDim, outDim)
    }
    layers.reduce[TFLayers](_ | _)
  }
}
