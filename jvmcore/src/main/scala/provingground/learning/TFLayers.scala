package provingground.learning

import org.tensorflow._, framework.activations._
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

trait TFVars[V] {
  def getVars(session: Session): V
}
abstract class TFLayers[V](implicit tf: Ops) extends TFVars[V] {
  val inDim: Int

  val outDim: Int

  def output(input: Operand[TFloat32]): Operand[TFloat32]

  def pipe[X](that: TFLayers[X]) = TFLayers.Composition(this, that)
  def |[X](that: TFLayers[X])    = pipe(that)

  def stackSigmoid(
      out: Int
  )(varInit: Array[Array[Float]] = TFLayers.randomMatix(outDim, out)) =
    this pipe (new TFLayers.SigmoidLayer(outDim, out)(varInit))

  def stackReLU(
      out: Int
  )(varInit: Array[Array[Float]] = TFLayers.randomMatix(outDim, out)) =
    this pipe (new TFLayers.ReLULayer(outDim, out)(varInit))

}

object TFLayers {
  def randomMatix(inDim: Int, outDim: Int): Array[Array[Float]] =
    Array.fill(outDim, inDim)(rnd.nextGaussian().toFloat / inDim.toFloat)

  def randomArray(dim: Int) = Array.fill(dim)(rnd.nextGaussian().toFloat)

  def getMatrix(inDim: Int, outDim: Int)(data: TFloat32) =
    Array.tabulate(outDim, inDim) { case (i, j) => data.getFloat(i, j) }

  case class Composition[X, Y](first: TFLayers[X], second: TFLayers[Y])(
      implicit tf: Ops
  ) extends TFLayers[(X, Y)] {
    val inDim: Int = first.inDim

    val outDim: Int = second.outDim

    def getVars(session: Session): (X, Y) =
      (first.getVars(session), second.getVars(session))

    def output(input: Operand[TFloat32]): Operand[TFloat32] =
      first.output(second.output(input))
  }

  class LinearLayer(val inDim: Int, val outDim: Int)(
      varInit: Array[Array[Float]] = randomMatix(inDim, outDim)
  )(implicit tf: Ops)
      extends TFLayers[Array[Array[Float]]] {
    val matrix: core.Variable[TFloat32] = tf.variable(
      tf.constant(
        varInit
      )
    )

    def getVars(session: Session): Array[Array[Float]] = {
      val data =
        session.runner().fetch(matrix).run().get(0).asInstanceOf[TFloat32]
      getMatrix(inDim, outDim)(data)
    }

    def output(input: Operand[TFloat32]): Operand[TFloat32] =
      tf.reshape(
        tf.linalg
          .matMul(matrix, input),
        tf.constant(Array(outDim))
      )
  }

  class SigmoidLayer(val inDim: Int, val outDim: Int)(
      varInit: Array[Array[Float]] = randomMatix(inDim, outDim)
  )(implicit tf: Ops)
      extends TFLayers[Array[Array[Float]]] {
    val matrix: core.Variable[TFloat32] = tf.variable(
      tf.constant(
        varInit
      )
    )

    def getVars(session: Session): Array[Array[Float]] = {
      val data =
        session.runner().fetch(matrix).run().get(0).asInstanceOf[TFloat32]
      getMatrix(inDim, outDim)(data)
    }

    def output(input: Operand[TFloat32]): Operand[TFloat32] =
      tf.math.sigmoid(
        tf.reshape(
          tf.linalg
            .matMul(matrix, input),
          tf.constant(Array(outDim))
        )
      )
  }

  class ReLULayer(val inDim: Int, val outDim: Int)(
      varInit: Array[Array[Float]] = randomMatix(inDim, outDim)
  )(implicit tf: Ops)
      extends TFLayers[Array[Array[Float]]] {
    val matrix: core.Variable[TFloat32] = tf.variable(
      tf.constant(
        varInit
      )
    )

    def getVars(session: Session): Array[Array[Float]] = {
      val data =
        session.runner().fetch(matrix).run().get(0).asInstanceOf[TFloat32]
      getMatrix(inDim, outDim)(data)
    }

    val  relu = new ReLU[TFloat32](tf)
    def output(input: Operand[TFloat32]): Operand[TFloat32] =
      relu.call(
        tf.reshape(
          tf.linalg
            .matMul(matrix, input),
          tf.constant(Array(outDim))
        )
      )
  }

  
}

trait SigmoidStacker[Sizes, Vars] {
  def build(sizes: Sizes)(implicit tf: Ops): TFLayers[Vars]

  def buildWithData(sizes: Sizes, data: Vars)(implicit tf: Ops): TFLayers[Vars]
}

object SigmoidStacker {
  def stack[Sizes, Vars](
      sizes: Sizes
  )(implicit stacker: SigmoidStacker[Sizes, Vars], tf: Ops): TFLayers[Vars] =
    stacker.build(sizes)

  def stackWithData[Sizes, Vars](
      sizes: Sizes,
      data: Vars
  )(implicit stacker: SigmoidStacker[Sizes, Vars], tf: Ops): TFLayers[Vars] =
    stacker.buildWithData(sizes, data)

  implicit val pairStacker: SigmoidStacker[(Int, Int), Array[Array[Float]]] =
    new SigmoidStacker[(Int, Int), Array[Array[Float]]] {
      def build(
          sizes: (Int, Int)
      )(implicit tf: Ops): TFLayers[Array[Array[Float]]] =
        new TFLayers.SigmoidLayer(sizes._1, sizes._2)()

      def buildWithData(sizes: (Int, Int), data: Array[Array[Float]])(
          implicit tf: Ops
      ): TFLayers[Array[Array[Float]]] =
        new TFLayers.SigmoidLayer(sizes._1, sizes._2)(data)
    }

  def eg1(implicit tf: Ops): TFLayers[Array[Array[Float]]] = stack(2 -> 3)

  implicit def appendStacker[InitSizes, InitVars](
      implicit
      init: SigmoidStacker[InitSizes, InitVars]
  ): SigmoidStacker[(InitSizes, Int), (InitVars, Array[Array[Float]])] =
    new SigmoidStacker[(InitSizes, Int), (InitVars, Array[Array[Float]])] {
      def build(sizes: (InitSizes, Int))(
          implicit tf: Ops
      ): TFLayers[(InitVars, Array[Array[Float]])] =
        init.build(sizes._1).stackSigmoid(sizes._2)()

      def buildWithData(
          sizes: (InitSizes, Int),
          data: (InitVars, Array[Array[Float]])
      )(implicit tf: Ops): TFLayers[(InitVars, Array[Array[Float]])] =
        init.buildWithData(sizes._1, data._1).stackSigmoid(sizes._2)(data._2)
    }

  def eg2(
      implicit tf: Ops
  ): TFLayers[(Array[Array[Float]], Array[Array[Float]])] = stack((2 -> 3) -> 5)

  def eg3(
      implicit tf: Ops
  ): TFLayers[((Array[Array[Float]], Array[Array[Float]]), Array[Array[Float]])] = 
  stack(1 -> 3 -> 4 -> 5)

}

trait ReLUStacker[Sizes, Vars] {
  def build(sizes: Sizes)(implicit tf: Ops): TFLayers[Vars]

  def buildWithData(sizes: Sizes, data: Vars)(implicit tf: Ops): TFLayers[Vars]
}

object ReLUStacker {
  def stack[Sizes, Vars](
      sizes: Sizes
  )(implicit stacker: ReLUStacker[Sizes, Vars], tf: Ops): TFLayers[Vars] =
    stacker.build(sizes)

  def stackWithData[Sizes, Vars](
      sizes: Sizes,
      data: Vars
  )(implicit stacker: ReLUStacker[Sizes, Vars], tf: Ops): TFLayers[Vars] =
    stacker.buildWithData(sizes, data)

  implicit val pairStacker: ReLUStacker[(Int, Int), Array[Array[Float]]] =
    new ReLUStacker[(Int, Int), Array[Array[Float]]] {
      def build(
          sizes: (Int, Int)
      )(implicit tf: Ops): TFLayers[Array[Array[Float]]] =
        new TFLayers.ReLULayer(sizes._1, sizes._2)()

      def buildWithData(sizes: (Int, Int), data: Array[Array[Float]])(
          implicit tf: Ops
      ): TFLayers[Array[Array[Float]]] =
        new TFLayers.ReLULayer(sizes._1, sizes._2)(data)
    }

  def eg1(implicit tf: Ops): TFLayers[Array[Array[Float]]] = stack(2 -> 3)

  implicit def appendStacker[InitSizes, InitVars](
      implicit
      init: ReLUStacker[InitSizes, InitVars]
  ): ReLUStacker[(InitSizes, Int), (InitVars, Array[Array[Float]])] =
    new ReLUStacker[(InitSizes, Int), (InitVars, Array[Array[Float]])] {
      def build(sizes: (InitSizes, Int))(
          implicit tf: Ops
      ): TFLayers[(InitVars, Array[Array[Float]])] =
        init.build(sizes._1).stackReLU(sizes._2)()

      def buildWithData(
          sizes: (InitSizes, Int),
          data: (InitVars, Array[Array[Float]])
      )(implicit tf: Ops): TFLayers[(InitVars, Array[Array[Float]])] =
        init.buildWithData(sizes._1, data._1).stackReLU(sizes._2)(data._2)
    }

  def eg2(
      implicit tf: Ops
  ): TFLayers[(Array[Array[Float]], Array[Array[Float]])] = stack((2 -> 3) -> 5)

  def eg3(
      implicit tf: Ops
  ): TFLayers[((Array[Array[Float]], Array[Array[Float]]), Array[Array[Float]])] = 
  stack(1 -> 3 -> 4 -> 5)

}
