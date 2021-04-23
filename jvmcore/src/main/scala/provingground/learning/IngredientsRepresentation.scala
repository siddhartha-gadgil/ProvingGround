package provingground.learning

import org.tensorflow._
import org.tensorflow.op._, linalg.MatMul
import org.tensorflow.types._
import scala.util.Using
import org.tensorflow.ndarray._
import org.tensorflow.framework.optimizers._
import IngredientsRepresentation._
import provingground._, HoTT._

object IngredientsRepresentation {
  def dataLookup(v: Operand[TFloat32], sess: Session): TFloat32 = {
    val result = sess.runner().fetch(v).run()
    val data   = result.get(0).asInstanceOf[TFloat32]
    data
  }

  val rnd = new scala.util.Random()
}

class IngredientsRepresentation(numPoints: Int, graph: Graph, dim: Int) {
  val tf = Ops.create(graph)

  val ones = tf.constant(Array.fill(numPoints)(1.0f))

  val vertexEmbed = tf.variable(
    tf.constant(Array.fill(dim, numPoints)(rnd.nextFloat() * 2.0f))
  )

  val contextEmbed = tf.variable(
    tf.constant(Array.fill(dim, numPoints)(rnd.nextFloat() * 2.0f))
  )

  val dotProds = tf.linalg.matMul(
    vertexEmbed,
    contextEmbed,
    MatMul.transposeA(true).transposeB(false)
  )

  val incidence = tf.placeholderWithDefault(
    tf.constant(Array.fill(numPoints, numPoints)(1f)),
    Shape.of(numPoints, numPoints)
  )

  // max(x, 0) - x * z + log(1 + exp(-abs(x)))
  val loss = tf.math.add(
    tf.math
      .sub(
        tf.math.maximum(dotProds, tf.constant(0f)),
        tf.math.mul(dotProds, incidence)
      ),
    tf.math.log(
      tf.math
        .add(tf.constant(1f), tf.math.exp(tf.math.neg(tf.math.abs(dotProds))))
    )
  )

  val optimizer = new Adam(graph)

  val minimize = optimizer.minimize(loss)

  def fit(inc: Array[Array[Float]], steps: Int = 40000) = {
    Using(new Session(graph)) { session =>
      session.run(tf.init())
      println("initialized")
      val incT = TFloat32.tensorOf(StdArrays.ndCopyOf(inc))
      println("Tuning")
      (1 to steps).foreach { j =>
        val tData = session
          .runner()
          .feed(incidence, incT)
          .addTarget(minimize)
          .run()
      }
      JvmUtils.logger.info("Tuning complete")
      val tundedData = session
        .runner()
        .feed(incidence, incT)
        .fetch(loss)
        .run()
        .get(0)
        .asInstanceOf[TFloat32]

      JvmUtils.logger.info(tundedData.getFloat().toString())

      val tData = session
        .runner()
        .fetch(vertexEmbed)
        .fetch(contextEmbed)
        .run()
      val vd = tData.get(0).asInstanceOf[TFloat32]
      val cd = tData.get(1).asInstanceOf[TFloat32]
      val vEmbedding = Vector.tabulate(dim, numPoints) {
        case (i, j) => vd.getFloat(i, j)
      }
      val cEmbedding = Vector.tabulate(dim, numPoints) {
        case (i, j) => cd.getFloat(i, j)
      }
      (vEmbedding ++ cEmbedding)
    }
  }

}
