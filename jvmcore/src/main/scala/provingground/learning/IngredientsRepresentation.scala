package provingground.learning

import org.tensorflow._
import org.tensorflow.op._
import org.tensorflow.types._
import scala.util.Using
import org.tensorflow.ndarray._
import org.tensorflow.framework.optimizers._
import IngredientsRepresentation._

object IngredientsRepresentation{
    def dataLookup(v: Operand[TFloat32], sess: Session): TFloat32 = {
    val result = sess.runner().fetch(v).run()
    val data = result.get(0).asInstanceOf[TFloat32]
    data
  }

  val rnd = new scala.util.Random()
}

class IngredientsRepresentation(numPoints: Int, graph: Graph, epsilon: Float = 0.01f) {
  val tf = Ops.create(graph)

  val ones = tf.constant(Array.fill(numPoints)(1.0f))

  val xs = tf.variable(
    tf.constant(Array.fill(numPoints)(rnd.nextFloat() * 2.0f))
  )

  val ys = tf.variable(
    tf.constant(Array.fill(numPoints)(rnd.nextFloat() * 2.0f))
  )

  def rankOne(v: Operand[TFloat32], w: Operand[TFloat32]) =
    tf.linalg.matMul(
      tf.reshape(v, tf.constant(Array(numPoints, 1))),
      tf.reshape(w, tf.constant(Array(1, numPoints)))
    )

  val xDiff = tf.math.squaredDifference(rankOne(xs, ones), rankOne(ones, xs))

  val yDiff = tf.math.squaredDifference(rankOne(ys, ones), rankOne(ones, ys))

  val totDiff = tf.math.add(xDiff, yDiff)

  val oneMatrix = tf.constant(Array.fill(numPoints, numPoints)(1.0f))

  val oneEpsMatrix =
    tf.constant(Array.fill(numPoints, numPoints)(1.0f + epsilon))

  val probs = tf.math.div(
    oneMatrix,
    tf.math.add(
      oneEpsMatrix,
      totDiff
    )
  )

  val incidence = tf.placeholder(classOf[TFloat32])

  val loss = tf.math.neg(
    tf.reduceSum(
      (
        tf.math.add(
          tf.math.mul(incidence, tf.math.log(probs)),
          tf.math.mul(
            tf.math.sub(oneMatrix, incidence),
            tf.math.log(tf.math.sub(oneMatrix, probs))
          )
        )
      ),
      tf.constant(Array(0, 1))
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
      println("Tuning complete")
      val tundedData = session
        .runner()
        .feed(incidence, incT)
        .fetch(loss)
        .run()
        .get(0)
        .asInstanceOf[TFloat32]

      println(tundedData.getFloat())
      val txd = dataLookup(xs, session)
      val tyd = dataLookup(ys, session)
      val tpoints =
        (0 until (inc.size)).map(n =>
          (txd.getFloat(n) * 60f, tyd.getFloat(n) * 60f)
        )
    }
  }

}