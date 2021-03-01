package provingground.learning

import org.tensorflow._, ndarray._, types._
import org.tensorflow.op._, core._, math._, linalg._
import org.tensorflow.types._, family.TType
import scala.util.Using
import org.tensorflow.ndarray._
import org.tensorflow.framework.optimizers.{Optimizer, Adam}
import scala.util.Random
import GraphEmbedding._
import scala.util.Try
import provingground.JvmUtils

object GraphEmbedding {
  val rnd: Random = new Random()

  @scala.annotation.tailrec
  def getSample(
      remaining: Vector[Int],
      size: Int,
      accum: Vector[Int] = Vector()
  ): Vector[Int] =
    if (size < 1) accum
    else {
      val pick = remaining(rnd.nextInt(remaining.size))
      getSample(remaining.filterNot(_ == pick), size - 1, accum :+ pick)
    }

}

class GraphEmbeddingLogisitic(
    numPoints: Int,
    batchSize: Int,
    graph: Graph,
    epsilon: Float = 0.01f
) {
  val tf: Ops = Ops.create(graph)

  var stepsRun: Int = 0
  var dataSnap
      : (Vector[(Float, Float)], Vector[((Float, Float), (Float, Float))]) =
    (Vector(), Vector())
  var fitDone: Boolean = false

  JvmUtils.logger.info("graph batch created")

  val ones: Constant[TFloat32] = tf.constant(Array.fill(batchSize)(1.0f))

  val fxs: Variable[TFloat32] = tf.variable(
    tf.constant(Array.fill(numPoints)(rnd.nextFloat() * 2.0f))
  )

  val fys: Variable[TFloat32] = tf.variable(
    tf.constant(Array.fill(numPoints)(rnd.nextFloat() * 2.0f))
  )

  val projection: PlaceholderWithDefault[TFloat32] = tf.placeholderWithDefault(
    tf.constant(Array.fill(batchSize)(Array.fill(numPoints)(1f))),
    ndarray.Shape.of(batchSize, numPoints)
  )

  val xs: MatMul[TFloat32] = {
    tf.linalg.matMul(
      projection,
      tf.reshape(fxs, tf.constant(Array(numPoints, 1)))
    )
  }

  val ys: MatMul[TFloat32] = tf.linalg.matMul(
    projection,
    tf.reshape(fys, tf.constant(Array(numPoints, 1)))
  )

  def rankOne(v: Operand[TFloat32], w: Operand[TFloat32]) = {
    val row = tf.reshape(v, tf.constant(Array(batchSize, 1)))
    tf.linalg.matMul(
      row,
      tf.reshape(w, tf.constant(Array(1, batchSize)))
    )
  }

  val xDiff: SquaredDifference[TFloat32] =
    tf.math.squaredDifference(rankOne(xs, ones), rankOne(ones, xs))

  val yDiff: SquaredDifference[TFloat32] =
    tf.math.squaredDifference(rankOne(ys, ones), rankOne(ones, ys))

  val totDiff: Add[TFloat32] = tf.math.add(xDiff, yDiff)

  val oneMatrix: Constant[TFloat32] =
    tf.constant(Array.fill(batchSize, batchSize)(1.0f))

  val oneEpsMatrix: Constant[TFloat32] =
    tf.constant(Array.fill(batchSize, batchSize)(1.0f + epsilon))

  val probs: Div[TFloat32] = tf.math.div(
    oneMatrix,
    tf.math.add(
      oneEpsMatrix,
      totDiff
    )
  )

  val incidence: Placeholder[TFloat32] = tf.placeholder(TFloat32.DTYPE)

  val loss: Neg[TFloat32] = tf.math.neg(
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

  val optimizer: Adam = new Adam(graph)

  val minimize: Op = optimizer.minimize(loss)

  def fit(
      inc: Int => Int => Float,
      steps: Int = 2000000
  ): Try[Vector[(Float, Float)]] = {
    Using(new Session(graph)) { session =>
      session.run(tf.init())
      JvmUtils.logger.info("initialized")
      JvmUtils.logger.info("Tuning")
      val data = (1 to steps).toVector.map { j =>
        val batch: Array[Int] =
          getSample((0 until (numPoints)).toVector, batchSize).toArray
        val incB = Array.tabulate(batchSize)(
          i => Array.tabulate(batchSize)(j => inc(batch(i))(batch(j)))
        )
        val incT = TFloat32.tensorOf(
          StdArrays.ndCopyOf(
            incB
          )
        )
        val projMat =
          Array.tabulate(batchSize) { i =>
            Array.tabulate(numPoints)(j => if (j == batch(i)) 1f else 0f)
          }
        val projT = TFloat32.tensorOf(
          StdArrays.ndCopyOf(
            projMat
          )
        )
        val tData = Try(
          session
            .runner()
            .feed(incidence, incT)
            .feed(projection, projT)
            .addTarget(minimize)
            .fetch(fxs)
            .fetch(fys)
            .run()
        ).fold(
          fa => {
            JvmUtils.logger.error(fa.getMessage())
            JvmUtils.logger.error(fa.getStackTrace().mkString("\n"))
            throw fa
          },
          identity(_)
        )
        val xd = tData.get(0).expect(TFloat32.DTYPE).data()
        val yd = tData.get(1).expect(TFloat32.DTYPE).data()
        val points: Vector[(Float, Float)] =
          (0 until (numPoints))
            .map(n => (xd.getFloat(n), yd.getFloat(n)))
            .toVector
        val maxX  = points.map(_._1).max
        val maxY  = points.map(_._2).max
        val scale = scala.math.min(300f / maxX, 300f / maxY)
        val scaledPoints = points.map {
          case (x, y) =>
            (x * scale, y * scale)
        }
        val lines = scaledPoints.zip(scaledPoints.tail :+ scaledPoints.head)
        stepsRun = j
        dataSnap = (scaledPoints, lines)
        points
      }
      fitDone = true
      JvmUtils.logger.info("Tuning complete")
      data.last
    }
  }

}
