package provingground


import org.tensorflow._
import org.tensorflow.op._, linalg.MatMul
import org.tensorflow.types._
import org.tensorflow.ndarray._

object TensorFlowSyntax {
  implicit class TFOps(op: Operand[TFloat32])(implicit tf: Ops){
      def +(that: Operand[TFloat32]) = tf.math.add(op, that)
      def *(that: Operand[TFloat32]) = tf.math.mul(op, that)
      def -(that: Operand[TFloat32]) = tf.math.sub(op, that)
      def /(that: Operand[TFloat32]) = tf.math.div(op, that)

      def unary_- = tf.math.neg(op)
  }
}
