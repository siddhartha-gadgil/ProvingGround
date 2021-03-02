package provingground.learning

import org.tensorflow._, org.tensorflow.op._, types._
import provingground.learning.Expression._

case class TensorflowExpressions(
    tf: Ops,
    init: PartialFunction[Expression, Operand[TFloat32]]
) {
  def operand(
      expr: Expression
  ): Operand[TFloat32] = expr match {
    case fv @ FinalVal(variable)   => init(fv)
    case iv @ InitialVal(variable) => init(iv)
    case Log(exp)                  => tf.math.log(operand(exp))
    case Exp(exp)                  => tf.math.log(operand(exp))
    case Expression.Sum(xs) =>
      xs.map(x => operand(x)).reduce(tf.sum(_, _))
    case Product(x, y)         => tf.math.mul(operand(x), operand(y))
    case Literal(value)        => tf.constant(value.toFloat)
    case Quotient(x, y)        => tf.math.div(operand(x), operand(y))
    case cf @ Coeff(node)      => init(cf)
    case isc @ IsleScale(boat) => init(isc)
  }

  def apply(expr: Expression): Operand[TFloat32] = operand(expr)
}
