package provingground
import provingground.HoTT._
import ScalaRep._
//import scala.reflect.runtime.universe.{Try => UnivTry, Function => FunctionUniv, _}
import IntTypes._
import EnumType._
import BigOps._

object MatrixTypes {
  val X = "X" :: Type
  val Y = "Y" :: Type
  val W = "W" :: Type

  // TODO replace this by recursive records.
  val Zmat = {
    lambda(X)(
      lambda(Y)(X ->: Y ->: Z)
    )
  }

  val A = "A" :: Zmat(X)(Y)

  val B = "B" :: Zmat(Y)(W)

  val C = "C" :: Zmat(X)(Y)

  val basis = "basis" :: EnumTyp(Y)

  val x = "x" :: X

  val y = "y" :: Y

  implicit class Matrix(entries: Func[Term, Func[Term, Term]]) {
    val dom                                   = entries.dom
    val codom                                 = entries.codom
    val typ                                   = Zmat(dom)(codom)
    def +(that: Func[Term, Func[Term, Term]]) = {}
  }

  val ZmatSum = lambda(X)(
    lambda(Y)(
      lambda(A)(lambda(C)(lambda(x)(lambda(y)(Z.sum(A(x)(y))(C(x)(y))))))))

  val w = "w" :: W

  val ZmatProd = {
    lambda(X)(
      lambda(Y)(
        lambda(W)(
          lambda(basis)(lambda(A)(lambda(B)(lambda(x)(
            lambda(w)({
              val f = LambdaFixed(y, Z.prod(A(x)(y))(B(y)(w)))
              BigSum(Y)(basis)(f)
            })
          )))))))
  }

  val ZmatEql = lambda(X)(
    lambda(Y)(
      lambda(A)(lambda(C)(lambda(x)(lambda(y)(Z.eql(A(x)(y))(C(x)(y))))))))
}
