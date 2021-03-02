package provingground.induction.coarse

import provingground._, HoTT._

trait Curry[Iter <: Term with Subs[Term],
            Total <: Term with Subs[Total],
            Cod <: Term with Subs[Cod]] {
  def curry(fn: Func[Total, Cod]): Iter

  def unCurry(cfn: Iter): Func[Total, Cod]
}

object Curry {
  implicit def idCurry[D <: Term with Subs[D], C <: Term with Subs[C]]
    : Curry[Func[D, C], D, C] =
    new Curry[Func[D, C], D, C] {
      def curry(fn: Func[D, C]) = fn

      def unCurry(cfn: Func[D, C]) = cfn
    }

  implicit def funcCurry[Dom <: Term with Subs[Dom],
                         Iter <: Term with Subs[Iter],
                         Total <: Term with Subs[Total],
                         Cod <: Term with Subs[Cod]](
      implicit base: Curry[Iter, Total, Cod])
    : Curry[Func[Dom, Iter], PairTerm[Dom, Total], Cod] =
    new Curry[Func[Dom, Iter], PairTerm[Dom, Total], Cod] {

      def curry(fn: Func[PairTerm[Dom, Total], Cod]) = {
        val xy     = fn.dom.Var
        val (x, y) = (xy.first, xy.second)
        lmbda(x) {
          base.curry(lmbda(y) { fn(xy) })
        }
      }

      def unCurry(cfn: Func[Dom, Iter]) = {
        val x  = cfn.dom.Var
        val z  = base.unCurry(cfn(x))
        val y  = z.dom.Var
        val xy = PairTerm(x, y)
        lmbda(xy)(z(y))
      }
    }
}
