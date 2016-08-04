package provingground

import HoTT._

trait Curry[Iter <: Term with Subs[Term], Total <: Term with Subs[Total], Cod <: Term with Subs[Cod]] {
  def curry(fn: Func[Total, Cod]): Iter
  
  def unCurry(cfn: Iter): Func[Total, Cod]
}

object Curry{
  implicit def idCurry[D <: Term with Subs[D], C <: Term with Subs[C]] : Curry[Func[D, C], D, C] = 
    new Curry[Func[D, C], D, C] {
      def curry(fn: Func[D, C]) = fn
  
      def unCurry(cfn: Func[D, C]) = cfn
  }
  
  implicit def funcCurry[
    Dom <: Term with Subs[Dom],
    Iter <: Term with Subs[Iter], 
    Total <: Term with Subs[Total], 
    Cod <: Term with Subs[Cod]](
        implicit curr : Curry[Iter, Total, Cod]
        ) : Curry[Func[Dom, Iter], PairObj[Dom, Total], Cod] = 
  new Curry[Func[Dom, Iter], PairObj[Dom, Total], Cod]{
            
    def curry(fn: Func[PairObj[Dom, Total], Cod]) = ???
            
    def unCurry(cfn: Func[Dom, Iter]) = ???
  }
}