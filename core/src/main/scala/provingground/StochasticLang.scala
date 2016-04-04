package provingground

import provingground.{TruncatedDistribution => TD, TruncatedDistributionLang => TDL}

class StochasticLang[E: ExprLang](
    baseWeight : Double = 1.0,
    flipWeight : Double = 0.0,
    argShiftWeight : Double = 0.0) extends TruncatedDistributionLang[E] {self =>

  override def appln(func: TD[E], arg: TD[E]) = {
    val base = new TDL[E]
    val oc = new ExprApplnOps(self.appln)(base)
    for (
        b <- oc.base(func, arg);
        f<- oc.flip(func, arg);
        s <- oc.shiftArg(func, arg)) 
      yield (b <*> baseWeight <+> (f <*> flipWeight) <+> (s <*> argShiftWeight)) 
  }
}