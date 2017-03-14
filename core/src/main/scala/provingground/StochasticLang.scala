package provingground

import provingground.{TruncatedDistribution => TD, TruncatedDistributionLang => TDL}

class StochasticLang[E : ExprLang : Domain : ExprPatterns](
    baseWeight: Double = 1.0,
    flipWeight: Double = 0.0,
    argShiftWeight: Double = 0.0,
    conversions: TD[E => Option[E]])
    extends TruncatedDistributionLang[E] { self =>

  override def appln(func: TD[E], arg: TD[E]) = {
    val base = new TDL[E]
    val bd = new TruncatedDistributionDomain
    val be = new TruncatedDistributionExprPatterns
    val oc = new ExprApplnOps(self.appln)(base, bd, be)
    val withOps = for (b <- oc.base(func, arg);
    f <- oc.flip(func, arg);
    s <- oc.shiftArg(func, arg)) yield
    (b <*> baseWeight <+> (f <*> flipWeight) <+> (s <*> argShiftWeight))
    val tdConversions =
      conversions map ((cnv) => (td: TD[E]) => TD.optF(td map (cnv)))
    val convertedTDTD = oc.convert(tdConversions)(func, arg)
    val convertedOpt = convertedTDTD map (TD.flatten[E])
    val withConversions =
      convertedOpt flatMap ((c) => withOps map ((wo) => wo <+> c))
    (withConversions orElse withOps): Option[TD[E]]
  }
}
