package provingground

class ExprApplnOps[E](appln: => (E, E) => Option[E])(implicit l: ExprLang[E],
                                                     d: Domain[E],
                                                     ep: ExprPatterns[E]) {

  def base(f: E, x: E) = l.appln(f, x)

  def flip(f: E, x: E) = l.appln(x, f)

  def convert(cs: TruncatedDistribution[E => Option[E]])(f: E, x: E) = {
    val argOptTD = cs map ((g) => g(x))
    val resOpts =
      argOptTD map ((argOpt) => argOpt flatMap ((arg) => appln(f, arg)))
    TruncatedDistribution.optF(resOpts)
  }

  def shiftArg(f: E, x: E) =
    for (d   <- d.domain(f);
         y   <- l.anonVar(d);
         fy  <- appln(f, y);
         g   <- l.lambda(y, fy);
         fyx <- appln(g, x)) yield fyx
}
