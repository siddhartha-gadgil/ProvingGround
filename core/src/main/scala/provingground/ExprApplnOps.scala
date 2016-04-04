package provingground

class ExprApplnOps[E](appln : => (E, E) => Option[E])(implicit l : ExprLang[E]) {
  
  def base(f: E, x: E) = l.appln(f, x)
  
  def flip(f: E, x: E) = l.appln(x, f)
  
  def shiftArg(f: E, x: E) =
    for (
        d <- l.domain(f); 
        y <-l.anonVar(d); 
        fy <- appln(f, y); 
        g <-l.lambda(y, fy);
        fyx <- appln(g, x)
        ) yield fyx
}