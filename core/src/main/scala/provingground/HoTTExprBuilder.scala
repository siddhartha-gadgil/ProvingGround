package provingground

trait HoTTExprBuilder[L] {
  def appln(f: L, x: L) : Option[L]
  
  def some(x: L) : Option[L]
  
  def all(x: L) : Option[L]
  
  def const[S](name: S, typ: L) : Option[L]
  
  def isTyp(x: L) : Boolean
  
  def pair: L
}

object HoTTExprBuilder{
  def appln[L : HoTTExprBuilder](f: L, x: L) = implicitly[HoTTExprBuilder[L]].appln(f, x)
  
  def some[L : HoTTExprBuilder](x: L) = implicitly[HoTTExprBuilder[L]].some(x)
  
  def all[L : HoTTExprBuilder](x: L) = implicitly[HoTTExprBuilder[L]].all(x)
  
  def const[S, L : HoTTExprBuilder](name: S, x: L) = implicitly[HoTTExprBuilder[L]].const(name, x)
  
  def lambda[L : HoTTExprBuilder](x: L, y: L) = {
    val b = implicitly[HoTTExprBuilder[L]]
    b.all(x) flatMap (b.appln(_, y))
  }
  
  def pi[L : HoTTExprBuilder](x: L, y: L) = {
    val b = implicitly[HoTTExprBuilder[L]]
    if (b.isTyp(y))  b.all(x) flatMap (b.appln(_, y)) else None
  }
  
  def pair[L : HoTTExprBuilder](x: L, y: L) = {
    val B = implicitly[HoTTExprBuilder[L]]
    for (a <- B.some(x); b <- B.appln(B.pair, a); c <- B.appln(b, y)) yield c
  }
}