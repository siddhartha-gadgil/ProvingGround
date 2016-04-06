package provingground

sealed trait FreeExprLang {
  def as[E](implicit l: ExprLang[E]) : Option[E]
}

object FreeExprLang{
  case class Variable[S](name: S, typ: FreeExprLang) extends FreeExprLang{
    def as[E](implicit l: ExprLang[E]) = 
      for (tp <- typ.as[E]; result <- l.variable(name, tp)) yield result
  }
  
  case class AnonVar(typ: FreeExprLang) extends FreeExprLang{
    def as[E](implicit l: ExprLang[E]) = 
      for (tp <- typ.as[E]; result <- l.anonVar(tp)) yield result
  }
  
  case class MetaVar(typ: FreeExprLang) extends FreeExprLang{
    def as[E](implicit l: ExprLang[E]) = 
      for (tp <- typ.as[E]; result <- l.anonVar(tp)) yield result
  }
  
  case class Incl1(typ: FreeExprLang) extends FreeExprLang{
    def as[E](implicit l: ExprLang[E]) = 
      for (tp <- typ.as[E]; result <- l.incl1(tp)) yield result
  }
  
  case class Incl2(typ: FreeExprLang) extends FreeExprLang{
    def as[E](implicit l: ExprLang[E]) = 
      for (tp <- typ.as[E]; result <- l.incl2(tp)) yield result
  }
  
  case class Proj1(xy: FreeExprLang) extends FreeExprLang{
    def as[E](implicit l: ExprLang[E]) = 
      for (tp <- xy.as[E]; result <- l.proj1(tp)) yield result
  }
  
  case class Proj2(xy: FreeExprLang) extends FreeExprLang{
    def as[E](implicit l: ExprLang[E]) = 
      for (tp <- xy.as[E]; result <- l.proj2(tp)) yield result
  }
  
  case class Domain(typ: FreeExprLang) extends FreeExprLang{
    def as[E](implicit l: ExprLang[E]) = 
      for (tp <- typ.as[E]; result <- l.domain(tp)) yield result
  }
  
  
  case class Lambda(variable: FreeExprLang, value: FreeExprLang)extends FreeExprLang{
    def as[E](implicit l: ExprLang[E]) = 
      for (x <- variable.as[E]; y <- value.as[E]; result <- l.lambda(x, y)) yield result
  }
  
  case class Pi(variable: FreeExprLang, value: FreeExprLang)extends FreeExprLang{
    def as[E](implicit l: ExprLang[E]) = 
      for (x <- variable.as[E]; y <- value.as[E]; result <- l.pi(x, y)) yield result
  }
  
  case class Appln(func: FreeExprLang, arg: FreeExprLang)extends FreeExprLang{
    def as[E](implicit l: ExprLang[E]) = 
      for (x <- func.as[E]; y <- arg.as[E]; result <- l.appln(x, y)) yield result
  }
  
  case class Equality(lhs: FreeExprLang, rhs: FreeExprLang)extends FreeExprLang{
    def as[E](implicit l: ExprLang[E]) = 
      for (x <- lhs.as[E]; y <- rhs.as[E]; result <- l.equality(x, y)) yield result
  }
  
  case class Sigma(variable: FreeExprLang, value: FreeExprLang)extends FreeExprLang{
    def as[E](implicit l: ExprLang[E]) = 
      for (x <- variable.as[E]; y <- value.as[E]; result <- l.sigma(x, y)) yield result
  }
  
  case class Pair(first: FreeExprLang, second: FreeExprLang)extends FreeExprLang{
    def as[E](implicit l: ExprLang[E]) = 
      for (x <- first.as[E]; y <- second.as[E]; result <- l.pair(x, y)) yield result
  }
  
  case class OrCases(first: FreeExprLang, second: FreeExprLang)extends FreeExprLang{
    def as[E](implicit l: ExprLang[E]) = 
      for (x <- first.as[E]; y <- second.as[E]; result <- l.orCases(x, y)) yield result
  }
  
  case class Or(first: FreeExprLang, second: FreeExprLang)extends FreeExprLang{
    def as[E](implicit l: ExprLang[E]) = 
      for (x <- first.as[E]; y <- second.as[E]; result <- l.or(x, y)) yield result
  }
  
  case object TT extends FreeExprLang{
    def as[E](implicit l: ExprLang[E]) = l.tt
  }
  
  case object FF extends FreeExprLang{
    def as[E](implicit l: ExprLang[E]) = l.ff
  }
  
  case object QED extends FreeExprLang{
    def as[E](implicit l: ExprLang[E]) = l.qed
  }
  
  case class Numeral(n: Int) extends FreeExprLang{
    def as[E](implicit l: ExprLang[E]) = l.numeral(n)
  }
  implicit object FreeLang extends ExprLang[FreeExprLang]{
    def variable[S](name: S, typ: FreeExprLang): Option[FreeExprLang] = Some(Variable(name, typ))

  /**
   * anonymous variable
   */
  def anonVar(typ: FreeExprLang): Option[FreeExprLang] = Some (AnonVar(typ))

  /**
   * meta-variable of a given type, i.e., whose value must be inferred 
   * (elaborated in lean's terminology). 
   */
  def metaVar(typ: FreeExprLang): Option[FreeExprLang] = Some(MetaVar(typ))
  
  def lambda(variable: FreeExprLang, value: FreeExprLang) : Option[FreeExprLang] = Some(Lambda(variable, value))

  def pi(variable: FreeExprLang, typ: FreeExprLang): Option[FreeExprLang] =Some(Pi(variable, typ))

  def appln(func: FreeExprLang, arg: FreeExprLang): Option[FreeExprLang] = Some(Appln(func, arg))

  def equality(lhs: FreeExprLang, rhs: FreeExprLang) : Option[FreeExprLang] = Some(Equality(lhs, rhs))
  
  def sigma(variable: FreeExprLang, typFamily: FreeExprLang) : Option[FreeExprLang] = Some(Sigma(variable, typFamily))
  
  def pair (x: FreeExprLang, y: FreeExprLang): Option[FreeExprLang] = Some(Pair(x, y))

  def proj1(xy: FreeExprLang): Option[FreeExprLang] = Some(Proj1(xy))

  def proj2(xy: FreeExprLang): Option[FreeExprLang] = Some(Proj2(xy))

  def or(first: FreeExprLang, second: FreeExprLang):  Option[FreeExprLang] = Some(Or(first, second))

  def incl1(typ : FreeExprLang) : Option[FreeExprLang] = Some(Incl1(typ))

  def incl2(typ: FreeExprLang) :  Option[FreeExprLang] = Some(Incl2(typ))

  /**
   * true type
   */
  def tt : Option[FreeExprLang] = Some(TT)

  /**
   * element of true type
   */
  def qed : Option[FreeExprLang] = Some(QED)

  /**
   * false type
   */
  def ff : Option[FreeExprLang] = Some(FF)
  
  def orCases(first: FreeExprLang, second: FreeExprLang) : Option[FreeExprLang] = Some(OrCases(first, second))

  def numeral(n: Int): Option[FreeExprLang] = Some(Numeral(n))
  
  def isPair: FreeExprLang => Option[(FreeExprLang, FreeExprLang)] = {
    case Pair(first, second) => Some(first, second)
    case _ => None
  }
  
  def isSigma: FreeExprLang => Option[(FreeExprLang, FreeExprLang)]= {
    case Sigma(first, second) => Some(first, second)
    case _ => None
  }
  
  def isPi : FreeExprLang => Option[(FreeExprLang, FreeExprLang)] = {
    case Pi(first, second) => Some(first, second)
    case _ => None
  }
  
  def domain: FreeExprLang => Option[FreeExprLang] = (typ) => Some(Domain(typ))
  }
}