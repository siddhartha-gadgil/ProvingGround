package compact_enumeration

/**
 * @author gadgil
 */
class FunctionCombinator[F](add: (F, F) => F, sum: F => Option[(F, F)],
  mult: (F, F) => F, prod: F => Option[(F, F)],
  div: (F, F) => F, quot: F => Option[(F, F)],
  compose: (F, F) => F, composition: F => Option[(F, F)], 
  const: Double => F) {
  implicit class FnOp(fn: F){
    def + (that: F) = add(fn, that)
    
    def *(that: F) = mult(fn, that)
    
    def /(that : F) = div(fn, that)
    
    def -(that : F) = this + (const(0.0) * that)
    
    def unary_- = const(0.0) - fn
    
    def apply(that: F) = compose(fn, that)
    
    def circ(that : F) = compose(fn, that)
  }
  
  def derivative(table : F => Option[F]) : F =>  Option[F] = {
    def sumrule(g: F) = for ((x, y) <- sum(g); a <- derivative(table)(x); b<- derivative(table)(y)) yield (a + b)
    
    def leibnitz(g: F) = for ((x, y) <- prod(g); a <- derivative(table)(x); b<- derivative(table)(y)) yield (a * y + b * x)
    
    def quotient(g: F) = for ((x, y) <- prod(g); a <- derivative(table)(x); b<- derivative(table)(y)) yield 
      (a * y - b * x)/ (y * y)
    
    def chain(g: F) = for ((x, y) <- composition(g); a <- derivative(table)(x); b<- derivative(table)(y)) 
      yield (b * (a circ y))
    
    (f: F) => table(f) orElse sumrule(f) orElse leibnitz(f) orElse chain(f)
  }
  
}