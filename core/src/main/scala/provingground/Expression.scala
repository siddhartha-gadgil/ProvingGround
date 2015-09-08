package provingground

import HoTT._

import upickle.default._

/**
 * @author gadgil
 */
sealed trait TermExpr{
    def asTerm(implicit lp: LiteralParser): Term
    
    def asTyp(implicit lp: LiteralParser) : Typ[Term] = asTerm match {
      case tp : Typ[_] => tp
      case _ =>         
        throw(new IllegalArgumentException(s"expected type but found $asTerm"))      
    }
    
  }

  
  
  /**
   * TermExpr a = b
   */
  case class Equality(dom: TermExpr, lhs: TermExpr, rhs: TermExpr) extends TermExpr{
    def asTerm(implicit lp: LiteralParser) = IdentityTyp(dom.asTyp, lhs.asTerm, rhs.asTerm)
  }
  
 

  
  /**
   * Symbolic variable with given type
   */
  case class TypedVar(name: String, typ: TermExpr) extends TermExpr{
    def asTerm(implicit lp: LiteralParser): Term = typ.asTyp.symbObj(name)
  }
  
  case class TypedLiteral(lit: String, typ: TermExpr) extends TermExpr{
    def asTerm(implicit lp: LiteralParser) = lp(typ.asTyp)(lit)
  }
  
  
  
  /**
   * LambdaTermExpr maps to a lambda
   */
  case class LambdaExp(x : TermExpr, y: TermExpr) extends TermExpr{
    def asTerm(implicit lp: LiteralParser) =
      lambda(x.asTerm)(y.asTerm)    
    }


  case class PairExp(first: TermExpr, second: TermExpr) extends TermExpr{
    def asTerm(implicit lp: LiteralParser) = mkPair(first.asTerm, second.asTerm)
  }
  

  
  /**
   * expression func(arg)
   */
  case class Apply(func: TermExpr, arg: TermExpr) extends TermExpr{
    def asTerm(implicit lp: LiteralParser): Term = fold(func.asTerm)(arg.asTerm)
  }
  

  /**
   * the first universe
   */
  case object U extends TermExpr{
    def asTerm(implicit lp: LiteralParser) = __
  }

  
  /**
   * expression for A -> B
   */
  case class Arrow(lhs: TermExpr, rhs: TermExpr) extends TermExpr{
    def asTerm(implicit lp: LiteralParser): Typ[Term] = lhs.asTyp ->: rhs.asTyp
  }
  
  
  case class SigmaExpr(fiber: TermExpr) extends TermExpr{
    def asTerm(implicit lp: LiteralParser) = fiber.asTerm match{
      case fib : Func[_, _] => 
        {
          val x = fib.dom.obj 
          val fibre =   lmbda(x)(fib(x).asInstanceOf[Typ[Term]])  
          SigmaTyp(fibre)
        }
      case  _ => 
        throw(new IllegalArgumentException("cannot construct Sigma Type with fibre $fiber.asTerm"))   
    }   
    
  }
  
  case class PiExpr(fiber: TermExpr) extends TermExpr{
    def asTerm(implicit lp: LiteralParser) = fiber.asTerm match{
      case fib : Func[_, _] => 
        {
          val x = fib.dom.obj 
          val fibre =   lmbda(x)(fib(x).asInstanceOf[Typ[Term]])  
          PiTyp(fibre)
        }
      case  _ => 
        throw(new IllegalArgumentException("cannot construct Pi Type with fibre $fiber.asTerm"))   
    }   
    
  }
  
  case class PlusTypExpr(first: TermExpr, second: TermExpr) extends TermExpr{
    def asTerm(implicit lp: LiteralParser) = PlusTyp(first.asTyp, second.asTyp)
  }
  
trait LiteralParser{
  def parse(typ: Typ[Term])(lit: String) : Option[Term]
  
  def apply(typ: Typ[Term])(lit: String) = parse(typ)(lit).get
}




object TermExpr{
  def pickle(expr: TermExpr) = write(expr)
  
  def unpickle(str: String) = read[TermExpr](str)
}