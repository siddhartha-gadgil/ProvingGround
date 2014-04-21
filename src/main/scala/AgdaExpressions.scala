package provingGround

object AgdaExpressions{
  
  /*
   * All Expressions, but not statements or blocks
   */
  trait Expression
  
  /*
   * Statement: Declaration of type, pattern based definitions, postulates
   */
  trait Statement
  
  /*
   * Blocks: definitions of types and functions, postulates etc.
   */
  trait Block
  
  trait Pattern extends Expression
  
  trait PatternList
  
  object PatternList{
    object empty extends PatternList
    
    def apply = empty
  }
  
  trait Variable extends PatternHead with PatternList{
    val name: String
    
    val func = name
  }
  
  case class Var(name: String) extends Variable
  
  case class TypedVar(name: String, typ: String) extends Variable
  
  trait PatternHead extends Pattern{
    val func: String
  }
  
  case class RecPattern(head: PatternHead, tail: Pattern) extends PatternHead{
    val func = head.func
  }
  
  case class PatternCons(head: PatternHead, tail: PatternList) extends PatternList
 
  case class ConstructorDefn(name: String, typ: Pattern) extends Statement
  
  trait Term extends Expression with TypSpec
  
  case class Token(name: String) extends Term
  
  case class Apply(func: Term, arg: Term) extends Term
  
  case class TypedTerm(obj: Term, typ: Term) extends Term
  
  trait TypSpec extends Expression
  
  case class TypName(name: String) extends TypSpec
  
  case object __ extends TypSpec
  
  case class Arrow(lhs: TypSpec, rhs: TypSpec) extends TypSpec
  
  case class DefnEquality(lhs: Pattern, rhs: Term) extends Statement
  
  case class DataTypHeader(name: String, params: TypSpec = __) extends Statement
  
  case class DataTypDefn(header: DataTypHeader, constructors: List[ConstructorDefn]) extends Block
  
  case class DataTypFmlyHeader(name: String, params: TypSpec = __) extends Statement
  
  case class DataTypFmlyDefn(header: DataTypFmlyHeader, constructors: List[ConstructorDefn]) 
  
}