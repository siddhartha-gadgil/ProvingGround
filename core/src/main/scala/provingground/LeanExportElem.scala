package provingground

sealed trait LeanExportElem

object LeanExportElem {
  sealed trait Name extends LeanExportElem
  
  object Name {
    case object anonymous extends Name
    
    case class NameString(env: Name, name: String) extends Name

    case class NameInt(env: Name, number: Int) extends Name
  }
  
  sealed trait Univ extends LeanExportElem
  
  object Univ{
    case class Succ(base: Univ) extends Univ
    
    case class Max(first: Univ, second: Univ) extends Univ
    
    case class IMax(first: Univ, second: Univ) extends Univ
    
    case class Param(name: Name) extends Univ
    
    case class Global(name: Name) extends Univ
  }
  
  sealed trait Info
  
  object Info{
    case object BD extends Info
    
    case object BI extends Info
    
    case object BS extends Info
    
    case object BC extends Info
  }
  
  sealed trait Expr extends LeanExportElem
  
  object Expr{
    case class Var(index: Int) extends Expr
    
    case class Const(univ: Univ) extends Expr
    
    case class Appln(func: Expr, arg: Expr) extends Expr
    
    case class Lambda(info: Info, varName : Name, variable: Expr, value : Expr) extends Expr
    
    case class Pi(info: Info, varName : Name, variable: Expr, value : Expr) extends Expr
  }
  
  
  sealed trait Import extends LeanExportElem
  
  object Import{
    case class Direct(file: Name) extends Import
    
    case class Relative(backtrack: Int, file: Name) extends Import
  }
  
  case class GlobalUniv(name: Name) extends LeanExportElem
  
  case class Definition(name: Name, univParams: List[Name] = List(), tpe: Expr, value: Expr) extends LeanExportElem
 
  case class Axiom(name: Name, univParams: List[Name] = List(), tpe: Expr) extends LeanExportElem
  
  case class Bind(numParam: Int, numTypes: Int, univParams: List[Int]) extends LeanExportElem
  
  case object Eind extends LeanExportElem
  
  case class Ind(name: Name, tpe: Expr) extends LeanExportElem
  
  case class Intro(name: Name, tpe: Expr) extends LeanExportElem
}