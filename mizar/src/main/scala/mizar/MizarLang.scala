package mizar

import fastparse.all._

object MizarLang {

  case class Identifier(name: String)
  
  case class FileName(name: String)
  
  case class Symbol(name: String)

  
  
  case class Article(environ: EnvironmentDeclaration, text: TextProper)

  type EnvironmentDeclaration = List[Directive]
  
  case class TextProper(sections : List[Section]) extends AnyVal
  
  sealed trait Directive{
    val files : List[FileName]
  }
  
  case class VocabularyDirective(files: List[FileName]) extends Directive
  
  case class RequirementDirective(files: List[FileName]) extends Directive
  
  sealed trait LibraryDirective extends Directive
  
  case class Notation(files: List[FileName]) extends LibraryDirective
  
  case class Constructors(files: List[FileName]) extends LibraryDirective
  
  case class Registrations(files: List[FileName]) extends LibraryDirective
  
  case class Expansions(files: List[FileName]) extends LibraryDirective

  case class Equalities(files: List[FileName]) extends LibraryDirective
  
  case class Definitions(files: List[FileName]) extends LibraryDirective
  
  case class Theorems(files: List[FileName]) extends LibraryDirective
  
  case class Schemes(files: List[FileName]) extends LibraryDirective  

  
  
  
  type Section = List[TextItem]
  
  sealed trait TextItem

  case class Reservation(segements: List[ReservationSegment]) extends TextItem
  
  case class ReservationSegment(
      reservedIdentifiers: List[Identifier], 
      typ : TypeExpression)
  
  
  sealed trait TermExpression    
      
  sealed trait TypeExpression
  
  sealed trait ModeSymbol
  
  case class ModeSym(sym: Symbol) extends ModeSymbol
  
  case object setSymbol extends ModeSymbol
  
  sealed trait RadixType
  
  case class ModeRadixType(
      mode: ModeSymbol,
      l: List[TermExpression]) extends RadixType
      
  case class StrucRadixType(
      structureSymbol: Symbol,
      l : List[TermExpression]) extends RadixType
  
  
  case class PrivateDefinition(param: Char) extends TermExpression    
      
      
  val p = P("a".!)
  


  
}