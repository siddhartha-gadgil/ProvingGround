package mizar

import fastparse._

import MizarLang._

object Parser {
  val White = WhitespaceApi.Wrapper{
    import fastparse.all._
    NoTrace(CharIn(" \t").rep)
  }
  import fastparse.noApi._
  import White._
  
  val article : P[Article]  = environDecl~testProper map {case (x, y) => Article(x, y)}
  
  val environDecl : P[EnvironmentDeclaration] = ???
  
  val testProper : P[TextProper] = ???
  
}