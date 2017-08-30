package provingground.translation

import scala.util.Try

/**
  * A statement obtained from source which affects a document.
  */
trait Statement[E] {
  def appendTo[D](doc: D)(implicit dw: MathWriter[D, E]) =
    dw.append(this, doc)
}

object Statement {
  trait Assertion[E] extends Statement[E] {
    def claim: E
  }

  case class Assert[E](claim: E) extends Assertion[E]

  /**
    * Assertion which we do not expect to conclude immediately.
    */
  class Propn[E](val claim: E) extends Assertion[E]

  case class Conclude[E](claim: E) extends Assertion[E]

  case class Using[E](method: E, claim: E) extends Assertion[E]

  case class Case[E](claim: E) extends Assertion[E]

  case class EndCase[E](beginning: Case[E]) extends Statement[E]

  abstract class Define[S, E: ExprLang](name: S, typ: E)
      extends Statement[E]
      with Assertion[E] {
    lazy val lang = implicitly[ExprLang[E]]

    lazy val variable = lang.variable(name, typ).get
  }

  case class Definition[S, E: ExprLang](name: S, typ: E, value: E)
      extends Define[S, E](name, typ) {
    lazy val claim = lang.equality(variable, value).get
  }

  case class DefnPropn[S, E: ExprLang](name: S, typ: E, claim: E)
      extends Define[S, E](name, typ)

  case class Fix[E](variable: E) extends Statement[E]

  object Fix {
    def apply[S, E: ExprLang](name: S, typ: E): Fix[E] =
      Fix(implicitly[ExprLang[E]].variable(name, typ).get)
  }

  object Assume {
    def apply[E: ExprLang](typ: E): Fix[E] =
      Fix(implicitly[ExprLang[E]].anonVar(typ).get)
  }

  trait Beginning {
    def begin[D, E](doc: D)(implicit dw: MathWriter[D, E]) =
      dw.begin(this, doc)
    def apply[D, E](doc: D)(implicit dw: MathWriter[D, E]) = begin(doc)
  }

  case class End(beginning: Beginning) {
    def end[D, E](doc: D)(implicit dw: MathWriter[D, E]) =
      dw.end(beginning, doc)
    def apply[D, E](doc: D)(implicit dw: MathWriter[D, E]) = end(doc)
  }

  case class BeginProof[E](propn: Assertion[E]) extends Beginning

  class BeginPropn extends Beginning
}

sealed trait Scoped[S] {
  def ::[L](label: L) = Scoped.Cons(label, this)
}

object Scoped {
  case class Outer[S](body: S) extends Scoped[S]

  case class Cons[L, S](label: L, inner: Scoped[S]) extends Scoped[S]
}

/**
  * Type-class for  writing to a document of type D in terms of expressions of type E.
  * We can write statements and begin and end blocks.
  * We append to the document, but the document may come with a cursor that allows insertion instead.
  */
trait MathWriter[D, E] {

  /**
    * appends statement to document
    */
  def append(s: Statement[E], doc: D): D

  def begin(b: Statement.Beginning, doc: D): D

  def end(e: Statement.Beginning, doc: D): D

  /**
    * tries to append, does nothing if it fails;
    * statement is passed by name, so should also wrap statement creation involving get.
    */
  def appendTry(s: => Statement[E], doc: D): D =
    Try(append(s, doc)) getOrElse (doc)
}

object MathWriter {
  implicit def nestedDocWriter[E]: MathWriter[NestedDoc[Statement[E]], E] =
    new MathWriter[NestedDoc[Statement[E]], E] {
      def append(s: Statement[E], doc: NestedDoc[Statement[E]]) =
        doc.append(s)

      def begin(b: Statement.Beginning, doc: NestedDoc[Statement[E]]) =
        doc.begin(b)

      def end(e: Statement.Beginning, doc: NestedDoc[Statement[E]]) =
        doc.end(e)
    }
}

trait MathReader[D, E] {
  def read(doc: D): Vector[Scoped[Statement[E]]]

  def contexts(doc: D): Map[Scoped[Statement[E]], Vector[Scoped[Statement[E]]]]
}

object MathReader {
  implicit def nestedDocReader[E]: MathReader[NestedDoc[Statement[E]], E] =
    new MathReader[NestedDoc[Statement[E]], E] {
      def read(doc: NestedDoc[Statement[E]]) = doc.read

      def contexts(doc: NestedDoc[Statement[E]]) = doc.contexts
    }
}
