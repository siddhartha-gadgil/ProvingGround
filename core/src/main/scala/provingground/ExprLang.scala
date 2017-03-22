package provingground
import scala.util.Try

/**
  * Abstraction (as a type class) of a language for mathematical expressions, i.e., of
  *
  * * methods for forming expressions,
  * * some basic expressions such as true, false, numbers etc.
  *
  * Expression include functions and  propositions.
  * However, expressions here need not include sophisticated, and foundation specific, constructions such as inductive types;
  * these should be abstracted separately.
  *
  * While the design is based on homotopy type theory, implementations can be with a range of logics or functional languages,
  * for instance HOL or the Wolfram language. All methods are optional, so anything outside a language is just not built.
  *
  * This is integrated with and used by many components.
  *
  * * As a target for natural language and latex translation.
  * * Bidirectional integration with HoTT implementations:
  * * * terms have such a structure,
  * * * terms can be translated to a language with this structure.
  * * Allows pickling of terms by implementing a formal expression language.
  * * _Goal:_ Bidirectional integration with lean expressions.
  *
  */
trait ExprLang[E] {
  def variable[S](name: S, typ: E): Option[E]

  def typVariable[S](name: S, level: Int): Option[E]

  /**
    * anonymous variable
    */
  def anonVar(typ: E): Option[E]

  /**
    * meta-variable of a given type, i.e., whose value must be inferred
    * (elaborated in lean's terminology).
    */
  def metaVar(typ: E): Option[E]

  def lambda(variable: E, value: E): Option[E]

  def pi(variable: E, typ: E): Option[E]

  def appln(func: E, arg: E): Option[E]

  def equality(lhs: E, rhs: E): Option[E]

  def sigma(variable: E, typFamily: E): Option[E]

  def pair(x: E, y: E): Option[E]

  def proj1(xy: E): Option[E]

  def proj2(xy: E): Option[E]

  def or(first: E, second: E): Option[E]

  def incl1(typ: E): Option[E]

  def incl2(typ: E): Option[E]

  /**
    * true type
    */
  def tt: Option[E]

  /**
    * element of true type
    */
  def qed: Option[E]

  /**
    * false type
    */
  def ff: Option[E]

  def orCases(first: E, second: E): Option[E]

  def numeral(n: Int): Option[E]

  /**
    * non-dependent pair built from abstract methods.
    */
  def pairTyp(first: E, second: E) =
    anonVar(first) flatMap ((x) => lambda(x, second) flatMap (sigma(x, _)))

  def funcTyp(dom: E, codom: E) = anonVar(dom) flatMap ((x) => pi(x, codom))

  def i1(typ: E, value: E) = incl1(typ) flatMap ((i) => appln(i, value))

  def i2(typ: E, value: E) = incl2(typ) flatMap ((i) => appln(i, value))
}

object ExprLang {
  case class Exists[E](term: E)

  case class All[E](term: E)

  def appln[E: ExprLang] =
    (fa: (E, E)) => implicitly[ExprLang[E]].appln(fa._1, fa._2)

  def lambda[E: ExprLang] =
    (fa: (E, E)) => implicitly[ExprLang[E]].lambda(fa._1, fa._2)

  def pair[E: ExprLang] =
    (fa: (E, E)) => implicitly[ExprLang[E]].pair(fa._1, fa._2)

  def pairTyp[E: ExprLang] =
    (fa: (E, E)) => implicitly[ExprLang[E]].pairTyp(fa._1, fa._2)

  def equality[E: ExprLang] =
    (fa: (E, E)) => implicitly[ExprLang[E]].equality(fa._1, fa._2)

  def pi[E: ExprLang] =
    (fa: (E, E)) => implicitly[ExprLang[E]].pi(fa._1, fa._2)

  def func[E: ExprLang] =
    (fa: (E, E)) => implicitly[ExprLang[E]].funcTyp(fa._1, fa._2)

  def sigma[E: ExprLang] =
    (fa: (E, E)) => implicitly[ExprLang[E]].sigma(fa._1, fa._2)

  def or[E: ExprLang] =
    (fa: (E, E)) => implicitly[ExprLang[E]].or(fa._1, fa._2)

  def orCases[E: ExprLang] =
    (fa: (E, E)) => implicitly[ExprLang[E]].orCases(fa._1, fa._2)

  def i1[E: ExprLang] =
    (fa: (E, E)) => implicitly[ExprLang[E]].i1(fa._1, fa._2)

  def i2[E: ExprLang] =
    (fa: (E, E)) => implicitly[ExprLang[E]].i2(fa._1, fa._2)

  def incl1[E: ExprLang] = implicitly[ExprLang[E]].incl1 _

  def incl2[E: ExprLang] = implicitly[ExprLang[E]].incl2 _

  def proj1[E: ExprLang] = implicitly[ExprLang[E]].proj1 _

  def proj2[E: ExprLang] = implicitly[ExprLang[E]].proj2 _

  def anonVar[E: ExprLang] = implicitly[ExprLang[E]].anonVar _

  def metaVar[E: ExprLang] = implicitly[ExprLang[E]].metaVar _

  def numeral[E: ExprLang] = implicitly[ExprLang[E]].numeral _

  def variable[E: ExprLang] =
    (nt: (String, E)) => implicitly[ExprLang[E]].variable(nt._1, nt._2)

  def tt[E: ExprLang] = implicitly[ExprLang[E]].tt

  def ff[E: ExprLang] = implicitly[ExprLang[E]].ff

  def qed[E: ExprLang] = implicitly[ExprLang[E]].qed

  def applnQ[E: ExprLang](func: E, arg: E): Option[E] = {
    val lang = implicitly[ExprLang[E]]
    (func, arg) match {
      case (_, All(x)) =>
        Try(
          applnQ(func, x.asInstanceOf[E]) flatMap
            (lang.lambda(x.asInstanceOf[E], _))).toOption.flatten
      case (_, Exists(x)) =>
        Try(
          applnQ(func, x.asInstanceOf[E]) flatMap
            (lang.sigma(x.asInstanceOf[E], _))).toOption.flatten
      case (All(x), _) =>
        Try(lang.lambda(x.asInstanceOf[E], arg)).toOption.flatten
      case (Exists(x), _) =>
        Try(lang.sigma(x.asInstanceOf[E], arg)).toOption.flatten
      case _ => lang.appln(func, arg)
    }
  }
}

trait Vocabulary[E, C] {

  /**
    * optionally parse token, such as function in language, to expression, depending on context;
    * note that this changes with more definitions,
    * so it may be best to refer to something external, which can be a mutable buffer, database etc.;
    *
    */
  def vocab(name: String, context: C): Option[E]
}

trait FormulaParser[E] {

  /**
    * optionally parse formula (such as latex) to expression.
    */
  def formula(fmla: String): Option[E]
}

trait Domain[E] {
  def domain: E => Option[E]
}

trait ExprPatterns[E] {
  def isPair: E => Option[(E, E)]

  def isSigma: E => Option[(E, E)]

  def isPi: E => Option[(E, E)]
}
