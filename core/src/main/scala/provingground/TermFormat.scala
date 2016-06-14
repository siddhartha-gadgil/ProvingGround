package provingground

import HoTT._

import UnicodeSyms.UnivSym

object TermFormat {}

object PrintFormat extends TermRec[String] {
  val syms = UnicodeSyms

  val specialTerms: PartialFunction[Term, String] = Map()

  def fromString(str: String)(implicit typ: Typ[Term]): String = str

  def appln(func: String, arg: String): String = func + "(" + arg + ")"

  def arrow(dom: String, codom: String): String =
    dom + " " + syms.Arrow + " " + codom

  def lambda(variable: String, typ: String, value: String): String =
    s"($variable : $typ) ${syms.MapsTo}  $value"

  def equality(dom: String, lhs: String, rhs: String) =
    s"$lhs = $rhs (in $dom)"

  def pi(fibre: String): String = s"${syms.Pi}($fibre)"

  def sigma(fibre: String): String = s"${syms.Sigma}($fibre)"

  def plus(first: String, scnd: String): String = first + " + " + scnd

  def pair(first: String, second: String) =
    "(" + first + " , " + second + ")"

  def symbobj(term: SymbObj[Term]): String =
    term.name.toString

  def symbtyp(typ: SymbTyp): String = typ.name.toString

  def symbolic(name: AnySym, typ: Typ[Term]): String =
    name.toString

  def univ(n: Int) = s"${UnivSym}"
}

object LatexFormat extends TermRec[String] {
  def latex(t: Term) = "$"+ (apply(t).replace("$", ".")) +"$"

  val specialTerms: PartialFunction[Term, String] = Map()

  def fromString(str: String)(implicit typ: Typ[Term]): String = str

  def appln(func: String, arg: String): String = func + "(" + arg + ")"

  def arrow(dom: String, codom: String): String =
    dom + " \\to " + codom

  def lambda(variable: String, typ: String, value: String): String =
    s"($variable : $typ)"+" \\mapsto "+  s"$value"

  def equality(dom: String, lhs: String, rhs: String) =
    s"$lhs = $rhs (in $dom)"

  def pi(fibre: String): String = s"\\prod\\limits_{$fibre}"

  def sigma(fibre: String): String = s"\\sum\\limits_{$fibre}"

  def plus(first: String, scnd: String): String = first + " + " + scnd

  def pair(first: String, second: String) =
    "(" + first + " , " + second + ")"

  def symbobj(term: SymbObj[Term]): String =
    term.name.toString

  def symbtyp(typ: SymbTyp): String = typ.name.toString

  def symbolic(name: AnySym, typ: Typ[Term]): String =
    name.toString

  def univ(n: Int) = "\\mathcal{U}"
}

object FansiFormat extends TermRec[fansi.Str] {
  import fansi.Str
  import fansi.Color._

  val syms = UnicodeSyms

  val specialTerms: PartialFunction[Term, Str] = Map()

  def fromString(str: String)(implicit typ: Typ[Term]): Str = Str(str)

  def appln(func: Str, arg: Str): Str = func ++ "(" ++ arg ++ ")"

  def arrow(dom: Str, codom: Str): Str =
    dom ++ " " ++ LightRed(syms.Arrow) ++ " " ++ codom

  def lambda(variable: Str, typ: Str, value: Str): Str =
    Str("(") ++ variable ++ Yellow(" : ") ++ typ ++ ") " ++ LightRed(
        syms.MapsTo) ++ " " ++ value

  def equality(dom: Str, lhs: Str, rhs: Str) =
    lhs ++ LightRed(" = ") ++ rhs ++ " (in " ++ dom ++ ")"

  def pi(fibre: Str): Str =
    Cyan(Str(syms.Pi)) ++ LightYellow("(") ++ fibre ++ LightYellow(")")

  def sigma(fibre: Str): Str =
    Cyan(Str(syms.Sigma)) ++ LightYellow("(") ++ fibre ++ LightYellow(")")

  def plus(first: Str, scnd: Str): Str = first ++ LightRed(Str(" + ")) ++ scnd

  def pair(first: Str, second: Str) =
    Str("(") ++ first ++ " , " ++ second ++ ")"

  def symbobj(term: SymbObj[Term]): Str =
    Green(Str(term.name.toString))

  def symbtyp(typ: SymbTyp): Str = Cyan(Str(typ.name.toString))

  def symbolic(name: AnySym, typ: Typ[Term]): Str =
    Magenta(Str(name.toString))

  def univ(n: Int) = LightCyan(Str(UnivSym))
}


trait FansiShow[-U] {
  def show(x: U): String
}

object FansiShow {
  implicit class View[U: FansiShow](x: U) {
    def fansi = implicitly[FansiShow[U]].show(x)
  }

  implicit def term[U <: Term]: FansiShow[U] = new FansiShow[U] {
    def show(x: U) = FansiFormat(x).toString
  }

  implicit def list[U: FansiShow]: FansiShow[List[U]] =
    new FansiShow[List[U]] {
      def show(x: List[U]) = (x map (_.fansi)).mkString("[", "\n", "]")
    }

  implicit def vec[U: FansiShow]: FansiShow[Vector[U]] =
    new FansiShow[Vector[U]] {
      def show(x: Vector[U]) = (x map (_.fansi)).mkString("[", "\n", "]")
    }

  implicit def set[U: FansiShow]: FansiShow[Set[U]] = new FansiShow[Set[U]] {
    def show(x: Set[U]) = (x map (_.fansi)).toString
  }

  implicit def tuple2[U1: FansiShow, U2: FansiShow]: FansiShow[(U1, U2)] =
    new FansiShow[(U1, U2)] {
      def show(tup: (U1, U2)): String = (tup._1.fansi, tup._2.fansi).toString
    }

  implicit def tuple3[U1: FansiShow, U2: FansiShow, U3: FansiShow]
    : FansiShow[(U1, U2, U3)] =
    new FansiShow[(U1, U2, U3)] {
      def show(tup: (U1, U2, U3)): String =
        (tup._1.fansi, tup._2.fansi, tup._3.fansi).toString
    }

  implicit def tuple4[
      U1: FansiShow, U2: FansiShow, U3: FansiShow, U4: FansiShow]
    : FansiShow[(U1, U2, U3, U4)] =
    new FansiShow[(U1, U2, U3, U4)] {
      def show(tup: (U1, U2, U3, U4)): String =
        (tup._1.fansi, tup._2.fansi, tup._3.fansi, tup._4.fansi).toString
    }

  implicit def string: FansiShow[String] = new FansiShow[String] {
    def show(x: String) = x
  }

  implicit def num[U: Numeric]: FansiShow[U] = new FansiShow[U] {
    def show(x: U) = x.toString
  }

  implicit def mapp[U: FansiShow, V: FansiShow]: FansiShow[Map[U, V]] =
    new FansiShow[Map[U, V]] {
      def show(m: Map[U, V]) =
        (for ((x, y) <- m) yield (x.fansi, y.fansi)).toMap.toString
    }

  implicit def weighted[U: FansiShow]: FansiShow[Weighted[U]] =
    new FansiShow[Weighted[U]] {
      def show(x: Weighted[U]) =
        s"${x.elem.fansi} -> ${x.weight}"
    }

  implicit def fd[U: FansiShow]: FansiShow[FiniteDistribution[U]] =
    new FansiShow[FiniteDistribution[U]] {
      def show(x: FiniteDistribution[U]) =
        (x.flatten.sort map (_.fansi)).pmf.fansi
    }
}
