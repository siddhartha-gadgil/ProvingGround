package provingground.translation

import provingground._
import HoTT._
import UnicodeSyms.UnivSym
import fansi._
import fansi.Color._

import scala.util.matching.Regex

object FansiTranslate {
  import TermPatterns._

  val syms: HoTT.UnicodeSyms.type = UnicodeSyms

  def apply(x: Term): String =
    fansiTrans(x) map (_.toString()) getOrElse (x.toString())

  // import fansi.Color.LightRed

  val fansiTrans: Translator.OrElse[Term, Str] =
    Translator.Empty[Term, Str] || formalAppln >>> {
      case (func, arg) => func ++ "(" ++ arg ++ ")"
    } || funcTyp >>> {
      case (dom, codom) =>
        Str("(") ++ dom ++ " " ++ Color.LightRed(syms.Arrow) ++ " " ++ codom ++ ")"
    } || lambdaTriple >>> {
      case ((variable, typ), value) =>
        (Str("(") ++ variable ++ Yellow(" : ") ++ typ ++ ") " ++ LightRed(
          syms.MapsTo
        ) ++ " " ++ value)
    } || piTriple >>> {
      case ((variable, typ), value) =>
        (Cyan(Str(syms.Pi)) ++ LightYellow("(") ++ variable ++ Yellow(" : ") ++ typ ++ LightYellow(
          ")"
        ) ++ LightRed("{ ") ++ value ++ LightRed(" }"))
    } || sigmaTriple >>> {
      case ((variable, typ), value) =>
        (Cyan(Str(syms.Sigma)) ++ LightYellow("(") ++ variable ++ Yellow(" : ") ++ typ ++ LightYellow(
          ")"
        ) ++ LightRed(
          "{ "
        ) ++ value ++ LightRed(" }"))
    } || universe >>> { (_) =>
      (LightCyan(Str(UnivSym)))
    } || symName >>> ((s) => (Str(s))) || symString >>> ((s) => (Str(s))) ||
      prodTyp >>> { case (first, second) => (first ++ syms.Prod ++ second) } ||
      absPair >>> {
        case (first, second) => (Str("(") ++ first ++ " , " ++ second ++ ")")
      } ||
      // identityTyp >>> {case ((dom, lhs), rhs) => (lhs ++ LightRed(" = ") ++ rhs ++ " (in " ++ dom ++ ")")} || // invoke if we want expanded equality
      equation >>> { case (lhs, rhs) => (lhs ++ LightRed(" = ")) ++ rhs } ||
      plusTyp >>> {
        case (first, scnd) => (first ++ LightRed(Str(" + ")) ++ scnd)
      } ||
      indRecFunc >>> {
        case (domW, (index, (dom, (codom, defnData)))) =>
          defnData.foldLeft(s"rec_{ $domW ; $codom }") {
            case (head, d) => s"$head($d)"
          }
      } ||
      recFunc >>> {
        case (dom, (codom, defnData)) =>
          defnData.foldLeft(s"rec_{ $dom ; $codom }") {
            case (head, d) => s"$head($d)"
          }
      } ||
      indInducFunc >>> {
        case (domW, (index, (dom, (depcodom, defnData)))) =>
          val h =
            Str(s"induc_{ $domW ; $depcodom }")
          defnData.foldLeft(h) {
            case (head, d) => s"$head($d)"
          }
      } ||
      inducFunc >>> {
        case (dom, (depcodom, defnData)) =>
          val h = Str(s"induc_{ $dom ; $depcodom }")
          defnData.foldLeft(h) {
            case (head, d) => s"$head($d)"
          }
      } ||
      stringRep >>> { (s) =>
        s
      }

  /*import pprint._
  implicit val termprint: PPrinter[Term] = new PPrinter[Term] {
    def render0(t: Term, c: Config) = List(apply(t)).iterator
  }*/
}

object TeXTranslate {
  import TermPatterns._

  val syms: HoTT.UnicodeSyms.type = UnicodeSyms

  val dolName: Regex = """\$([a-z]+)""".r

  def hatDol(s: String): String =
    dolName.replaceAllIn(s, (m) => s"\\\\widehat\\{${m.group(1)}\\}")

  def apply(x: Term, underscoreEscape: Boolean = false): String =
    hatDol(
      texTrans(underscoreEscape)(x) map (_.toString()) getOrElse (x.toString())
    )

  // import fansi.Color.LightRed

  def texTrans(underscoreEscape: Boolean): Translator.OrElse[Term, String] =
    Translator.Empty[Term, String] || formalAppln >>> {
      case (func, arg) => func ++ "(" ++ arg ++ ")"
    } || funcTyp >>> {
      case (dom, codom) =>
        s"""($dom \\to $codom)"""
    } || lambdaTriple >>> {
      case ((variable, typ), value) =>
        s"""(($variable : $typ) \\mapsto $value)"""
    } || piTriple >>> {
      case ((variable, typ), value) =>
        s"""(\\prod\\limits_{$variable : $typ} $value)"""
    } || sigmaTriple >>> {
      case ((variable, typ), value) =>
        s"""(\\sum\\limits_{$variable : $typ} $value)"""
    } || universe >>> { (n) =>
      s"""\\mathcal{U}_$n"""
    } || symName >>> (
        (s) =>
          if (s == "_") "\\_"
          else if (underscoreEscape) s.replace("_", "\\_")
          else s
      ) || symString >>> (
        (s) =>
          if (s == "_") "\\_"
          else if (underscoreEscape) s.replace("_", "\\_")
          else s
      ) ||
      prodTyp >>> { case (first, second) => s"""($first \\times $second)""" } ||
      absPair >>> {
        case (first, second) => s"""($first, $second)"""
      } ||
      // identityTyp >>> {case ((dom, lhs), rhs) => (lhs ++ LightRed(" = ") ++ rhs ++ " (in " ++ dom ++ ")")} || // invoke if we want expanded equality
      equation >>> { case (lhs, rhs) => s"$lhs = $rhs" } ||
      plusTyp >>> {
        case (first, scnd) => s"""($first \\oplus $scnd)"""
      } ||
      indRecFunc >>> {
        case (domW, (index, (dom, (codom, defnData)))) =>
          defnData.foldLeft(
            s"rec_{$domW ; ${index.mkString("(", ")(", ")")}}($codom)"
          ) {
            case (head, d) => s"$head($d)"
          }
      } ||
      recFunc >>> {
        case (dom, (codom, defnData)) =>
          defnData.foldLeft(s"rec_{$dom;$codom}") {
            case (head, d) => s"$head($d)"
          }
      } ||
      indInducFunc >>> {
        case (domW, (index, (dom, (depcodom, defnData)))) =>
          val h =
            s"induc_{$domW ; ${index.mkString("(", ")(", ")")}}($depcodom)"
          defnData.foldLeft(h) {
            case (head, d) => s"$head($d)"
          }
      } ||
      inducFunc >>> {
        case (dom, (depcodom, defnData)) =>
          val h = s"induc_{$dom ;$depcodom}"
          defnData.foldLeft(h) {
            case (head, d) => s"$head($d)"
          }
      } ||
      stringRep >>> { (s) =>
        s
      }

}

trait FansiShow[-U] {
  def show(x: U): String
}

object FansiShow {
  implicit class View[U: FansiShow](x: U) {
    def fansi = implicitly[FansiShow[U]].show(x)
  }

  implicit def term[U <: Term]: FansiShow[U] = new FansiShow[U] {
    def show(x: U) = FansiTranslate(x)
  }

  import pprint._

  val fansiHandler: PartialFunction[Any, Tree] = {
    case t: Term                          => Tree.Literal(FansiTranslate(t))
    case sym: AnySym                      => Tree.Literal(sym.toString)
    case w: andrewscurtis.FreeGroups.Word => Tree.Literal(w.toString)
    case s: String                        => Tree.Literal(s)
    case stream : LazyList[a] => Tree.Literal("[" + stream.head.toString+" ..." )
  }

  val fansiPrint: PPrinter =
    pprint.PPrinter.Color.copy(additionalHandlers = fansiHandler)

  val simpleHandler: PartialFunction[Any, Tree] = {
    case t: Term                          => Tree.Literal(t.toString)
    case sym: AnySym                      => Tree.Literal(sym.toString)
    case w: andrewscurtis.FreeGroups.Word => Tree.Literal(w.toString)
    case s: String                        => Tree.Literal(s)
    case stream : LazyList[a] => Tree.Literal("[" + stream.head.toString+" ..." )
  }

  val simplePrint: PPrinter =
    pprint.PPrinter.BlackWhite.copy(additionalHandlers = simpleHandler)

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
      U1: FansiShow,
      U2: FansiShow,
      U3: FansiShow,
      U4: FansiShow
  ]: FansiShow[(U1, U2, U3, U4)] =
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
        (for ((x, y) <- m) yield (x.fansi, y.fansi)).toString
    }

  implicit def weighted[U: FansiShow]: FansiShow[Weighted[U]] =
    new FansiShow[Weighted[U]] {
      def show(x: Weighted[U]) =
        s"${x.elem.fansi} -> ${x.weight}"
    }

  implicit def fd[U: FansiShow]: FansiShow[FiniteDistribution[U]] =
    new FansiShow[FiniteDistribution[U]] {
      def show(x: FiniteDistribution[U]): String =
        (x.flatten.sort map (_.fansi)).pmf.fansi
    }
}
