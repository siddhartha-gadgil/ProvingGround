package provingground.translation

import provingground._, HoTT._

import induction._

import scala.meta.{Term => MTerm, Type => MType, _}

case class CodeGen(indNames: Map[MTerm, MTerm] = Map()){codegen =>
  import CodeGen._

  import TermPatterns._

  def apply(t: Term) = onTerm(t)

  val onTerm : Term => Option[MTerm] =
          {
            def prefix(s: MTerm) : MTerm =
              indNames.get(s).orElse(indName(s)).getOrElse(s.toString.parse[MTerm].get)
            base || indRecFunc >>> {
            case (index, (dom, (codom, defnData))) =>
              val ind = s"${prefix(dom)}".parse[MTerm].get
              val fullInd = index.foldLeft(ind){case (func, arg) => q"$func($arg)"}
              val withImplicit : MTerm = q"val rxyz = ${fullInd}.rec($codom); rxyz"
              defnData.foldLeft(
                withImplicit) {
                case (head, d) => q"$head($d)"
              }
          } ||
          recFunc >>> {
            case (dom, (codom, defnData)) =>
              val ind = s"${prefix(dom)}".parse[MTerm].get
              val withImplicit : MTerm = q"val rxyz = ${ind}.rec($codom); rxyz"
              defnData.foldLeft(
                withImplicit) {
                case (head, d) => q"$head($d)"
              }
          } ||
          indInducFunc >>> {
            case (index, (dom, (codom, defnData))) =>
              val ind = s"${prefix(dom)}".parse[MTerm].get
              val fullInd = index.foldLeft(ind){case (func, arg) => q"$func($arg)"}
              val withImplicit : MTerm = q"val rxyz = ${fullInd}.induc($codom); rxyz"
              defnData.foldLeft(
                withImplicit) {
                case (head, d) => q"$head($d)"
              }
          } || inducFunc >>> {
            case (dom, (codom, defnData)) =>
              val ind = s"${prefix(dom)}".parse[MTerm].get
              val withImplicit : MTerm = q"val rxyz = ${ind}.rec($codom); rxyz"
              defnData.foldLeft(
                withImplicit) {
                case (head, d) => q"$head($d)"
              }
            }
      }


      def iterFunc[O <: Term with Subs[O], F <: Term with Subs[F]](s: IterFuncShape[O, F], typ: Typ[O]): Option[MTerm] ={
        import IterFuncShape._
        s match {
          case _ : IdIterShape[u] => Some(q"IterFuncShape.IdIterShape.byTyp(typ)")
          case fs: FuncShape[u, O, w] =>
            for {
              tail <- codegen(fs.tail)
              head <- iterFunc(fs.head, typ)
            } yield q"IterFuncShape.FuncShape($tail, $head)"
        case fs: DepFuncShape[u, O, w] =>
          for {
            tail <- codegen(fs.tail)
            x = fs.tail.Var
            xv <- codegen(x)
            headfibreVal <- iterFunc(fs.headfibre(x), typ)
          } yield
            q"val x =  $xv ; ${headfibreVal}.piShape($xv, $tail)"


        }
  }

}

object CodeGen{
  import TermPatterns._

  val base =
    Translator.Empty[Term, MTerm] ||
    formalAppln >>> {
    case (func, arg) => q"$func($arg)"
  } || funcTyp >>> {
    case (dom, codom) =>
      q"""$dom ->: $codom"""
  } || lambdaFixedTriple >>> {
    case ((variable, typ), value) =>
      q"""$variable :-> $value"""
  }  || lambdaTriple >>> {
    case ((variable, typ), value) =>
      q"""$variable :~> $value"""
  } || piTriple >>> {
    case ((variable, typ), value) =>
      q"""$variable ~>: $value"""
  } || sigmaTriple >>> {
    case ((variable, typ), value) =>
      q"""$variable ++ $value"""
  } || universe >>> { (n) =>
    if (n ==0) q"Type" else q"""Universe($n)"""
  } || symbolic >>> {case (s, typ) =>
      s""" "$s" :: $typ """.parse[MTerm].get
  } || prodTyp >>> { case (first, second) => q"""ProdTyp($first, $second)"""
  } || pairTerm >>> {
      case (first, second) => q"""PairTerm($first, $second)"""
  } || equation >>> { case (lhs, rhs) => q"$lhs =:= $rhs"
  } || depPairTerm >>> {
      case ((a, b), f) => q"DepPair($a, $b, $f)"
  } || plusTyp >>> {
      case (first, scnd) => q"""PlusTyp($first, $scnd)"""
  }

  def getName(s: MTerm) : Option[String] = s match {
    case q""" $name :: $typ """ => Some(name.toString)
    case _ => None
  }

  def indName(s: MTerm) : Option[MTerm] = getName(s).map((name) => s"${name}Ind".parse[MTerm].get)


}
