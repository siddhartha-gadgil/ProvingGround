package provingground.translation

import provingground._, HoTT._

import induction._

import shapeless._

import scala.meta.{Term => MTerm, Type => MType, _}

case class CodeGen(indNames: Map[MTerm, MTerm] = Map()) { codegen =>
  import CodeGen._

  import TermPatterns._

  def apply(t: Term) = onTerm(t)

  val onTerm: Term => Option[MTerm] = {
    def prefix(s: MTerm): MTerm =
      indNames.get(s).orElse(indName(s)).getOrElse(s.toString.parse[MTerm].get)
    base || indRecFunc >>> {
      case (index, (dom, (codom, defnData))) =>
        val ind                 = s"${prefix(dom)}".parse[MTerm].get
        val fullInd             = index.foldLeft(ind) { case (func, arg) => q"$func($arg)" }
        val withImplicit: MTerm = q"val rxyz = ${fullInd}.rec($codom); rxyz"
        defnData.foldLeft(withImplicit) {
          case (head, d) => q"$head($d)"
        }
    } ||
    recFunc >>> {
      case (dom, (codom, defnData)) =>
        val ind                 = s"${prefix(dom)}".parse[MTerm].get
        val withImplicit: MTerm = q"val rxyz = ${ind}.rec($codom); rxyz"
        defnData.foldLeft(withImplicit) {
          case (head, d) => q"$head($d)"
        }
    } ||
    indInducFunc >>> {
      case (index, (dom, (codom, defnData))) =>
        val ind                 = s"${prefix(dom)}".parse[MTerm].get
        val fullInd             = index.foldLeft(ind) { case (func, arg) => q"$func($arg)" }
        val withImplicit: MTerm = q"val rxyz = ${fullInd}.induc($codom); rxyz"
        defnData.foldLeft(withImplicit) {
          case (head, d) => q"$head($d)"
        }
    } || inducFunc >>> {
      case (dom, (codom, defnData)) =>
        val ind                 = s"${prefix(dom)}".parse[MTerm].get
        val withImplicit: MTerm = q"val rxyz = ${ind}.rec($codom); rxyz"
        defnData.foldLeft(withImplicit) {
          case (head, d) => q"$head($d)"
        }
    }
  }

  def iterFunc[O <: Term with Subs[O], F <: Term with Subs[F]](
      s: IterFuncShape[O, F],
      typ: Typ[O]): Option[MTerm] = {
    import IterFuncShape._
    val typOpt = onTerm(typ)
    s match {
      case _: IdIterShape[u] =>
        for {
          typCode <- typOpt
        } yield q"IterFuncShape.IdIterShape.byTyp($typCode)"
      case fs: FuncShape[u, O, w] =>
        for {
          tail <- codegen(fs.tail)
          head <- iterFunc(fs.head, typ)
        } yield q"IterFuncShape.FuncShape($tail, $head)"
      case fs: DepFuncShape[u, O, w] =>
        for {
          tail <- codegen(fs.tail)
          x = fs.tail.Var
          xv           <- codegen(x)
          headfibreVal <- iterFunc(fs.headfibre(x), typ)
        } yield q"val x =  $xv ; ${headfibreVal}.piShape($xv, $tail)"
    }
  }

  def consShape[S <: HList,
                H <: Term with Subs[H],
                ConstructorType <: Term with Subs[ConstructorType]](
      shape: ConstructorShape[S, H, ConstructorType],
      typ: Typ[H]): Option[MTerm] = {
    import ConstructorShape._
    val typOpt = onTerm(typ)
    shape match {
      case _: IdShape[H] =>
        for {
          typCode <- typOpt
        } yield q"ConstructorShape.IdShape.byTyp($typCode)"
      case fc: FuncConsShape[hs, H, hc, f] =>
        val tailOpt = iterFunc(fc.tail, typ)
        val headOpt = consShape(fc.head, typ)
        for {
          tailCode <- tailOpt
          headCode <- headOpt
        } yield q"ConstructorShape.FuncConsShape($tailCode, $headCode)"
      case fc: CnstFuncConsShape[hs, H, a, b, c] =>
        val tailOpt = onTerm(fc.tail)
        val headOpt = consShape(fc.head, typ)
        for {
          tailCode <- tailOpt
          headCode <- headOpt
        } yield q"ConstructorShape.CnstFuncConsShape($tailCode, $headCode)"
      case fc: CnstDepFuncConsShape[hs, H, a, b, c] =>
        val tailOpt    = onTerm(fc.tail)
        val x          = fc.tail.Var
        val headValOpt = consShape(fc.headfibre(x), typ)
        for {
          tailCode    <- tailOpt
          headValCode <- headValOpt
          xv          <- codegen(x)
        } yield q"val x = $xv; x ~>: $headValCode"
    }

  }

  def consSeqDom[SS <: HList, H <: Term with Subs[H], Intros <: HList](
      seqDom: ConstructorSeqDom[SS, H, Intros],
      typ: Typ[H]): Option[MTerm] = {
    import ConstructorSeqDom._
    val typOpt = onTerm(typ)
    seqDom match {
      case _: Empty[H] =>
        for {
          typCode <- typOpt
        } yield q"ConstructorSeqDom.Empty.byTyp($typCode)"
      case cons: Cons[a, b, H, c, d] =>
        val name     = s"""HoTT.Name("${cons.name}")"""
        val nameCode = name.parse[MTerm].get
        for {
          shapeCode <- consShape(cons.pattern, typ)
          tailCode  <- consSeqDom(cons.tail, typ)
        } yield q"ConstructorSeqDom.Cons($nameCode, $shapeCode, $tailCode)"
    }

  }

  def consSeq[SS <: HList, H <: Term with Subs[H], Intros <: HList](
      seq: ConstructorSeqTL[SS, H, Intros]): Option[MTerm] = {
    for {
      typCode <- onTerm(seq.typ)
      domCode <- consSeqDom(seq.seqDom, seq.typ)
    } yield q"ConstructorSeqTL($domCode, $typCode)"

  }

  def fmtConsSeq[SS <: HList, H <: Term with Subs[H], Intros <: HList](
      seq: ConstructorSeqTL[SS, H, Intros]) =
    consSeq(seq).map((c) => org.scalafmt.Scalafmt.format(c.toString))

}

object CodeGen {
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
    } || lambdaTriple >>> {
      case ((variable, typ), value) =>
        q"""$variable :~> $value"""
    } || piTriple >>> {
      case ((variable, typ), value) =>
        q"""$variable ~>: $value"""
    } || sigmaTriple >>> {
      case ((variable, typ), value) =>
        q"""$variable ++ $value"""
    } || universe >>> { (n) =>
      if (n == 0) q"Type" else q"""Universe($n)"""
    } || symbolic >>> {
      case (s, typ) =>
        s""" "$s" :: $typ """.parse[MTerm].get
    } || prodTyp >>> { case (first, second) => q"""ProdTyp($first, $second)""" } || pairTerm >>> {
      case (first, second)                  => q"""PairTerm($first, $second)"""
    } || equation >>> { case (lhs, rhs)     => q"$lhs =:= $rhs" } || depPairTerm >>> {
      case ((a, b), f)                      => q"DepPair($a, $b, $f)"
    } || plusTyp >>> {
      case (first, scnd) => q"""PlusTyp($first, $scnd)"""
    }

  def getName(s: MTerm): Option[String] = s match {
    case q""" $name :: $typ """ => Some(name.toString)
    case _                      => None
  }

  def indName(s: MTerm): Option[MTerm] =
    getName(s).map((name) => s"${name}Ind".parse[MTerm].get)

}
