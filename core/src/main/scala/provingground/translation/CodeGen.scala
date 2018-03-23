package provingground.translation

import provingground._, HoTT._

import induction._

import shapeless._

import scala.meta.{Term => MTerm, Type => MType, _}

case class CodeGen(indNames: Map[MTerm, MTerm] = Map(),
                   defns: PartialFunction[Term, MTerm] = Map()) { codegen =>
  import CodeGen._

  import TermPatterns._

  def apply(t: Term) = onTerm(t)

  val onTerm: Term => Option[MTerm] = {
    def prefix(s: MTerm): MTerm =
      indNames.get(s).orElse(indName(s)).getOrElse(s.toString.parse[MTerm].get)
    Translator.Simple(defns.lift) || base || indRecFunc >>> {
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
        val withImplicit: MTerm = q"val rxyz = ${ind}.induc($codom); rxyz"
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

  def typFamilyPtn[H <: Term with Subs[H],
                   F <: Term with Subs[F],
                   Index <: HList](ptn: TypFamilyPtn[H, F, Index],
                                   typ: Typ[H]): Option[MTerm] = {
    import TypFamilyPtn._
    val typOpt = onTerm(typ)
    ptn match {
      case _: IdTypFamily[H] =>
        for {
          typCode <- typOpt
        } yield q"TypFamilyPtn.IdTypFamily.byTyp($typCode)"
      case ft: FuncTypFamily[a, H, b, c] =>
        for {
          headCode <- onTerm(ft.head)
          tailCode <- typFamilyPtn(ft.tail, typ)
        } yield q"TypFamilyPtn.FuncTypFamily($headCode, $tailCode)"
      case ft: DepFuncTypFamily[a, H, b, c] =>
        val x       = ft.head.Var
        val tailVal = ft.tailfibre(x)
        for {
          headCode    <- onTerm(ft.head)
          xv          <- onTerm(x)
          tailValCode <- typFamilyPtn(tailVal, typ)
        } yield
          q"val x = $xv; TypFamilyPtn.DepFuncTypFamily($headCode, x ~>: $tailValCode)"
    }

  }

  def index[Index](ind: Index): Option[MTerm] = {
    import shapeless._
    ind match {
      case _: HNil => Some(q"HNil")
      case (head: Term) :: tail =>
        for {
          headCode <- codegen(head)
          tailCode <- index(tail)
        } yield q"import shapeless._ ; $headCode :: $tailCode"
      case _ => None
    }
  }

  def indexedIterFunc[H <: Term with Subs[H],
                      F <: Term with Subs[F],
                      Fb <: Term with Subs[Fb],
                      Index <: HList](
      iterFunc: IndexedIterFuncShape[H, F, Fb, Index],
      w: Fb): Option[MTerm] = {
    import IndexedIterFuncShape._, iterFunc.family
    iterFunc match {
      case id: IdIterShape[H, Fb, Index] =>
        val typ       = id.family.typ(w, id.index)
        val familyOpt = typFamilyPtn(family, typ)
        for {
          familyCode <- familyOpt
          indexCode  <- index(id.index)
        } yield q"IndexedIterFuncShape.IdIterShape($familyCode, $indexCode)"

      case fc: FuncShape[a, b, H, Fb, Index] =>
        for {
          headCode <- codegen(fc.head)
          tailCode <- indexedIterFunc(fc.tail, w)
        } yield q"IndexedIterFuncShape.FuncShape($headCode, $tailCode)"
      case fs: DepFuncShape[u, H, w, Fb, Index] =>
        for {
          head <- codegen(fs.head)
          x = fs.head.Var
          xv           <- codegen(x)
          tailfibreVal <- indexedIterFunc(fs.tailfibre(x), w)
        } yield q"val x =  $xv ; ${tailfibreVal}.piShape($xv, $head)"
    }
  }

  def indexedConsShape[S <: HList,
                       H <: Term with Subs[H],
                       Fb <: Term with Subs[Fb],
                       ConstructorType <: Term with Subs[ConstructorType],
                       Index <: HList](
      shape: IndexedConstructorShape[S, H, Fb, ConstructorType, Index],
      w: Fb): Option[MTerm] = {
    import IndexedConstructorShape._
    shape match {
      case id: IndexedIdShape[H, Fb, Index] =>
        val typ       = id.family.typ(w, id.index)
        val familyOpt = typFamilyPtn(shape.family, typ)
        for {
          familyCode <- familyOpt
          indexCode  <- index(id.index)
        } yield
          q"IndexedConstructorShape.IndexedIdShape($familyCode, $indexCode)"
      case fc: IndexedFuncConsShape[a, H, Fb, b, c, Index] =>
        ??? // wornd, deprecated case
      //   for {
      //     tailCode <- iterFunc(fc.tail, w)
      //     headCode <- indexedConsShape(fc.head, w)
      //     indCode <- index(fc.ind)
      //   } yield q"IndexedConstructorShape.IndexedFuncConsShape($tailCode, $headCode, $indCode)"
      case fc: IndexedIndexedFuncConsShape[a, H, c, b, Fb, Index] =>
        for {
          tailCode <- indexedIterFunc(fc.tail, w)
          headCode <- indexedConsShape(fc.head, w)
          indCode  <- index(fc.ind)
        } yield
          q"IndexedConstructorShape.IndexedIndexedFuncConsShape($tailCode, $headCode, $indCode)"
      case fc: IndexedCnstFuncConsShape[a, b, H, Fb, c, Index] =>
        for {
          tailCode <- codegen(fc.tail)
          headCode <- indexedConsShape(fc.head, w)
        } yield
          q"IndexedConstructorShape.IndexedCnstFuncConsShape($tailCode, $headCode)"
      case fc: IndexedCnstDepFuncConsShape[a, b, H, Fb, c, Index] =>
        val tailOpt    = onTerm(fc.tail)
        val x          = fc.tail.Var
        val headValOpt = indexedConsShape(fc.headfibre(x), w)
        for {
          tailCode    <- tailOpt
          headValCode <- headValOpt
          xv          <- codegen(x)
        } yield q"val x = $xv; x ~>>: $headValCode"
    }

  }

  def indexedConsSeqDom[SS <: HList,
                        H <: Term with Subs[H],
                        F <: Term with Subs[F],
                        Index <: HList,
                        Intros <: HList](
      seqDom: IndexedConstructorSeqDom[SS, H, F, Index, Intros])
    : Option[MTerm] = {
    import IndexedConstructorSeqDom._
    seqDom match {
      case em: Empty[H, F, Index] =>
        val typ = em.family.someTyp(em.W)
        for {
          WCode      <- codegen(em.W)
          familyCode <- typFamilyPtn(em.family, typ)
        } yield q"IndexedConstructorSeqDom.Empty($WCode, $familyCode)"
      case cons: Cons[a, b, H, F, c, Index, d] =>
        val name     = s"""HoTT.Name("${cons.name}")"""
        val nameCode = name.parse[MTerm].get
        for {
          patternCode <- indexedConsShape(cons.pattern, seqDom.W)
          tailCode    <- indexedConsSeqDom(cons.tail)
        } yield
          q"IndexedConstructorSeqDom.Cons($nameCode, $patternCode, $tailCode)"
    }

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
        q"""FuncTyp($dom, $codom)"""
    } || lambdaFixedTriple >>> {
      case ((variable, typ), value) =>
        q"""lmbda($variable)($value)"""
    } || lambdaTriple >>> {
      case ((variable, typ), value) =>
        q"""lambda($variable)($value)"""
    } || piTriple >>> {
      case ((variable, typ), value) =>
        q"""piDefn($variable)($value)"""
    } || sigmaTriple >>> {
      case ((variable, typ), value) =>
        q"""$variable ++ $value"""
    } || universe >>> { (n) =>
      if (n == 0) q"Type" else q"""Universe($n)"""
    } || symbolic >>> {
      case (s, typ) =>
        val name = s""" "$s" """.parse[MTerm].get
        q"$typ.symbObj(Name($name))"
    } || prodTyp >>> { case (first, second) => q"""ProdTyp($first, $second)""" } || pairTerm >>> {
      case (first, second)                  => q"""PairTerm($first, $second)"""
    } || equation >>> { case (lhs, rhs)     => q"$lhs =:= $rhs" } || depPairTerm >>> {
      case ((a, b), f)                      => q"DepPair($a, $b, $f)"
    } || plusTyp >>> {
      case (first, scnd) => q"""PlusTyp($first, $scnd)"""
    }

  def getName(s: MTerm): Option[String] = s match {
    case q""" $name :: $typ """ => Some(name.toString)
    case q""" $typ.symbObj(Name($name)) """ => Some(name.toString.replace("\"", "").trim)
    case _                      => None
  }

  def indName(s: MTerm): Option[MTerm] =
    getName(s).map((name) => s"${name}Ind".parse[MTerm].get)

}
