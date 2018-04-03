package provingground.translation

import provingground._, HoTT._

import induction._

import shapeless._

import scala.meta

import scala.meta.{Term => _, Type => _, _}

case class CodeGen(inducNames: Term => Option[meta.Term] = (_) => None,
                   defns: Map[String, meta.Term] = Map()) {
  codegen =>
  import CodeGen._

  import TermPatterns._

  def apply(t: Term) = onTerm(t)

  val onTerm: Term => Option[meta.Term] = {

    def prefixTermOpt(t: Term): Option[meta.Term] =
      inducNames(t)
        .orElse(
          t match {
            case FormalAppln(f, x) =>
              for {
                fc <- prefixTermOpt(f)
                xc <- onTerm(x)
              } yield q"$fc($xc)"
            case _ => None
          }
        )
        .orElse(indName(t))

    def prefixTerm(t: Term, s: meta.Term): meta.Term =
      prefixTermOpt(t).getOrElse(s)

    Translator.Empty[Term, meta.Term] ||
    symbolic >> {
      case (s, _) =>
        defns.lift(s)
    } ||
    base ||
    indRecFunc :>>> {
      case (
          (tdomW, (tindex, (tdom, (tcodom, tdefnData)))),
          (domW, (index, (dom, (codom, defnData))))
          ) =>
        val ind = prefixTerm(tdomW, domW)
        val inducHead = tdom match {
          case idt: IdentityTyp[u] =>
            q"IdentityTyp.induc($dom.dom, $codom)"
          case _ =>
            q"${ind}.rec($codom)"
        }
        val withImplicit: meta.Term = q"val rxyz = $inducHead; rxyz"
        (defnData ++ index).foldLeft(withImplicit) {
          case (head, d) => q"$head($d)"
        }
    } ||
    recFunc :>>> {
      case ((tdom, (_, _)), (dom, (codom, defnData))) =>
        val ind                     = prefixTerm(tdom, dom) // q"${prefix(dom)}"
        val withImplicit: meta.Term = q"val rxyz = ${ind}.rec($codom); rxyz"
        defnData.foldLeft(withImplicit) {
          case (head, d) => q"$head($d)"
        }
    } ||
    indInducFunc :>>> {
      case (
          (tdomW, (tindex, (tdom, (tcodom, tdefnData)))),
          (domW, (index, (dom, (codom, defnData))))
          ) =>
        val ind = prefixTerm(tdomW, domW)
        val inducHead = tdom match {
          case idt: IdentityTyp[u] =>
            q"IdentityTyp.induc($dom.dom, $codom)"
          case _ =>
            q"${ind}.induc($codom)"
        }
        val withImplicit: meta.Term = q"val rxyz = $inducHead; rxyz"
        (defnData ++ index).foldLeft(withImplicit) {
          case (head, d) => q"$head($d)"
        }
    } || inducFunc :>>> {
      case ((tdom, (_, _)), (dom, (codom, defnData))) =>
        val ind                     = prefixTerm(tdom, dom) //q"${prefix(dom)}"
        val withImplicit: meta.Term = q"val rxyz = ${ind}.induc($codom); rxyz"
        defnData.foldLeft(withImplicit) {
          case (head, d) => q"$head($d)"
        }
    }
  }

  def iterFunc[O <: Term with Subs[O], F <: Term with Subs[F]](
      s: IterFuncShape[O, F],
      typ: Typ[O]): Option[meta.Term] = {
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
      typ: Typ[H]): Option[meta.Term] = {
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
      typ: Typ[H]): Option[meta.Term] = {
    import ConstructorSeqDom._
    val typOpt = onTerm(typ)
    seqDom match {
      case _: Empty[H] =>
        for {
          typCode <- typOpt
        } yield q"ConstructorSeqDom.Empty.byTyp($typCode)"
      case cons: Cons[a, b, H, c, d] =>
        // val name     = s"""HoTT.Name("${cons.name}")"""
        val nameCode = q"""HoTT.Name(${Lit.String(cons.name.toString)})""" // name.parse[meta.Term].get
        for {
          shapeCode <- consShape(cons.pattern, typ)
          tailCode  <- consSeqDom(cons.tail, typ)
        } yield q"ConstructorSeqDom.Cons($nameCode, $shapeCode, $tailCode)"
    }

  }

  def consSeq[SS <: HList, H <: Term with Subs[H], Intros <: HList](
      seq: ConstructorSeqTL[SS, H, Intros]): Option[meta.Term] = {
    for {
      typCode <- onTerm(seq.typ)
      domCode <- consSeqDom(seq.seqDom, seq.typ)
    } yield q"ConstructorSeqTL($domCode, $typCode)"

  }

  def typFamilyPtn[H <: Term with Subs[H],
                   F <: Term with Subs[F],
                   Index <: HList](ptn: TypFamilyPtn[H, F, Index],
                                   typ: Typ[H]): Option[meta.Term] = {
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

  def index[Index](ind: Index): Option[meta.Term] = {
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
      w: Fb): Option[meta.Term] = {
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
      w: Fb): Option[meta.Term] = {
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
    : Option[meta.Term] = {
    import IndexedConstructorSeqDom._
    seqDom match {
      case em: Empty[H, F, Index] =>
        val typ = em.family.someTyp(em.W)
        for {
          WCode      <- codegen(em.W)
          familyCode <- typFamilyPtn(em.family, typ)
        } yield q"IndexedConstructorSeqDom.Empty($WCode, $familyCode)"
      case cons: Cons[a, b, H, F, c, Index, d] =>
        // val name     = s"""HoTT.Name("${cons.name}")"""
        val nameCode = q"""HoTT.Name(${Lit.String(cons.name.toString)})""" //name.parse[meta.Term].get
        for {
          patternCode <- indexedConsShape(cons.pattern, seqDom.W)
          tailCode    <- indexedConsSeqDom(cons.tail)
        } yield
          q"IndexedConstructorSeqDom.Cons($nameCode, $patternCode, $tailCode)"
    }

  }

  def termDefOpt(name: String, term: Term) =
    onTerm(term).map(mkDef(name, _))

  def termDefs(nts: (String, Term)*) = {
    val defs =
      nts.map { case (name, term) => termDefOpt(name, term) }.toList.flatten
    q"..$defs"
  }

  def termObjectOpt(name: String, term: Term) =
    onTerm(term).map(mkDef(name, _))

}

object CodeGen {
  def escape(s: String) = s.replace(""".""", "$")

  def fromNames(indMap: Map[String, meta.Term],
                termMap: Map[String, meta.Term] = Map()) = {
    val inducNames: Term => Option[meta.Term] = {
      case sym: Symbolic => indMap.get(sym.name.toString)
      case _             => None
    }
    CodeGen(inducNames, termMap)
  }

  def objNames(termNames: Vector[String], indNames: Vector[String]) = {
    val defnMap =
      termNames.map { (s) =>
        val code = q"${meta.Term.Name(escape(s))}.value"
        s -> code
      }.toMap
    val indMap =
      indNames.map { (s) =>
        val code: meta.Term = q"${meta.Term.Name(escape(s) + "Ind")}.value"
        s -> code
      }.toMap
    val inducNames: Term => Option[meta.Term] = {
      case sym: Symbolic => indMap.get(sym.name.toString)
      case _             => None
    }
    CodeGen(inducNames, defnMap)
  }

  def mkDef(name: String, code: meta.Term) = {
    val x = Pat.Var(meta.Term.Name(name))
    q"val $x = $code"
  }

  def mkDefs(ncs: (String, meta.Term)*) = {
    val defs = ncs.map { case (name, code) => mkDef(name, code) }.toList
    q"..$defs"
  }

  def mkObject(name: String, code: meta.Term) = {
    val obj = meta.Term.Name(escape(name))
    q"object $obj {val value =  $code}"
  }

  def mkIndObject(name: String, code: meta.Term) =
    mkObject(name + "Ind", code)

  import TermPatterns._

  val base =
    Translator.Empty[Term, meta.Term] ||
      formalAppln >>> {
        case (func, arg) =>
          q"$func($arg)"
      } || funcTyp >>> {
      case (dom, codom) =>
        q"""FuncTyp($dom, $codom)"""
      // q"$dom ->: $codom"
    } || lambdaFixedTriple >>> {
      case ((variable, typ), value) =>
        // q"""lmbda($variable)($value)"""
        q"""lmbda($variable)($value)"""
    } || lambdaTriple >>> {
      case ((variable, typ), value) =>
        q"""lambda($variable)($value)"""
      // q"""lambda($variable)($value)"""
    } || piTriple >>> {
      case ((variable, typ), value) =>
        q"""piDefn($variable)($value)"""
      // q"""$variable ~>: $value"""
    } || sigmaTriple >>> {
      case ((variable, typ), value) =>
        q"""$variable ++ $value"""
    } || universe >>> { (n) =>
      if (n == 0) q"Type" else q"""Universe($n)"""
    } || propUniv >>> {
      case _ => q"Prop"
    } || symbolic >>> {
      case (s, typ) =>
        val name = Lit.String(s)
        // q"$typ.symbObj(Name($name))"
        q"$name :: $typ"
    } || prodTyp >>> { case (first, second) => q"""ProdTyp($first, $second)""" } || pairTerm >>> {
      case (first, second)                  => q"""PairTerm($first, $second)"""
    } || equation >>> { case (lhs, rhs)     => q"IdentityTyp.get($lhs, $rhs)" } || depPairTerm >>> {
      case ((a, b), f)                      => q"DepPair($a, $b, $f)"
    } || plusTyp >>> {
      case (first, scnd) => q"""PlusTyp($first, $scnd)"""
    } || firstIncl >>> {
      case (typ, value) => q"PlusTyp.FirstIncl($typ, $value)"
    } || secondIncl >>> {
      case (typ, value) => q"PlusTyp.ScndIncl($typ, $value)"
    } || refl >>> {
      case (typ, term) => q"Refl($typ, $term)"
    }

  def getName(t: Term): Option[String] = t match {
    case s: Symbolic => Some(s.name.toString)
    case _           => None
  }

  def indName(t: Term): Option[meta.Term] =
    getName(t).map((name) => meta.Term.Name(s"${name}Ind"))
}
