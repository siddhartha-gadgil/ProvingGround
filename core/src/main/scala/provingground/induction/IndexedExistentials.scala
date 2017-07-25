package provingground.induction

import provingground._, HoTT._
import shapeless._
import scala.language.existentials

sealed trait TypFamilyExst {
  import TypFamilyPtn._
  type Fb <: Term with Subs[Fb]
  type Index <: HList

  val pattern: TypFamilyPtn[Term, Fb, Index]

  val W: Fb

  implicit val subst: TermList[Index]

  def lambdaExst[TT <: Term with Subs[TT]](variable: TT, dom: Typ[TT]) =
    TypFamilyExst(
      DepFuncTypFamily(dom, (t: TT) => pattern.subs(variable, t)),
      variable :~> W
    )

  def ~>:[TT <: Term with Subs[TT]](variable: TT) =
    TypFamilyExst(
      DepFuncTypFamily(variable.typ.asInstanceOf[Typ[TT]],
                       (t: TT) => pattern.subs(variable, t)),
      variable :~> W
    )

  def ->:[TT <: Term with Subs[TT]](dom: Typ[TT]) =
    TypFamilyExst(FuncTypFamily(dom, pattern), dom.Var :-> W)

  def mapsTo[TT <: Term with Subs[TT]](variable: TT, dom: Typ[TT]) = {
    val fmly = W match {
      case FormalAppln(f, x) if x == variable => f.asInstanceOf[Func[TT, Fb]]
      case _                                  => variable :-> W
    }
    TypFamilyExst(FuncTypFamily(dom, pattern), fmly)
  }
// Inner existentials

  trait IndexedIterFuncExst {
    import IndexedIterFuncShape._
    // type O <: Term with Subs[O]

    type F <: Term with Subs[F]

    val shape: IndexedIterFuncShape[Term, F, Fb, Index]

    def piShape[TT <: Term with Subs[TT]](variable: TT, dom: Typ[TT]) = {
      DepFuncShape(dom, (t: TT) => shape.subs(variable, t))
    }

    def piWrap[TT <: Term with Subs[TT]](variable: TT, dom: Typ[TT]) =
      IndexedIterFuncExst(piShape(variable, dom))

    def ->:[TT <: Term with Subs[TT]](dom: Typ[TT]) =
      IndexedIterFuncExst(FuncShape(dom, shape))
  }

  object IndexedIterFuncExst {
    import IndexedIterFuncShape._

    def apply[Fm <: Term with Subs[Fm]](
        sh: IndexedIterFuncShape[Term, Fm, Fb, Index]) =
      new IndexedIterFuncExst {
        type F = Fm
        val shape = sh
      }

    def getIterFuncShape(fmly: Typ[Term]): IndexedIterFuncExst = fmly match {
      case ft: FuncTyp[u, v] => ft.dom ->: getIterFuncShape(ft.codom)
      case pd: PiDefn[u, v] =>
        val targWrap = getIterFuncShape(pd.value)
        targWrap.piWrap(pd.variable, pd.domain)
      case tp: Typ[u] =>
        IndexedIterFuncExst(
          IdIterShape[Term, Fb, Index](pattern, pattern.getIndex(W, tp).get))
    }
  }

  sealed trait IndexedConstructorShapeExst {
    type S <: HList
    type ConstructorType <: Term with Subs[ConstructorType]

    val value: IndexedConstructorShape[S, Term, Fb, ConstructorType, Index]

    def :::(name: AnySym) = name ::: value

    import IndexedConstructorShape._

    def -->>:(that: IndexedIterFuncExst, ind: Index) =
      IndexedConstructorShapeExst {
        IndexedIndexedFuncConsShape(that.shape, value, ind)
      }

    def ->>:[T <: Term with Subs[T]](tail: Typ[T]) =
      IndexedConstructorShapeExst {
        IndexedCnstFuncConsShape(tail, value)
      }

    def ~>>:[T <: Term with Subs[T]](tailVar: T) = {
      val fibre = (t: T) => value.subs(tailVar, t)
      IndexedConstructorShapeExst {
        IndexedCnstDepFuncConsShape(tailVar.typ.asInstanceOf[Typ[T]], fibre)
      }
    }
  }

  object IndexedConstructorShapeExst {
    def apply[SI <: HList,
              ConstructorTypeI <: Term with Subs[ConstructorTypeI]](
        shape: IndexedConstructorShape[SI, Term, Fb, ConstructorTypeI, Index]) =
      new IndexedConstructorShapeExst {
        type S               = SI
        type ConstructorType = ConstructorTypeI

        lazy val value = shape
      }
    def getIndexedConstructorShape(
        cnstTyp: Typ[Term]): IndexedConstructorShapeExst =
      pattern
        .getIndex(W, cnstTyp)
        .map { (ind: Index) =>
          apply(IndexedConstructorShape.IndexedIdShape(pattern, ind))
        }
        .getOrElse {
          cnstTyp match {
            case ft: FuncTyp[u, v] if (ft.dom.indepOf(W)) =>
              // println(s"apparently ${ft.dom} is independent of $W")
              ft.dom ->>: getIndexedConstructorShape(ft.codom)
            case pd: PiDefn[u, v] if (pd.domain.indepOf(W)) =>
              pd.variable ~>>: getIndexedConstructorShape(pd.value)
            case ft: FuncTyp[u, v] =>
              val ind = pattern.getIndex(W, ft.dom).get
              getIndexedConstructorShape(ft.codom)
                .-->>:(IndexedIterFuncExst.getIterFuncShape(ft.dom), ind)
            case _ =>
              println(cnstTyp)
              println(pattern.getIndex(W, cnstTyp))
              println(W)
              println(pattern)
              ???
          }
        }
  }

  trait IndexedConstructorSeqExst {
    type SS <: HList

    type Intros <: HList

    val value: IndexedConstructorSeqDom[SS, Term, Fb, Index, Intros]

    def |:[HShape <: HList, HC <: Term with Subs[HC]](
        head: IndexedConstructor[HShape, Term, Fb, HC, Index]) =
      IndexedConstructorSeqExst {
        head |: value
      }
  }

  object IndexedConstructorSeqExst {
    def apply[SSS <: HList, IIntros <: HList](
        seq: IndexedConstructorSeqDom[SSS, Term, Fb, Index, IIntros]) =
      new IndexedConstructorSeqExst {
        type SS = SSS

        type Intros = IIntros

        lazy val value = seq
      }

    lazy val empty = IndexedConstructorSeqExst {
      IndexedConstructorSeqDom.Empty(W, pattern)
    }

    def getIndexedConstructorSeq(
        intros: Vector[Term]): IndexedConstructorSeqExst = {
      intros match {
        case Vector() => empty
        case l =>
          val x    = l.head
          val ys   = l.tail
          val name = x.asInstanceOf[Symbolic].name.toString
          val head =
            name ::: IndexedConstructorShapeExst.getIndexedConstructorShape(
              x.typ)
          head |: getIndexedConstructorSeq(ys)
      }
    }
  }
}

object TypFamilyExst {
  import TypFamilyPtn._
  def apply[Fib <: Term with Subs[Fib], In <: HList: TermList](
      tf: TypFamilyPtn[Term, Fib, In],
      w: Fib) =
    new TypFamilyExst {
      type Fb    = Fib
      type Index = In

      lazy val subst = implicitly[TermList[Index]]

      lazy val pattern = tf

      lazy val W = w
    }

  def getFamily[Fb <: Term with Subs[Fb]](w: Fb): TypFamilyExst = w match {
    case tp: Typ[u] => TypFamilyExst[Typ[Term], HNil](IdTypFamily[Term], tp)
    case fn: Func[u, v] =>
      val x = fn.dom.Var
      getFamily(fn(x)).mapsTo(x, fn.dom)
    case g: FuncLike[u, v] =>
      val x = g.dom.Var
      x ~>: getFamily(g(x))
  }

  def getIndexedConstructorSeq[Fb <: Term with Subs[Fb]](
      w: Fb,
      intros: Vector[Term]) = {
    getFamily(w).IndexedConstructorSeqExst.getIndexedConstructorSeq(intros)
  }

  // def getTerms: HList => Vector[Term] = {
  //   case HNil                 => Vector()
  //   case (head: Term) :: tail => head +: getTerms(tail)
  // }
}
