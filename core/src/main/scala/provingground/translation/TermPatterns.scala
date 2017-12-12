package provingground.translation

import Functors._

import cats.implicits._

import cats._

import provingground._, HoTT._

import Translator._

import induction._

/**
  * Patterns, in the sense of [[Translator.Pattern]], as well as some builders
  * for various kinds of HoTT terms. Includes matching recursively and inductively defined functions.
  */
object TermPatterns {

  /**
    * matches formal applications
    */
  val formalAppln = Pattern[Term, II](FormalAppln.unapply)

  /**
    * matches lambda definitions
    */
  val lambdaAppln = Pattern.partial[Term, II] {
    case l: LambdaLike[u, v] => (l.variable, l.value)
  }

  /**
    * matches lambda applications and returns HoTT-type of the variable as well
    */
  val lambdaTriple = Pattern.partial[Term, III] {
    case l: LambdaLike[u, v] => ((l.variable, l.variable.typ), l.value)
  }

  /**
    * matches Pi-Types, returns in the pi-lambda form  with the HoTT-type of variable included.
    */
  val piTriple = Pattern.partial[Term, III] {
    case PiDefn(x: Term, y: Typ[u]) => ((x, x.typ), y)
    case PiTyp(fibre: Func[u, _]) =>
      val x: Term = fibre.dom.Var.asInstanceOf[Term]
      ((x, x.typ), fibre(x))
  }

  /**
    * matches Sigma-Types, returns in the pi-lambda form  with the HoTT-type of variable included.
    */
  val sigmaTriple = Pattern.partial[Term, III] {
    case SigmaTyp(fibre: Func[u, _]) =>
      fibre match {
        case l: LambdaLike[u, v] => ((l.variable, l.variable.typ), l.value)
        case _ =>
          val x: Term = fibre.dom.Var.asInstanceOf[Term]
          ((x, x.typ.asInstanceOf[Typ[Term]]), fibre(x))
      }
  }

  /**
    * matches Pi-Type and returns the fibre
    */
  val piTyp = Pattern.partial[Term, Id] {
    case PiDefn(x: Term, y: Typ[v]) => HoTT.lmbda(x)(y)
    case PiTyp(fibre)               => fibre
  }

  /**
    * matches Pi-Type and returns the  fibre as a lambda
    */
  val piLam = Pattern.partial[Term, II] {
    case PiDefn(x: Term, y: Typ[u]) => (x, y)
    case PiTyp(fibre: Func[u, _]) =>
      val x: Term = fibre.dom.Var.asInstanceOf[Term]
      (x, fibre(x))
  }

  /**
    * matches Sigma-Type and returns the fibre
    */
  val sigmaTyp = Pattern.partial[Term, cats.Id] {
    case SigmaTyp(fibre) => fibre
  }

  /**
    * matches Sigma-Type and returns fibre as lambda
    */
  val sigmaLam = Pattern.partial[Term, II] {
    case SigmaTyp(fibre: Func[u, _]) =>
      val x: Term = fibre.dom.Var.asInstanceOf[Term]
      (x, fibre(x))
  }

  /**
    * matches coproduct type
    */
  val plusTyp = Pattern.partial[Term, II] {
    case PlusTyp(first: Typ[u], second: Typ[v]) => (first, second)
  }

  /**
    * matches all pairs
    */
  val absPair = Pattern.partial[Term, II] {
    case p: AbsPair[u, v] => (p.first, p.second)
  }

  /**
    * matches product types
    */
  val prodTyp = Pattern.partial[Term, II] {
    case ProdTyp(first: Typ[u], second: Typ[v]) => (first, second)
  }

  /**
    * matches function types, returning domain and codomain
    */
  val funcTyp = Pattern.partial[Term, II] {
    case FuncTyp(dom: Typ[u], codom: Typ[v]) => (dom, codom)
  }

  /**
    * matches recursively defined function, returns domain, codomain and definition data
    */
  val recFunc = Pattern.partial[Term, IIV] {
    case rf: RecFunc[u, v] => (rf.dom, (rf.codom, rf.defnData))
  }

  /**
    * matches idexed recursively defined function, returns index, domain, codomain and definition data
    */
  val indRecFunc = Pattern.partial[Term, VIIV] {
    case rf: IndRecFunc[u, v, w] =>
      (rf.index, (rf.dom, (rf.codom, rf.defnData)))
  }

  /**
    * matches inductively defined function, returns domain, codomain and definition data
    */
  val inducFunc = Pattern.partial[Term, IIV] {
    case rf: InducFuncLike[u, v] =>
      val fmly: Term = rf.depcodom match {
        case t: Term => t
        case _ =>
          val x = rf.dom.Var
          x :-> rf.depcodom(x)
      }
      (rf.dom, (fmly, rf.defnData))
  }

  /**
    * matches indexed inductively defined function, returns index, domain, codomain and definition data
    */
  val indInducFunc = Pattern.partial[Term, VIIV] {
    case rf: IndInducFuncLike[u, v, w, z] =>
      // val fmly: Term = rf.depcodom match {
      //   case t: Term => t
      //   case _ =>
      //     val x = rf.dom.Var
      //     x :-> rf.depcodom(x)
      // }
      (rf.index, (rf.dom, (rf.codXs, rf.defnData)))
  }

  /**
    * matches identity (equality) type, returns domain, lhs and rhs
    */
  val identityTyp = Pattern.partial[Term, III] {
    case IdentityTyp(dom: Typ[u], lhs: Term, rhs: Term) => ((dom, lhs), rhs)
  }

  /**
    * matches identity (equality) type, returns lhs and rhs
    */
  val equation = Pattern.partial[Term, II] {
    case IdentityTyp(dom: Typ[u], lhs: Term, rhs: Term) => (lhs, rhs)
  }

  /**
    * matches `i_1(a)`, the first inclusion function, returns the type and value
    */
  val firstIncl = Pattern.partial[Term, II] {
    case fi: PlusTyp.FirstIncl[u, v] => (fi.typ, fi.value)
  }

  /**
    * matches `i_2(a)`, the second inclusion function, returns the type and value
    */
  val secondIncl = Pattern.partial[Term, II] {
    case fi: PlusTyp.ScndIncl[u, v] => (fi.typ, fi.value)
  }

  val refl = Pattern.partial[Term, II] {
    case Refl(dom: Typ[u], value: Term) => (dom, value)
  }

  /**
    * matches  `Star`, the only element of the true type `Unit`, returns `() : Unit` if matched
    */
  val star = Pattern.check[Term](_ == Star)

  /**
    * matches the true type `Unit`, returns `() : Unit` if matched
    */
  val unit = Pattern.check[Term](_ == Unit)

  /**
    * matches  `Zero`, the false  type, returns `() : Unit` if matched
    */
  val zero = Pattern.check[Term](_ == Zero)

  /**
    * matches `Universe(n)`, returns the level `n`
    */
  val universe = Pattern.partial[Term, N] {
    case Universe(n) => n
  }

  /**
    * matches  `Prop`, the false  type, returns `() : Unit` if matched
    */
  val prop = Pattern.check[Term] {
    case _: Prop.type => true
    case _            => false
  }

  /**
    * matches a symbolic name, perhaps wrapped in `InnerSym`, returns `Name`
    */
  val symbolic = Pattern.partial[Term, Named] {
    case sym: Symbolic with Term =>
      outerSym(sym).name match {
        case Name(name) => (name, sym.typ)
      }
  }(namedTrav)

  /**
    * matches a symbolic name, perhaps wrapped in `InnerSym`, returns the name as a `String`
    */
  val symName = Pattern[Term, S] {
    case sym: Symbolic with Term =>
      outerSym(sym).name match {
        case Name(name) => Some(name)
        case _          => None
      }
    case _ => None
  }

  def termToExprRaw[E: ExprLang] = {
    import ExprLang._
    (formalAppln >> appln[E]) || (lambdaAppln >> lambda[E]) ||
    (prodTyp >> pairTyp[E]) || (funcTyp >> func[E]) || (piLam >> pi[E]) ||
    (sigmaLam >> sigma[E]) || (equation >> equality[E]) ||
    (symbolic >> variable[E]) || (plusTyp >> or[E]) || (funcTyp >> func[E]) ||
    (prodTyp >> pairTyp[E]) || (absPair >> pair[E]) ||
    (unit >> { (_) =>
      tt[E]
    }) ||
    (zero >> { (_) =>
      ff[E]
    }) ||
    (star >> { (_) =>
      qed[E]
    }) || (firstIncl >> i1[E]) || (secondIncl >> i2[E])
  }

  def termToExpr[E: ExprLang](univ: Int => Option[E]) = {
    (universe >> univ) || termToExprRaw[E]
  }

  import TermLang.applyAll

  /**
    * returns recursively defined function as function of domain, codomain and definition data
    * @param inds inductive types defined (other that products etc)
    */
  def buildRecDef(inds: Typ[Term] => Option[ConstructorSeqTL[_, _, _]] = (_) =>
    None): (Term, (Term, Vector[Term])) => Option[Term] = {
    case (pt: ProdTyp[u, v], (codom: Typ[w], data)) =>
      applyAll(Some(pt.rec(codom)), data)
    case (Zero, (codom: Typ[w], data)) =>
      applyAll(Some(Zero.rec(codom)), data)
    case (Unit, (codom: Typ[w], data)) =>
      applyAll(Some(Unit.rec(codom)), data)
    case (pt: PlusTyp[u, v], (codom: Typ[w], data)) =>
      applyAll(Some(pt.rec(codom)), data)
    case (pt: SigmaTyp[u, v], (codom: Typ[w], data)) =>
      applyAll(Some(pt.rec(codom)), data)
    case (dom: Typ[u], (codom: Typ[v], data)) =>
      inds(dom) flatMap ((cs) => applyAll(Some(cs.recE(codom)), data))
    case _ => None
  }

  /**
    * returns indexed recursively defined function as function of domain, codomain and definition data
    * @param inds indexed inductive types defined (other than equality)
    */
  def buildIndRecDef(
      inds: Term => Option[IndexedConstructorSeqDom[_, Term, _, _, _]] = (_) =>
        None): (Vector[Term], (Term, (Term, Vector[Term]))) => Option[Term] = {
    case (Vector(start, finish),
          (idt: IdentityTyp[u], (codom: Typ[v], Vector(fn: Func[x, y])))) =>
      val rf = idt.rec(codom)
      applyAll(Some(rf), Vector(fn, start, finish))
    case (index, (dom, (codom: Typ[v], data))) =>
      inds(dom) flatMap ((cs) => applyAll(Some(cs.recE(codom)), data ++ index))
  }

  def fm[U <: Term with Subs[U]](dom: Typ[U], fmly: Term) = {
    val x  = dom.Var
    val tp = fold(fmly)(x)
    tp match {
      case t: Typ[_] => x :-> t
    }
  }

  /**
    * returns inductively defined function as function of domain, codomain and definition data
    * @param inds inductive types defined (other than products etc)
    */
  def buildIndDef(
      inds: Typ[Term] => Option[ConstructorSeqTL[_, Term, _]] = (_) => None)
    : (Term, (Term, Vector[Term])) => Option[Term] = {
    case (pt: ProdTyp[u, v], (depcodom, data)) =>
      val x    = pt.first.Var
      val y    = pt.second.Var
      val tp   = fold(fold(depcodom)(x))(y).asInstanceOf[Typ[Term]]
      val fmly = x :-> (y :-> tp)
      applyAll(Some(pt.induc(fmly)), data)
    case (Zero, (depcodom, data)) =>
      applyAll(Some(Zero.induc(fm(Zero, depcodom))), data)
    case (Unit, (depcodom, data)) =>
      applyAll(Some(Unit.induc(fm(Unit, depcodom))), data)
    case (pt: PlusTyp[u, v], (depcodom, data)) =>
      applyAll(Some(pt.induc(fm(pt, depcodom))), data)
    case (pt: SigmaTyp[u, v], (depcodom, data)) =>
      val x    = pt.fibers.dom.Var
      val y    = pt.fibers(x).Var
      val tp   = fold(fold(depcodom)(x))(y).asInstanceOf[Typ[Term]]
      val fmly = x :-> (y :-> tp)
      applyAll(Some(pt.induc(fmly)), data)
    case (dom: Typ[u], (depcodom, data)) =>
      inds(dom) flatMap ((cs) =>
        applyAll(Some(cs.inducE(fm(cs.typ, depcodom))), data))
    case _ => None
  }

  /**
    * returns indexed inductively defined function as function of domain, codomain and definition data
    * @param inds inductive types defined (other than equality)
    */
  def buildIndIndDef(
      inds: Term => Option[IndexedConstructorSeqDom[_, Term, _, _, _]] = (_) =>
        None): (Vector[Term], (Term, (Term, Vector[Term]))) => Option[Term] = {
    case (Vector(start, finish),
          (idt: IdentityTyp[u], (depcodom, Vector(fn: FuncLike[x, y])))) =>
      // println("matching identity\n")
      val rf = idt.induc(
        depcodom
          .asInstanceOf[FuncLike[u, FuncLike[u, FuncLike[Term, Typ[Term]]]]])
      // println(s"got ind $rf")
      applyAll(Some(rf), Vector(fn, start, finish))
    case (index, (dom, (depcodom, data))) =>
      inds(dom) flatMap ((cs) =>
        applyAll(Some(cs.inducE(depcodom)), data ++ index))
  }
  // val blah: Term => Boolean = {case x : PlusTyp[u, v]#RecFn[w] => true}
}
