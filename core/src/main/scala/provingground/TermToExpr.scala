package provingground

import HoTT._

class TermToExpr[E](
    univ: Int => E, predef: Term => Option[E] = (t: Term) => None)(
    implicit l: ExprLang[E]) {
  def expr: Term => Option[E] = {
    case term if !predef(term).isEmpty => predef(term)
    case FormalAppln(func, arg) =>
      for (f <- expr(func); x <- expr(arg); fx <- l.appln(f, x)) yield fx
    case LambdaFixed(x: Term, y: Term) =>
      for (xe <- expr(x); ye <- expr(y); result <- l.lambda(xe, ye)) yield
        result
    case Lambda(x: Term, y: Term) =>
      for (xe <- expr(x); ye <- expr(y); result <- l.lambda(xe, ye)) yield
        result
    case pt: PiTyp[u, v] =>
      val x = pt.fibers.dom.Var
      for (xe <- expr(x); ye <- expr(pt.fibers(x)); result <- l.pi(xe, ye)) yield
        result
    case st: SigmaTyp[u, v] =>
      val x = st.fibers.dom.Var
      for (xe <- expr(x); ye <- expr(st.fibers(x)); result <- l.sigma(xe, ye)) yield
        result
    case PlusTyp(first: Typ[u], scnd: Typ[v]) =>
      for (xe <- expr(first.Var); ye <- expr(scnd); result <- l.or(xe, ye)) yield
        result
    case p: AbsPair[_, _] =>
      for (xe <- expr(p.first); ye <- expr(p.second); result <- l.pair(xe, ye)) yield
        result
    case fn: FuncTyp[_, _] =>
      for (xe <- expr(fn.dom.Var); ye <- expr(fn.codom); result <- l.pi(xe, ye)) yield
        result
    case sym: Symbolic with Term =>
      outerSym(sym).name match {
        case Name(name) =>
          for (typ <- expr(sym.typ); result <- l.variable(name, typ)) yield
            result
//        case inn: InnerSym[_] => expr(inn.variable)
        case _ => None
      }
    case IdentityTyp(dom, lhs: Term, rhs: Term) =>
      for (xe <- expr(lhs); ye <- expr(rhs); result <- l.equality(xe, ye)) yield
        result
    case Universe(n) =>
      Some(univ(n))
    case PlusTyp.FirstIncl(typ, value: Term) =>
      for (tp <- expr(typ); x <- expr(value); i <- l.incl1(tp);
           result <- l.appln(i, x)) yield result
    case PlusTyp.ScndIncl(typ, value: Term) =>
      for (tp <- expr(typ); x <- expr(value); i <- l.incl2(tp);
           result <- l.appln(i, x)) yield result
    case Unit => l.tt
    case Star => l.qed
    case Zero => l.ff
  }

  def apply(term: Term) = expr(term)
}

object TermToExpr {
  import TermLang._

  import IdentityTyp._

  val (reflFn, formalRefl) = {
    val X = "X" :: Type
    val x = "x" :: X
    (HoTT.lambda(X)(HoTT.lambda(x)(Refl(X, x))),
      "@refl" :: (
        HoTT.pi(X)(HoTT.pi(x)(x =:= x))
       ))
  }

  val (idRec, formalIdRec) = {
    val X = "X" :: Type
    val Y = "Y" :: Type
    val idRec = HoTT.lambda(X)(HoTT.lambda(Y)(rec(X, Y) : Term))
    val formal =  HoTT.lambda(X)(HoTT.lambda(Y)("@id.rec" :: idRec(X)(Y).typ))
    (idRec, formal)
  }

  val (idInduc, formalIdInduc) = {
    val X = "X" :: Type
    val a = "a" :: X
    val b = "b" :: X
    val p = "p" :: (a =:= b)

    val Ys =
      "fmly" :: (
          a ~>: (b ~>: (p ~>: Type))
        )
//    val fmly = HoTT.lambda(a)(HoTT.lambda(b)(HoTT.lambda(p)(Ys(a)(b)(p))))
    val idInduc= HoTT.lambda(X)(HoTT.lambda(Ys)(induc(X, Ys)))
    val formal =  HoTT.lambda(X)(HoTT.lambda(Ys)("@id.induc" :: idInduc(X)(Ys).typ))
    (idInduc, formal)
  }


  def encode(names: Vector[(Term, String)]): Term => Term = {
    def formalDefs: Term => Option[Term] = {
      case Refl(t : Typ[u], a: Term) =>
        import Fold._
        val newTyp = encode(names)(t)
        val newPoint = encode(names)(a)
        Some(formalRefl(newTyp)(newPoint))
      case sym: Symbolic => sym.name match {
        case ind : InducFunc[u, v] =>
          val newDom = encode(names)(ind.dom)
          val newTgt = encode(names)(ind.targetFmly)
          import Fold._
          val newFormalIdInduc = encode(names)(formalIdInduc)
          Some((newFormalIdInduc(newDom)(newTgt)))
        case RecFunc(dom: Typ[u], codom: Typ[v]) =>
          import Fold._
          Some((formalIdRec(encode(names)(dom))(encode(names)(codom))))
        case _ => None
      }
      case _ => None
    }

    def predefs(term: Term): Option[Term] = {
      val nameOpt = names find (_._1 == term) map (_._2)
      nameOpt flatMap {(name) => {
        def typOpt: Option[Typ[Term]] = encode(names)(term.typ) match {
          case tp: Typ[u] => Some(tp)
          case _ => None
        }
        typOpt map ((typ) => s"@$name" :: typ)
      }
      }
    }
    val rebuilder = new TermToExpr((n) => Universe(n), (term) => formalDefs(term) orElse predefs(term))
    (term: Term) =>
      rebuilder(term) getOrElse (term)
  }

  def decode(names: Vector[(Term, String)]): Term => Term = {
    val formalNames = Vector(reflFn -> "refl", idInduc ->"id.induc", idRec -> "id.rec") ++ names
    def predefs(term: Term): Option[Term] = term match {
      case sym: Symbolic =>
        formalNames find ("@" + _._2 == sym.name.toString) map (_._1)
      case _ => None
    }
    val rebuilder = new TermToExpr((n) => Universe(n), predefs)
    (term: Term) =>
      rebuilder(term) getOrElse (term)
  }

  class NewNameFactory(prefix: String = "$") {

    var name: String = ""

    def get = {
      val newname = nextName(name)

      name = newname

      prefix + newname
    }

    val termNames: scala.collection.mutable.Map[Term, String] =
      scala.collection.mutable.Map()

    def getName(t: Term) =
      termNames.get(t) getOrElse {
        val name = get
        termNames += (t -> name)
        name
      }

    def getTerm(t: Typ[Term]) = getName(t) :: t
  }

  import TermLang._

  def newTermOpt(term: Term, prefix: String = "."): Option[Term] = {
    val myNames = new NewNameFactory(prefix)
    def predefs(t: Term) =
      if (isVar(t)) Some(myNames.getTerm(rebuildTyp(t.typ, prefix))) else None
    val rebuilder = new TermToExpr((n) => Universe(n), predefs)
    rebuilder(term)
  }

  def rebuild(t: Term, prefix: String = ".") = newTermOpt(t, prefix).get

  def rebuildTyp(t: Typ[Term], prefix: String) =
    rebuild(t, prefix).asInstanceOf[Typ[Term]]

  def rebuildList(ts: List[Term], prefix: String = ".") = {
    val myNames = new NewNameFactory(prefix)
    def predefs(t: Term) =
      if (isVar(t)) Some(myNames.getTerm(rebuildTyp(t.typ, prefix))) else None
    val rebuilder = new TermToExpr[Term]((n) => Universe(n), predefs)
    def recc: List[Term] => List[Term] = {
      case List() => List()
      case x :: ys =>
        val xx = rebuilder(x).get
        xx :: recc(ys)
    }
    recc(ts)
  }

  def rebuildMap[U <: Term with Subs[U]](
      m: Map[Term, Set[(U, Term)]], prefix: String = ".") = {
    val list =
      m.toList map {
        case (x, s) =>
          val myNames = new NewNameFactory(prefix)
          def predefs(t: Term) =
            if (isVar(t)) Some(myNames.getTerm(rebuildTyp(t.typ, prefix)))
            else None
          val rebuilder = new TermToExpr[Term]((n) => Universe(n), predefs)
          val xx = rebuilder(x).get
          val ss =
            s map {
              case (x, y) =>
                (rebuilder(x).get.asInstanceOf[U], rebuilder(y).get)
            }
          (xx, ss)
      }
    list.toMap
  }
}
