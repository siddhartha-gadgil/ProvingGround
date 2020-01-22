package provingground.interface

import provingground.{Context, _}
import translation._
import Translator.unmatched

import scala.language.higherKinds
import cats._
import cats.implicits._
import provingground.induction.{ExstInducDefn, ExstInducStrucs}
import provingground.scalahott.NatRing

import scala.util.matching.Regex

trait MsgFunc[F[_]] {
  def encode(t: F[upack.Msg]): upack.Msg

  def decode(msg: upack.Msg): F[upack.Msg]
}

object MsgFunc {
  implicit val idJS: MsgFunc[Id] = new MsgFunc[Id] {
    def encode(t: upack.Msg): upack.Msg = t

    def decode(msg: upack.Msg): upack.Msg = msg
  }

  import Functors._

  implicit val intMsg: MsgFunc[N] = new MsgFunc[N] {
    def encode(t: Int) = upack.Int32(t)

    def decode(msg: upack.Msg): Int = msg.int32.toInt
  }

  implicit val strMsg: MsgFunc[S] = new MsgFunc[S] {
    def encode(t: String) = upack.Str(t)

    def decode(msg: upack.Msg): String = msg.str
  }

  implicit val unitMsg: MsgFunc[Un] = new MsgFunc[Un] {
    def encode(t: Unit): upack.Null.type = upack.Null

    def decode(msg: upack.Msg): Unit = ()
  }

  implicit val vecMsg: MsgFunc[Vector] = new MsgFunc[Vector] {
    def encode(t: Vector[upack.Msg]) = upack.Arr(t: _*)

    def decode(msg: upack.Msg): Vector[upack.Msg] = msg.arr.toVector
  }

  implicit def pairJS[X[_], Y[_]](
      implicit xMsg: MsgFunc[X],
      yMsg: MsgFunc[Y]): MsgFunc[({ type Z[A] = (X[A], Y[A]) })#Z] =
    new MsgFunc[({ type Z[A] = (X[A], Y[A]) })#Z] {
      def encode(t: (X[upack.Msg], Y[upack.Msg])) =
        upack.Obj(upack.Str("1") -> xMsg.encode(t._1), upack.Str("2") -> yMsg.encode(t._2))

      def decode(msg: upack.Msg): (X[upack.Msg], Y[upack.Msg]) =
        (xMsg.decode(msg.obj(upack.Str("1"))), yMsg.decode(msg.obj(upack.Str("2"))))
    }

  import Translator._

  def toMsg[I, F[_]](pat: Pattern[I, F])(name: String, header: upack.Msg = upack.Str("In"))(
      implicit msgF: MsgFunc[F]): Translator[I, upack.Msg] =
    pat >>> { (msg) =>
      upack.Obj(header -> upack.Str(name), upack.Str("T") -> msgF.encode(msg))
    }

  def msgToOpt[I, F[_]: Traverse](name: String, header: upack.Msg = upack.Str("In"))(
      build: F[I] => Option[I])(
      implicit msgF: MsgFunc[F]): Translator[upack.Msg, I] = {
    val pat = Pattern[upack.Msg, F] { (msg) =>
      if (msg.obj(header) == upack.Str(name)) Some(msgF.decode(msg.obj(upack.Str("T")))) else None
    }
    pat >> build
  }

  def msgToBuild[I, F[_]: Traverse](name: String, header: upack.Msg = upack.Str("In"))(
      build: F[I] => I)(implicit msgF: MsgFunc[F]): Translator[upack.Msg, I] = {
    val pat = Pattern[upack.Msg, F] { (msg) =>
      if (msg.obj(header) == upack.Str(name)) Some(msgF.decode(msg.obj(upack.Str("T")))) else None
    }
    pat >>> build
  }

}

import TermPatterns._

import HoTT._

import Functors._

object TermPack {
  import MsgFunc._

  implicit val travNamed: Traverse[Named] = traversePair[S, Id]

  val termToMsg: Translator.OrElse[Term, upack.Msg] =
    toMsg(universe)("U") ||
      toMsg(formalAppln)("Ap") ||
      toMsg(lambdaTriple)("\u03bb") ||
      toMsg(sigmaTriple)("\u03c3") ||
      toMsg(piTriple)("\u03c0") ||
      toMsg(prodTyp)("\u03a0") ||
      toMsg(absPair)("pair") ||
      toMsg(plusTyp)("\u03a3") ||
      toMsg(funcTyp)(UnicodeSyms.Arrow) ||
      toMsg(star)("*") ||
      toMsg(unit)("One") ||
      toMsg(zero)("Void") ||
      toMsg(prop)("Prop") ||
      toMsg(indInducFunc)("IInd") ||
      toMsg(indRecFunc)("IRec") ||
      toMsg(recFunc)("Rec") ||
      toMsg(inducFunc)("Ind") ||
      toMsg(hashSymbolic)("Sym") ||
      toMsg(mereWitness)("Witness") ||
      toMsg(firstIncl)("i1") ||
      toMsg(secondIncl)("i2") ||
      toMsg(identityTyp)("=") ||
      toMsg(refl)("=") ||
      toMsg(natTyp)("Nat") ||
      toMsg(natUniv)("NatU") ||
      toMsg(natZero)("0N") ||
      toMsg(natSucc)("succN") ||
      toMsg(natSum)("+N") ||
      toMsg(natProd)("*N") ||
      toMsg(natLiteral)("NL") ||
      toMsg(natAddMorph)("nat-additive-morphism") ||
      toMsg(foldedTerm)("folded-term") ||
      toMsg(miscAppln)("Ap")

  def termToMsgGet(t: Term) =
    termToMsg(t).getOrElse(throw new Exception(s"cannot serialize term $t"))

  def fdMsg(fd: FiniteDistribution[Term]): upack.Arr = {
    val pmf = for {
      Weighted(elem, p) <- fd.pmf
      tmsg               <- termToMsg(elem)
    } yield upack.Obj(upack.Str("term") -> tmsg, upack.Str("weight") -> upack.Float64(p))
    upack.Arr(pmf: _*)
  }

  import induction._

  def msgToTerm(
      inds: Typ[Term] => Option[ConstructorSeqTL[_, Term, _]] = (_) => None,
      indexedInds: Term => Option[IndexedConstructorSeqDom[_, Term, _, _, _]] =
        (_) => None): Translator.OrElse[upack.Msg, Term] =
    msgToTermBase ||
      msgToOpt[Term, IIV]("Rec") {
        case (x, (y, v)) =>
          buildRecDef(inds)(x, (y, v))
      } ||
      msgToOpt[Term, IIV]("Ind") {
        case (x, (y, v)) => buildIndDef(inds)(x, (y, v))
      } ||
      msgToOpt[Term, IVIIV]("IRec") {
        case (u, (w, (x, (y, v)))) =>
          buildIndRecDef(indexedInds)(w, (x, (y, v)))
      } ||
      msgToOpt[Term, IVIIV]("IInd") {
        case (u, (w, (x, (y, v)))) =>
          buildIndIndDef(indexedInds)(w, (x, (y, v)))
      }

  def msgToTermExst(exst: ExstInducStrucs): Translator.OrElse[upack.Msg, Term] =
    msgToTermBase ||
      msgToOpt[Term, IIV]("Rec") {
        case (dom: Term, (cod: Term, data: Vector[Term])) =>
          for {
            codom <- typOpt(cod)
            fn    <- exst.recOpt(dom, codom)
          } yield data.foldLeft(fn)(fold(_)(_))
        //buildRecDef(inds)(x, (y, v))
      } ||
      msgToOpt[Term, IIV]("Ind") {
        case (dom: Term, (cod: Term, data: Vector[Term])) =>
          for {
            fn <- exst.inducOpt(dom, cod)
          } yield data.foldLeft(fn)(fold(_)(_))
      } ||
      msgToOpt[Term, IVIIV]("IRec") {
        case (_,
              (index: Vector[Term],
               (dom: Term, (cod: Term, data: Vector[Term])))) =>
          for {
            codom <- typOpt(cod)
            fn    <- exst.recOpt(dom, codom)
          } yield
            (data ++ index).foldLeft(fn)(fold(_)(_)) 
      } ||
      msgToOpt[Term, IVIIV]("IInd") {
        case (_,
              (index: Vector[Term],
               (dom: Term, (cod: Term, data: Vector[Term])))) =>
          for {
            fn <- exst.inducOpt(dom, cod)
          } yield (data ++ index).foldLeft(fn)(fold(_)(_))
        
      }

  def msgToFD(exst: ExstInducStrucs)(
      msg: upack.Msg): FiniteDistribution[Term] = {
    val pmf =
      msg.arr.toVector.map { wp =>
        Weighted(
          msgToTermExst(exst)(wp.obj(upack.Str("term"))).get,
          wp.obj(upack.Str("weight")).asInstanceOf[upack.Float64].value
        )
      }
    FiniteDistribution(pmf)
  }

  val msgToTermBase: Translator.OrElse[upack.Msg, Term] =
    msgToBuild[Term, N]("U")((n) => Universe(n)) ||
      msgToBuild[Term, II]("Ap") { case (func, arg) => fold(func)(arg) } ||
      msgToBuild[Term, III]("\u03bb") {
        case ((variable, typ), value) => variable :~> value
      } ||
      msgToBuild[Term, III]("=") {
        case ((dom, lhs), rhs) => lhs =:= rhs
      } ||
      msgToBuild[Term, III]("\u03c0") {
        case ((variable, typ), value: Typ[u]) => variable ~>: value
        case (x, y)                           => unmatched(x, y)
      } ||
      msgToBuild[Term, III]("\u03c3") {
        case ((variable, typ), value: Typ[u]) => sigma(variable)(value)
        case (x, y)                           => unmatched(x, y)
      } ||
      msgToBuild[Term, II]("\u03a0") {
        case (x: Typ[u], y: Typ[v]) => ProdTyp(x, y)
        case (x, y)                 => unmatched(x, y)
      } ||
      msgToBuild[Term, II]("\u03a3") {
        case (x: Typ[u], y: Typ[v]) => PlusTyp(x, y)
        case (x, y)                 => unmatched(x, y)
      } ||
      msgToBuild[Term, II]("pair") { case (x, y) => mkPair(x, y) } ||
      msgToBuild[Term, II](UnicodeSyms.Arrow) {
        case (x: Typ[u], y: Typ[v]) => FuncTyp(x, y)
        case (x, y)                 => unmatched(x, y)
      } ||
      msgToBuild[Term, II]("=") {
        case (dom: Typ[u], value: Term) => Refl(dom, value)
        case (x, y)                     => unmatched(x, y)
      } ||
      msgToBuild[Term, IV]("folded-term") {
        case (op, v) =>
          v.reduce[Term] {
            case (a: Term, b: Term) => applyFunc(applyFunc(op, a), b)
          }
      } ||
      msgToBuild[Term, Un]("*") { (_) =>
        Star
      } ||
      msgToBuild[Term, Un]("One") { (_) =>
        Unit
      } ||
      msgToBuild[Term, Un]("Void") { (_) =>
        Zero
      } ||
      msgToBuild[Term, Un]("Prop") { (_) =>
        Prop
      } ||
      msgToBuild[Term, Un]("Nat") { (_) =>
        NatRing.NatTyp
      } ||
      msgToBuild[Term, Un]("NatU") { (_) =>
        NatRing.NatTyp.typ
      } ||
      msgToBuild[Term, Un]("0N") { (_) =>
        NatRing.zero
      } ||
      msgToBuild[Term, Un]("succN") { (_) =>
        NatRing.succ
      } ||
      msgToBuild[Term, Un]("+N") { (_) =>
        NatRing.sum
      } ||
      msgToBuild[Term, Un]("*N") { (_) =>
        NatRing.prod
      } ||
      msgToBuild[Term, N]("NL") { (n) =>
        NatRing.Literal(n)
      } ||
      msgToBuild[Term, II]("nat-additive-morphism") {
        case (base, op) =>
          NatRing.AdditiveMorphism(
            base.asInstanceOf[Func[NatRing.Nat, NatRing.Nat]],
            op.asInstanceOf[(NatRing.Nat, NatRing.Nat) => NatRing.Nat])

      } ||
      msgToBuild[Term, II]("i1") {
        case (tp: PlusTyp[u, v], x) => tp.incl1(x.asInstanceOf[u])
        case (x, y)                 => unmatched(x, y)
      } ||
      msgToBuild[Term, II]("i2") {
        case (tp: PlusTyp[u, v], x) => tp.incl2(x.asInstanceOf[v])
        case (x, y)                 => unmatched(x, y)
      } ||
      msgToOpt[Term, IIV]("Rec") {
        case (a, (b, v)) =>
          // println(s"building base recursive type $a codomain $b data $v")
          val fn = buildRecDef()
          fn(a, (b, v))
      } ||
      msgToOpt[Term, IIV]("Ind") {
        case (a, (b, v)) =>
          val fn = buildIndDef()
          fn(a, (b, v))
      } ||
      msgToOpt[Term, IVIIV]("IRec") {
        case (u, (w, (a, (b, v)))) =>
          // println(s"building indexed recursive:\n index $w,\n type $a,\n codomain $b,\n data $v\n\n")
          val fn  = buildIndRecDef()
          val res = fn(w, (a, (b, v)))
          println(s"result: $res")
          res
      } ||
      msgToOpt[Term, IVIIV]("IInd") {
        case (u, (w, (a, (b, v)))) =>
          // println(s"building indexed inductive:\n index $w,\n type $a,\n codomain $b,\n data $v\n\n")
          val fn  = buildIndIndDef()
          val res = fn(w, (a, (b, v)))
          // println(s"result: $res")
          res
      } ||
      msgToBuild[Term, Named]("Sym") {
        case (name, tp: Typ[u]) => deHash(name) :: tp
        case (x, y)             => unmatched(x, y)
      }(travNamed, implicitly[MsgFunc[Named]]) ||
      msgToBuild[Term, II]("Witness"){
        case (tp, value) => toTyp(tp).symbObj(MereWitness(value))
      }

  val hashReg: Regex = "_[0-9][0-9]+".r

  def deHash(s: String): String = hashReg.replaceAllIn(s, "")

}

object InducPack {
  import TermPack._, ExstInducStrucs._

  def toMsg(exst: ExstInducStrucs): upack.Msg = exst match {
    case Base    => upack.Obj(upack.Str("In") -> upack.Str("base"))
    case NatRing => upack.Obj(upack.Str("In") -> upack.Str("nat-ring"))
    case OrElse(first, second) =>
      upack.Obj(
        upack.Str("In")  -> upack.Str("or-else"),
        upack.Str("first")  -> toMsg(first),
        upack.Str("second") -> toMsg(second)
      )
    case LambdaInduc(x, struc) =>
      upack.Obj(
        upack.Str("In")     -> upack.Str("\u03bb"),
        upack.Str("variable")  -> termToMsgGet(x),
        upack.Str("structure") -> toMsg(struc)
      )
    case ConsSeqExst(cs, intros) =>
      upack.Obj(
        upack.Str("In") -> upack.Str("constructor-sequence"),
        upack.Str("type")  -> termToMsgGet(cs.typ),
        upack.Str("intros") -> upack.Arr(intros.map { (t) =>
          termToMsgGet(t)
        }: _*)
      )
    case ind @ IndConsSeqExst(cs, intros) =>
      upack.Obj(
        upack.Str("In") -> upack.Str("indexed-constructor-sequence"),
        upack.Str("type")  -> termToMsgGet(ind.fmly),
        upack.Str("intros") -> upack.Arr(intros.map { (t) =>
          termToMsgGet(t)
        }: _*)
      )
  }

  def fdMsg(fd: FiniteDistribution[ExstInducDefn]): upack.Arr = {
    val pmf = for {
      Weighted(elem, p) <- fd.pmf
    } yield
      upack.Obj(
        upack.Str("type-family") -> termToMsgGet(elem.typFamily),
        upack.Str("introduction-rules") -> upack.Arr(
          (elem.intros.map((t) => termToMsgGet(t))): _*
        ),
        upack.Str("structure") -> toMsg(elem.ind),
        upack.Str("weight") -> upack.Float64(p)
      )
    upack.Arr(pmf: _*)
  }

  def fromMsg(init: ExstInducStrucs)(msg: upack.Msg): ExstInducStrucs =
    msg.obj(upack.Str("In")).str match {
      case "base"     => Base
      case "nat-ring" => NatRing
      case "or-else" =>
        OrElse(
          fromMsg(init)(msg.obj(upack.Str("first"))),
          fromMsg(init)(msg.obj(upack.Str("second")))
        )
      case "\u03bb" =>
        val x     = msgToTermExst(init)(msg.obj(upack.Str("variable"))).get
        val struc = fromMsg(init)(msg.obj(upack.Str("structure")))
        LambdaInduc(x, struc)
      case "constructor-sequence" =>
        val typ = msgToTermExst(init)(msg.obj(upack.Str("type"))).flatMap(typOpt).get
        val intros =
          msg.obj(upack.Str("intros")).arr.map((t) => msgToTermExst(init)(t).get).toVector
        get(typ, intros)
      case "indexed-constructor-sequence" =>
        val typF = msgToTermExst(init)(msg.obj(upack.Str("type"))).get
        val intros =
          msg.obj(upack.Str("intros")).arr.map((t) => msgToTermExst(init)(t).get).toVector
        getIndexed(typF, intros)
    }

  def msgToFD(exst: ExstInducStrucs)(
      msg: upack.Msg): FiniteDistribution[ExstInducDefn] = {
    val pmf =
      msg.arr.toVector.map { wp =>
        val ind       = fromMsg(exst)(wp.obj(upack.Str("structure")))
        val typFamily = msgToTermExst(exst)(wp.obj(upack.Str("type-family"))).get
        val intros = wp
          .obj(upack.Str("introduction-rules"))
          .arr
          .toVector
          .map((t) => msgToTermExst(exst)(t).get)
        val parameters = wp
          .obj(upack.Str("parameters"))
          .arr
          .toVector
          .map((t) => msgToTermExst(exst)(t).get)
        Weighted(
          ExstInducDefn(typFamily, intros, ind),
          wp.obj(upack.Str("weight")).asInstanceOf[upack.Float64].value
        )
      }
    FiniteDistribution(pmf)
  }

}

object ContextPack {
  import Context._, TermPack._
  def toMsg(ctx: Context): upack.Msg = ctx match {
    case Empty => upack.Obj(upack.Str("In") -> upack.Str("empty"))
    case ac:  AppendConstant[u] =>
      upack.Obj(
        upack.Str("In")    -> upack.Str("append-constant"),
        upack.Str("init")     -> toMsg(ac.init),
        upack.Str("constant") -> termToMsgGet(ac.constant)
      )
    case at : AppendTerm[u] =>
      val rl = at.role match {
        case Context.Assert   => upack.Str("assert")
        case Context.Consider => upack.Str("consider")
      }
      upack.Obj(
        upack.Str("In") -> upack.Str("append-term"),
        upack.Str("init")  -> toMsg(at.init),
        upack.Str("term")  -> termToMsgGet(at.term),
        upack.Str("role")  -> rl
      )
    case av: AppendVariable[u] =>
      upack.Obj(
        upack.Str("In")      -> upack.Str("append-variable"),
        upack.Str("init")       -> toMsg(av.init),
        upack.Str("expression") -> termToMsgGet(av.variable)
      )
    case AppendDefn(init, defn, global) =>
      upack.Obj(
        upack.Str("In")  -> upack.Str("append-definition"),
        upack.Str("name")   -> termToMsgGet(defn.name),
        upack.Str("init")   -> toMsg(init),
        upack.Str("value")  -> termToMsgGet(defn.valueTerm),
        upack.Str("global") -> upack.Bool(global)
      )
    case AppendIndDef(init, defn) =>
      upack.Obj(
        upack.Str("In") -> upack.Str("append-inductive-definition"),
        upack.Str("defn")  -> InducPack.toMsg(defn),
        upack.Str("init")  -> toMsg(init)
      )
  }

  def fromMsg(msg: upack.Msg): Context =
    msg.obj(upack.Str("In")).str match {
      case "empty" => Empty
      case "append-term" =>
        val init = fromMsg(msg.obj(upack.Str("init")))
        val term = msgToTermExst(init.inducStruct)(msg.obj(upack.Str("term"))).get
        val role = msg.obj(upack.Str("role")).str match {
          case "assert"   => Assert
          case "consider" => Consider
        }
        AppendTerm(init, term, role)
      case "append-constant" =>
        val init = fromMsg(msg.obj(upack.Str("init")))
        val term = msgToTermExst(init.inducStruct)(msg.obj(upack.Str("constant"))).get
        AppendConstant(init, term)
      case "append-variable" =>
        val init = fromMsg(msg.obj(upack.Str("init")))
        val term = msgToTermExst(init.inducStruct)(msg.obj(upack.Str("variable"))).get
        AppendVariable(init, term)
      case "append-definition" =>
        val init  = fromMsg(msg.obj(upack.Str("init")))
        val name  = msgToTermExst(init.inducStruct)(msg.obj(upack.Str("name"))).get
        val value = msgToTermExst(init.inducStruct)(msg.obj(upack.Str("value"))).get
        AppendDefn(init, Defn(name, value), msg.obj(upack.Str("global")).bool)
      case "append-inductive-definition" =>
        val init = fromMsg(msg.obj(upack.Str("init")))
        val defn = InducPack.fromMsg(init.inducStruct)(msg.obj(upack.Str("defn")))
        AppendIndDef(init, defn)
    }
}
