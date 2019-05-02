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
        upack.Obj(upack.Str("Fst") -> xMsg.encode(t._1), upack.Str("Snd") -> yMsg.encode(t._2))

      def decode(msg: upack.Msg): (X[upack.Msg], Y[upack.Msg]) =
        (xMsg.decode(msg.obj(upack.Str("Fst"))), yMsg.decode(msg.obj(upack.Str("Snd"))))
    }

  import Translator._

  def toMsg[I, F[_]](pat: Pattern[I, F])(name: String, header: upack.Msg = upack.Str("intro"))(
      implicit msgF: MsgFunc[F]): Translator[I, upack.Msg] =
    pat >>> { (msg) =>
      upack.Obj(header -> upack.Str(name), upack.Str("Tree") -> msgF.encode(msg))
    }

  def msgToOpt[I, F[_]: Traverse](name: String, header: upack.Msg = upack.Str("intro"))(
      build: F[I] => Option[I])(
      implicit msgF: MsgFunc[F]): Translator[upack.Msg, I] = {
    val pat = Pattern[upack.Msg, F] { (msg) =>
      if (msg.obj(header) == upack.Str(name)) Some(msgF.decode(msg.obj(upack.Str("Tree")))) else None
    }
    pat >> build
  }

  def msgToBuild[I, F[_]: Traverse](name: String, header: upack.Msg = upack.Str("intro"))(
      build: F[I] => I)(implicit msgF: MsgFunc[F]): Translator[upack.Msg, I] = {
    val pat = Pattern[upack.Msg, F] { (msg) =>
      if (msg.obj(header) == upack.Str(name)) Some(msgF.decode(msg.obj(upack.Str("Tree")))) else None
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

  val termToPack: Translator.OrElse[Term, upack.Msg] =
    toMsg(universe)("universe") ||
      toMsg(formalAppln)("appln") ||
      toMsg(lambdaTriple)("lambda") ||
      toMsg(sigmaTriple)("sigma") ||
      toMsg(piTriple)("pi") ||
      toMsg(prodTyp)("product-type") ||
      toMsg(absPair)("pair") ||
      toMsg(plusTyp)("plus-type") ||
      toMsg(funcTyp)("func-type") ||
      toMsg(star)("star") ||
      toMsg(unit)("unit-type") ||
      toMsg(zero)("zero-type") ||
      toMsg(prop)("prop-universe") ||
      toMsg(indInducFunc)("indexed-inductive-function") ||
      toMsg(indRecFunc)("indexed-recursive-function") ||
      toMsg(recFunc)("recursive-function") ||
      toMsg(inducFunc)("inductive-function") ||
      toMsg(hashSymbolic)("symbolic") ||
      toMsg(firstIncl)("first-inclusion") ||
      toMsg(secondIncl)("second-inclusion") ||
      toMsg(identityTyp)("equality") ||
      toMsg(refl)("reflexivity") ||
      toMsg(natTyp)("nat-type") ||
      toMsg(natUniv)("nat-univ") ||
      toMsg(natZero)("nat-zero") ||
      toMsg(natSucc)("nat-succ") ||
      toMsg(natSum)("nat-sum") ||
      toMsg(natProd)("nat-prod") ||
      toMsg(natLiteral)("nat-literal") ||
      toMsg(natAddMorph)("nat-additive-morphism") ||
      toMsg(foldedTerm)("folded-term") ||
      toMsg(miscAppln)("appln")

  def termToPackGet(t: Term) =
    termToPack(t).getOrElse(throw new Exception(s"cannot serialize term $t"))

  def fdPack(fd: FiniteDistribution[Term]): upack.Arr = {
    val pmf = for {
      Weighted(elem, p) <- fd.pmf
      tmsg               <- termToPack(elem)
    } yield upack.Obj(upack.Str("term") -> tmsg, upack.Str("weight") -> upack.Float64(p))
    upack.Arr(pmf: _*)
  }

  import induction._

  def msgonToTerm(
      inds: Typ[Term] => Option[ConstructorSeqTL[_, Term, _]] = (_) => None,
      indexedInds: Term => Option[IndexedConstructorSeqDom[_, Term, _, _, _]] =
        (_) => None): Translator.OrElse[upack.Msg, Term] =
    msgonToTermBase ||
      msgToOpt[Term, IIV]("recursive-function") {
        case (x, (y, v)) =>
          buildRecDef(inds)(x, (y, v))
      } ||
      msgToOpt[Term, IIV]("inductive-function") {
        case (x, (y, v)) => buildIndDef(inds)(x, (y, v))
      } ||
      msgToOpt[Term, IVIIV]("indexed-recursive-function") {
        case (u, (w, (x, (y, v)))) =>
          buildIndRecDef(indexedInds)(w, (x, (y, v)))
      } ||
      msgToOpt[Term, IVIIV]("indexed-inductive-function") {
        case (u, (w, (x, (y, v)))) =>
          buildIndIndDef(indexedInds)(w, (x, (y, v)))
      }

  def msgToTermExst(exst: ExstInducStrucs): Translator.OrElse[upack.Msg, Term] =
    msgonToTermBase ||
      msgToOpt[Term, IIV]("recursive-function") {
        case (dom: Term, (cod: Term, data: Vector[Term])) =>
          for {
            codom <- typOpt(cod)
            fn    <- exst.recOpt(dom, codom)
          } yield (fn /: data)(fold(_)(_))
        //buildRecDef(inds)(x, (y, v))
      } ||
      msgToOpt[Term, IIV]("inductive-function") {
        case (dom: Term, (cod: Term, data: Vector[Term])) =>
          for {
            fn <- exst.inducOpt(dom, cod)
          } yield (fn /: data)(fold(_)(_))
      } ||
      msgToOpt[Term, IVIIV]("indexed-recursive-function") {
        case (_,
              (index: Vector[Term],
               (dom: Term, (cod: Term, data: Vector[Term])))) =>
          for {
            codom <- typOpt(cod)
            fn    <- exst.recOpt(dom, codom)
          } yield
            (fn /: (data ++ index))(fold(_)(_)) //buildIndRecDef(indexedInds)(w, (x, (y, v)))
      } ||
      msgToOpt[Term, IVIIV]("indexed-inductive-function") {
        case (_,
              (index: Vector[Term],
               (dom: Term, (cod: Term, data: Vector[Term])))) =>
          for {
            fn <- exst.inducOpt(dom, cod)
          } yield (fn /: (data ++ index))(fold(_)(_))
        //buildIndIndDef(indexedInds)(w, (x, (y, v)))
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

  val msgonToTermBase: Translator.OrElse[upack.Msg, Term] =
    msgToBuild[Term, N]("universe")((n) => Universe(n)) ||
      msgToBuild[Term, II]("appln") { case (func, arg) => fold(func)(arg) } ||
      msgToBuild[Term, III]("lambda") {
        case ((variable, typ), value) => variable :~> value
      } ||
      msgToBuild[Term, III]("equality") {
        case ((dom, lhs), rhs) => lhs =:= rhs
      } ||
      msgToBuild[Term, III]("pi") {
        case ((variable, typ), value: Typ[u]) => variable ~>: value
        case (x, y)                           => unmatched(x, y)
      } ||
      msgToBuild[Term, III]("sigma") {
        case ((variable, typ), value: Typ[u]) => sigma(variable)(value)
        case (x, y)                           => unmatched(x, y)
      } ||
      msgToBuild[Term, II]("product-type") {
        case (x: Typ[u], y: Typ[v]) => ProdTyp(x, y)
        case (x, y)                 => unmatched(x, y)
      } ||
      msgToBuild[Term, II]("plus-type") {
        case (x: Typ[u], y: Typ[v]) => PlusTyp(x, y)
        case (x, y)                 => unmatched(x, y)
      } ||
      msgToBuild[Term, II]("pair") { case (x, y) => mkPair(x, y) } ||
      msgToBuild[Term, II]("func-type") {
        case (x: Typ[u], y: Typ[v]) => FuncTyp(x, y)
        case (x, y)                 => unmatched(x, y)
      } ||
      msgToBuild[Term, II]("reflexivity") {
        case (dom: Typ[u], value: Term) => Refl(dom, value)
        case (x, y)                     => unmatched(x, y)
      } ||
      msgToBuild[Term, IV]("folded-term") {
        case (op, v) =>
          v.reduce[Term] {
            case (a: Term, b: Term) => applyFunc(applyFunc(op, a), b)
          }
      } ||
      msgToBuild[Term, Un]("star") { (_) =>
        Star
      } ||
      msgToBuild[Term, Un]("unit-type") { (_) =>
        Unit
      } ||
      msgToBuild[Term, Un]("zero-type") { (_) =>
        Zero
      } ||
      msgToBuild[Term, Un]("prop-universe") { (_) =>
        Prop
      } ||
      msgToBuild[Term, Un]("nat-type") { (_) =>
        NatRing.NatTyp
      } ||
      msgToBuild[Term, Un]("nat-univ") { (_) =>
        NatRing.NatTyp.typ
      } ||
      msgToBuild[Term, Un]("nat-zero") { (_) =>
        NatRing.zero
      } ||
      msgToBuild[Term, Un]("nat-succ") { (_) =>
        NatRing.succ
      } ||
      msgToBuild[Term, Un]("nat-sum") { (_) =>
        NatRing.sum
      } ||
      msgToBuild[Term, Un]("nat-prod") { (_) =>
        NatRing.prod
      } ||
      msgToBuild[Term, N]("nat-literal") { (n) =>
        NatRing.Literal(n)
      } ||
      msgToBuild[Term, II]("nat-additive-morphism") {
        case (base, op) =>
          NatRing.AdditiveMorphism(
            base.asInstanceOf[Func[NatRing.Nat, NatRing.Nat]],
            op.asInstanceOf[(NatRing.Nat, NatRing.Nat) => NatRing.Nat])

      } ||
      msgToBuild[Term, II]("first-inclusion") {
        case (tp: PlusTyp[u, v], x) => tp.incl1(x.asInstanceOf[u])
        case (x, y)                 => unmatched(x, y)
      } ||
      msgToBuild[Term, II]("second-inclusion") {
        case (tp: PlusTyp[u, v], x) => tp.incl2(x.asInstanceOf[v])
        case (x, y)                 => unmatched(x, y)
      } ||
      msgToOpt[Term, IIV]("recursive-function") {
        case (a, (b, v)) =>
          // println(s"building base recursive type $a codomain $b data $v")
          val fn = buildRecDef()
          fn(a, (b, v))
      } ||
      msgToOpt[Term, IIV]("inductive-function") {
        case (a, (b, v)) =>
          val fn = buildIndDef()
          fn(a, (b, v))
      } ||
      msgToOpt[Term, IVIIV]("indexed-recursive-function") {
        case (u, (w, (a, (b, v)))) =>
          // println(s"building indexed recursive:\n index $w,\n type $a,\n codomain $b,\n data $v\n\n")
          val fn  = buildIndRecDef()
          val res = fn(w, (a, (b, v)))
          println(s"result: $res")
          res
      } ||
      msgToOpt[Term, IVIIV]("indexed-inductive-function") {
        case (u, (w, (a, (b, v)))) =>
          // println(s"building indexed inductive:\n index $w,\n type $a,\n codomain $b,\n data $v\n\n")
          val fn  = buildIndIndDef()
          val res = fn(w, (a, (b, v)))
          // println(s"result: $res")
          res
      } ||
      msgToBuild[Term, Named]("symbolic") {
        case (name, tp: Typ[u]) => deHash(name) :: tp
        case (x, y)             => unmatched(x, y)
      }(travNamed, implicitly[MsgFunc[Named]])

  val hashReg: Regex = "_[0-9][0-9]+".r

  def deHash(s: String): String = hashReg.replaceAllIn(s, "")

}

object InducPack {
  import TermPack._, ExstInducStrucs._

  def toPack(exst: ExstInducStrucs): upack.Msg = exst match {
    case Base    => upack.Obj(upack.Str("intro") -> upack.Str("base"))
    case NatRing => upack.Obj(upack.Str("intro") -> upack.Str("nat-ring"))
    case OrElse(first, second) =>
      upack.Obj(
        upack.Str("intro")  -> upack.Str("or-else"),
        upack.Str("first")  -> toPack(first),
        upack.Str("second") -> toPack(second)
      )
    case LambdaInduc(x, struc) =>
      upack.Obj(
        upack.Str("intro")     -> upack.Str("lambda"),
        upack.Str("variable")  -> termToPackGet(x),
        upack.Str("structure") -> toPack(struc)
      )
    case ConsSeqExst(cs, intros) =>
      upack.Obj(
        upack.Str("intro") -> upack.Str("constructor-sequence"),
        upack.Str("type")  -> termToPackGet(cs.typ),
        upack.Str("intros") -> upack.Arr(intros.map { (t) =>
          termToPackGet(t)
        }: _*)
      )
    case ind @ IndConsSeqExst(cs, intros) =>
      upack.Obj(
        upack.Str("intro") -> upack.Str("indexed-constructor-sequence"),
        upack.Str("type")  -> termToPackGet(ind.fmly),
        upack.Str("intros") -> upack.Arr(intros.map { (t) =>
          termToPackGet(t)
        }: _*)
      )
  }

  def fdPack(fd: FiniteDistribution[ExstInducDefn]): upack.Arr = {
    val pmf = for {
      Weighted(elem, p) <- fd.pmf
    } yield
      upack.Obj(
        upack.Str("type-family") -> termToPackGet(elem.typFamily),
        upack.Str("introduction-rules") -> upack.Arr(
          (elem.intros.map((t) => termToPackGet(t))): _*
        ),
        upack.Str("structure") -> toPack(elem.ind),
        upack.Str("weight") -> upack.Float64(p)
      )
    upack.Arr(pmf: _*)
  }

  def fromPack(init: ExstInducStrucs)(msg: upack.Msg): ExstInducStrucs =
    msg.obj(upack.Str("intro")).str match {
      case "base"     => Base
      case "nat-ring" => NatRing
      case "or-else" =>
        OrElse(
          fromPack(init)(msg.obj(upack.Str("first"))),
          fromPack(init)(msg.obj(upack.Str("second")))
        )
      case "lambda" =>
        val x     = msgToTermExst(init)(msg.obj(upack.Str("variable"))).get
        val struc = fromPack(init)(msg.obj(upack.Str("structure")))
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
        val ind       = fromPack(exst)(wp.obj(upack.Str("structure")))
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
  def toPack(ctx: Context): upack.Msg = ctx match {
    case Empty => upack.Obj(upack.Str("intro") -> upack.Str("empty"))
    case AppendConstant(init, constant: Term) =>
      upack.Obj(
        upack.Str("intro")    -> upack.Str("append-constant"),
        upack.Str("init")     -> toPack(init),
        upack.Str("constant") -> termToPackGet(constant)
      )
    case AppendTerm(init, expr: Term, role: Role) =>
      val rl = role match {
        case Context.Assert   => upack.Str("assert")
        case Context.Consider => upack.Str("consider")
      }
      upack.Obj(
        upack.Str("intro") -> upack.Str("append-term"),
        upack.Str("init")  -> toPack(init),
        upack.Str("term")  -> termToPackGet(expr),
        upack.Str("role")  -> rl
      )
    case AppendVariable(init, expr: Term) =>
      upack.Obj(
        upack.Str("intro")      -> upack.Str("append-variable"),
        upack.Str("init")       -> toPack(init),
        upack.Str("expression") -> termToPackGet(expr)
      )
    case AppendDefn(init, defn, global) =>
      upack.Obj(
        upack.Str("intro")  -> upack.Str("append-definition"),
        upack.Str("name")   -> termToPackGet(defn.name),
        upack.Str("init")   -> toPack(init),
        upack.Str("value")  -> termToPackGet(defn.valueTerm),
        upack.Str("global") -> upack.Bool(global)
      )
    case AppendIndDef(init, defn) =>
      upack.Obj(
        upack.Str("intro") -> upack.Str("append-inductive-definition"),
        upack.Str("defn")  -> InducPack.toPack(defn),
        upack.Str("init")  -> toPack(init)
      )
  }

  def fromPack(msg: upack.Msg): Context =
    msg.obj(upack.Str("intro")).str match {
      case "empty" => Empty
      case "append-term" =>
        val init = fromPack(msg.obj(upack.Str("init")))
        val term = msgToTermExst(init.inducStruct)(msg.obj(upack.Str("term"))).get
        val role = msg.obj(upack.Str("role")).str match {
          case "assert"   => Assert
          case "consider" => Consider
        }
        AppendTerm(init, term, role)
      case "append-constant" =>
        val init = fromPack(msg.obj(upack.Str("init")))
        val term = msgToTermExst(init.inducStruct)(msg.obj(upack.Str("constant"))).get
        AppendConstant(init, term)
      case "append-variable" =>
        val init = fromPack(msg.obj(upack.Str("init")))
        val term = msgToTermExst(init.inducStruct)(msg.obj(upack.Str("variable"))).get
        AppendVariable(init, term)
      case "append-definition" =>
        val init  = fromPack(msg.obj(upack.Str("init")))
        val name  = msgToTermExst(init.inducStruct)(msg.obj(upack.Str("name"))).get
        val value = msgToTermExst(init.inducStruct)(msg.obj(upack.Str("value"))).get
        AppendDefn(init, Defn(name, value), msg.obj(upack.Str("global")).bool)
      case "append-inductive-definition" =>
        val init = fromPack(msg.obj(upack.Str("init")))
        val defn = InducPack.fromPack(init.inducStruct)(msg.obj(upack.Str("defn")))
        AppendIndDef(init, defn)
    }
}
