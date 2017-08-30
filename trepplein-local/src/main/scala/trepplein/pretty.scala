package trepplein

import trepplein.BinderInfo._
import trepplein.Level._
import Doc._

import scala.collection.mutable

case class PrettyOptions(showImplicits: Boolean = true,
                         hideProofs: Boolean = false,
                         hideProofTerms: Boolean = false,
                         showNotation: Boolean = true,
                         nestDepth: Int = 2)

sealed trait Notation {
  def fn: Name
  def prio: Int
  def op: String
}
case class Infix(fn: Name, prio: Int, op: String)   extends Notation
case class Prefix(fn: Name, prio: Int, op: String)  extends Notation
case class Postfix(fn: Name, prio: Int, op: String) extends Notation

class PrettyPrinter(typeChecker: Option[TypeChecker] = None,
                    notations: Map[Name, Notation] = Map(),
                    options: PrettyOptions = PrettyOptions()) {
  import options._

  val usedLCs: mutable.Set[Name] = mutable.Set[Name]()

  val MaxPrio             = 1024
  def nest(doc: Doc): Doc = doc.group.nest(nestDepth)
  case class Parenable(prio: Int, doc: Doc) {
    def parens(newPrio: Int): Doc =
      if (newPrio > prio) "(" <> doc <> ")" else doc
  }

  def showImplicits: Boolean = options.showImplicits && typeChecker.nonEmpty

  def pp(n: Name): Doc = n.toString

  def pp(level: Level): Parenable =
    level match {
      case Offset(n, Zero) => Parenable(MaxPrio, n.toString)
      case Offset(n, l) if n > 0 =>
        Parenable(0, pp(l).parens(1) <> "+" <> n.toString)
      case Max(a, b) =>
        Parenable(0, "max" <+> pp(a).parens(1) </> pp(b).parens(1))
      case IMax(a, b) =>
        Parenable(0, "imax" <+> pp(a).parens(1) </> pp(b).parens(1))
      case Param(param) => Parenable(MaxPrio, pp(param))
    }

  def mkFreshName(suggestion: Name): Name = {
    def alreadyUsed(n: Name): Boolean =
      usedLCs(n) || typeChecker.exists(_.env.declarations.contains(n))

    val sanitizedSuggestion = suggestion.toString
      .filter(c => c.isLetterOrDigit || c == '_')
      .dropWhile(c => c.isDigit || c == '_') match {
      case "" => "a"
      case s  => s
    }

    def findUnused(base: String, idx: Int): Name = {
      val n = Name(base + '_' + idx)
      if (alreadyUsed(n)) findUnused(base, idx + 1) else n
    }

    val fresh: Name =
      if (alreadyUsed(sanitizedSuggestion)) findUnused(sanitizedSuggestion, 0)
      else sanitizedSuggestion

    usedLCs += fresh
    fresh
  }

  def withFreshLC[T](suggestion: LocalConst)(f: LocalConst => T): T = {
    val fresh = mkFreshName(suggestion.of.prettyName)
    try f(suggestion.copy(of = suggestion.of.copy(prettyName = fresh)))
    finally usedLCs -= fresh
  }

  def ppBareBinder(binding: Binding): Doc =
    pp(binding.prettyName) <+> ":" </> pp(binding.ty).parens(1).group

  def isImplicit(fn: Expr): Boolean =
    typeChecker match {
      case Some(tc) =>
        try {
          tc.whnf(tc.infer(fn)) match {
            case Pi(dom, _) =>
              dom.info != BinderInfo.Default
            case _ => false
          }
        } catch { case _: Throwable => false }
      case _ => false
    }

  def pp(us: Iterable[Level]): Doc =
    ("{" <> wordwrap(us.map(pp).map(_.parens(0))) <> "}").group

  case class ParsedBinder(isPi: Boolean,
                          occursInBody: Boolean,
                          isAnon: Boolean,
                          lc: LocalConst) {
    def isImp: Boolean    = isPi && info == Default && isAnon && !occursInBody
    def isForall: Boolean = isPi && !isImp
    def isLambda: Boolean = !isPi
    def info: BinderInfo  = lc.of.info
    def ty: Expr          = lc.of.ty
    def name: Name        = lc.of.prettyName
  }
  type ParsedBinders = List[ParsedBinder]
  def parseBinders[T](e: Expr)(f: (ParsedBinders, Expr) => T): T = {
    def decompose(e: Expr, ctx: ParsedBinders): T =
      e match {
        case Pi(dom, body) =>
          val lcName = mkFreshName(dom.prettyName)
          val lc = LocalConst(
            dom.copy(prettyName = lcName,
                     ty = dom.ty.instantiate(0, ctx.view.map(_.lc).toVector)))
          decompose(body,
                    ParsedBinder(isPi = true,
                                 isAnon = dom.prettyName.isAnon,
                                 occursInBody = body.hasVar(0),
                                 lc = lc) :: ctx)
        case Lam(dom, body) =>
          val lcName = mkFreshName(dom.prettyName)
          val lc = LocalConst(
            dom.copy(prettyName = lcName,
                     ty = dom.ty.instantiate(0, ctx.view.map(_.lc).toVector)))
          decompose(body,
                    ParsedBinder(isPi = false,
                                 isAnon = dom.prettyName.isAnon,
                                 occursInBody = body.hasVar(0),
                                 lc = lc) :: ctx)
        case _ =>
          try f(ctx.reverse, e.instantiate(0, ctx.view.map(_.lc).toVector))
          finally usedLCs --= ctx.view.map(_.lc.of.prettyName)
      }
    decompose(e, Nil)
  }

  private def splitListWhile[T](xs: List[T])(
      pred: T => Boolean): (List[T], List[T]) =
    xs match {
      case x :: rest if pred(x) =>
        val (part1, part2) = splitListWhile(rest)(pred)
        (x :: part1, part2)
      case _ =>
        (Nil, xs)
    }

  def telescope(binders: List[ParsedBinder]): List[Doc] =
    binders match {
      case Nil => Nil
      case b0 :: _ =>
        val (group, rest) =
          if (b0.info == InstImplicit) (List(b0), binders.tail)
          else splitListWhile(binders)(b => b.info == b0.info && b.ty == b0.ty)
        val bare = wordwrap(group.map(b =>
            if (b.isAnon && !b.occursInBody) text("_") else pp(b.name))) <+>
            ":" </> pp(b0.ty).parens(1).group
        nest(b0.info match {
          //          case Default if group.size == 1 => bare
          case Default        => "(" <> bare <> ")"
          case Implicit       => "{" <> bare <> "}"
          case StrictImplicit => "{{" <> bare <> "}}"
          case InstImplicit   => "[" <> bare <> "]"
        }) :: telescope(rest)
    }

  def pp(binders: ParsedBinders, inner: Parenable): Parenable =
    binders match {
      case Nil => inner
      case b :: rest if b.isImp =>
        Parenable(
          24,
          (nest(pp(b.ty).parens(25)) <+> "→" <> line).group <> pp(rest, inner)
            .parens(24))
      case b :: _ if b.isForall =>
        val (group, rest) = splitListWhile(binders)(_.isForall)
        Parenable(
          0,
          nest("∀" <+> wordwrap(telescope(group)) <> ",") </> pp(rest, inner)
            .parens(0))
      case b :: _ if b.isLambda =>
        val (group, rest) = splitListWhile(binders)(_.isLambda)
        Parenable(
          0,
          nest("λ" <+> wordwrap(telescope(group)) <> ",") </> pp(rest, inner)
            .parens(0))
    }

  private def constName(n: Name): Parenable =
    Parenable(MaxPrio, if (!showImplicits) pp(n) else "@" <> pp(n))

  def pp(e: Expr): Parenable =
    e match {
      case _ if hideProofTerms && typeChecker.exists(_.isProof(e)) =>
        Parenable(MaxPrio, "_")
      case Var(idx) => Parenable(MaxPrio, s"#$idx")
      case Sort(level) if level.isZero && options.showNotation =>
        Parenable(MaxPrio, "Prop")
      case Sort(Succ(level)) =>
        Parenable(MaxPrio, "Type" <+> pp(level).parens(MaxPrio))
      case Sort(level) =>
        Parenable(MaxPrio, "Sort" <+> pp(level).parens(MaxPrio))
      case Const(name, _) if typeChecker.exists(_.env.get(name).nonEmpty) =>
        constName(name)
      case Const(name, levels) =>
        val univParams: Doc = if (levels.isEmpty) "" else "." <> pp(levels)
        Parenable(MaxPrio, "@" <> pp(name) <> univParams)
      case LocalConst(of, _, _) => constName(of.prettyName)
      case Lam(_, _) | Pi(_, _) =>
        parseBinders(e) { (binders, inner) =>
          pp(binders, pp(inner))
        }
      case Let(domain, value, body) =>
        withFreshLC(LocalConst(domain)) { lc =>
          Parenable(0,
                    (nest(
                      "let" <+> ppBareBinder(lc.of).group <+> ":=" </> pp(
                        value).parens(0).group <+> "in") </>
                      pp(body.instantiate(lc)).parens(0)).group)
        }
      case App(_, _) =>
        def go(e: Expr, as: List[Expr]): (Expr, List[Expr]) =
          e match {
            case App(hd, _) if !showImplicits && isImplicit(hd) => go(hd, as)
            case App(hd, a)                                     => go(hd, a :: as)
            case hd                                             => (hd, as)
          }
        def printDefault(fn: Expr, as: List[Expr]) =
          Parenable(MaxPrio - 1,
                    nest(
                      wordwrap(pp(fn).parens(MaxPrio - 1).group :: as.map(
                        pp(_).parens(MaxPrio).group))))
        go(e, Nil) match {
          case (fn, Nil) => pp(fn)
          case (fn @ Const(n, _), as) if showNotation =>
            notations.get(n) match {
              case Some(Prefix(_, prio, op)) if as.size == 1 =>
                Parenable(
                  prio - 1,
                  (op <> zeroWidthLine).group <> pp(as(0)).parens(prio))
              case Some(Postfix(_, prio, op)) if as.size == 1 =>
                Parenable(
                  prio - 1,
                  (pp(as(0)).parens(prio) <> zeroWidthLine <> op).group)
              case Some(Infix(_, prio, op)) if as.size == 2 =>
                Parenable(
                  prio - 1,
                  nest(
                    pp(as(0)).parens(prio) <> op <> zeroWidthLine <> pp(as(1))
                      .parens(prio)))
              case _ =>
                printDefault(fn, as)
            }
          case (fn, as) => printDefault(fn, as)
        }
    }

  def parseParams[T](ty: Expr, value: Expr)(
      f: (List[ParsedBinder], List[ParsedBinder], Expr, Expr) => T): T =
    parseBinders(ty) { (binders, ty) =>
      def go(binders: List[ParsedBinder],
             value: Expr,
             reverseParams: List[ParsedBinder]): T =
        (binders, value) match {
          case (b :: bs, Lam(_, value_)) if b.isForall =>
            go(bs, value_, b :: reverseParams)
          case _ =>
            f(reverseParams.reverse,
              binders,
              ty,
              value.instantiate(0, reverseParams.view.map(_.lc).toVector))
        }
      go(binders, value, Nil)
    }

  def pp(decl: Declaration): Doc =
    decl match {
      case Definition(name, univParams, ty, value, _) =>
        val ups: Doc = if (univParams.isEmpty) "" else " " <> pp(univParams)
        val isProp   = typeChecker.exists(_.isProposition(ty))
        parseParams(ty, value) { (params, tyBinders, ty, value) =>
          val cmd = if (isProp) "lemma" else "def"
          val ppVal: Doc =
            if (isProp && hideProofs) "_" else pp(value).parens(0).group
          cmd <> ups <+> nest(
            nest(wordwrap(pp(name) :: telescope(params))) <+>
              ":" </> pp(tyBinders, pp(ty))
              .parens(0)
              .group <+> ":=") </> ppVal <> line
        }
      case Axiom(name, univParams, ty, builtin) =>
        val ups: Doc = if (univParams.isEmpty) "" else " " <> pp(univParams)
        val doc = parseBinders(ty) { (binders, ty) =>
          val (params, rest) = splitListWhile(binders)(_.isForall)
          "axiom" <> ups <+> nest(
            nest(wordwrap(pp(name) :: telescope(params))) <+>
              ":" </> pp(rest, pp(ty)).parens(0).group) <> line
        }
        if (builtin) "/- builtin -/" <+> doc else doc
    }
}

object pretty {
  def apply(e: Expr): String =
    new PrettyPrinter().pp(e).doc.group.render(lineWidth = 80)
}
