package provingground.fol


/** Logical terms */
trait Term extends Expression {

  /** Formula giving equality */
  def eqls(that: Term) = Eq(this, that)

  /** TermListitutes variables by terms*/
  def subs(xt: Var => Term): Term

  /** Single variable substituted by a term */
  def subs(x: Var, t: Term): Term = {
    val xt: (Var => Term) = (y: Var) => if (y == x) t else y
    subs(xt)
  }

  /** Formal + operation */
  def +(that: Term) = BinOp("+")(this, that)

  /** Formal -(binary) operation */
  def -(that: Term) = BinOp("-")(this, that)

  /** Formal * operation */
  def *(that: Term) = BinOp("*")(this, that)

  /** Formal / operation */
  def /(that: Term) = BinOp("/")(this, that)

  /** Formal ** operation */
  def **(that: Term) = BinOp("**")(this, that)

  /** Formal | operation */
  def |(that: Term) = BinOp("|")(this, that)

  /** Formal unary - operation */
  def unary_- = UnOp("-")(this)

  /** Formal < relation */
  def <(that: Term): Formula = BinRel("<")(this, that)

  /** Formal > relation */
  def >(that: Term): Formula = BinRel(">")(this, that)

  /** Formal < relation */
  def <<(that: Term): Formula = BinRel("<")(this, that)

  /** Formal > relation */
  def >>(that: Term): Formula = BinRel(">")(this, that)

  /** Formal = relation */
  def =:=(that: Term): Formula = BinRel("=")(this, that)

  /** Formal <= relation */
  def <=(that: Term): Formula = BinRel("<=")(this, that)

  /** Formal >= relation */
  def >=(that: Term): Formula = BinRel(">=")(this, that)

  /** Formal ~ relation */
  def ~(that: Term): Formula = BinRel("~")(this, that)

  def apply(b: BinRel) = (that: Term) => b(this, that)

  def is(u: UnRel) = u(this)
}

/** Logical Variable */
class Var extends Term {
  val freeVars                    = Set(this)
  def subs(xt: Var => Term): Term = xt(this)
}

object Var {

  /** Logical Variable determined by Name */
  /** stream of Variables starting with Var("a") */
  val varstream: LazyList[Var] =
    (LazyList.from(0)) map ((x: Int) => VarSym((x + 'a').toChar.toString))

}

case class VarSym(name: String) extends Var {
  override def toString = name
}

/** Logical constants */
trait Const extends Term with LanguageParam {
  override val freeVars: Set[Var]    = Set()
  override def subs(xt: Var => Term) = this
}

/** Constants given by name */
case class ConstSym(name: String) extends Const

/** Integer constant */
case class IntConst(value: Long) extends Const {
  override def toString = value.toString
}

/** Unparsed term formally wrapped */
case class TermFmla(name: String) extends Term {
  override def toString           = name
  val freeVars: Set[Var]          = Set()
  def subs(xt: Var => Term): Term = this
}

/** Recursive term */
case class RecTerm(f: Func, params: List[Term]) extends Term {
  def this(f: Func, t: Term) = this(f, List(t))

  override def toString = f match {
    case FuncSym(name, _) => name+params.mkString("(", ", ", ")")
    case BinOp(name)      => params.head.toString + name + params.last.toString
    case _                => super.toString
  }
  val freeVars: Set[Var]          = (params map (_.freeVars)) reduce (_ union _)
  def subs(xt: Var => Term): Term = RecTerm(f, params map (_.subs(xt)))
}
