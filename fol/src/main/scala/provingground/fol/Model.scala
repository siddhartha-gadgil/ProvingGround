package provingground.fol
import Model._

/** Primary maps for an interpretation */
trait PrimaryMap[A] {
  def primConst: PartialFunction[Const, A]
  def primFunc: PartialFunction[Func, OnParams[A, A]]
  def primPred: PartialFunction[Pred, OnParams[A, Boolean]]
}
// This is fixed for a given interpretation as the stream varies

/** Logical Interpretation (data for Model, but no axioms) */
trait Model[A] extends PrimaryMap[A] {
  import Formula._

  /** The universe */
  val M: LazyList[A]

  /** Parameter space for free variables */
  type Mbar = Var => A

  /** Secondary map on terms */
  def secondaryMap(t: Term, z: Mbar): A = t match {
    case x: Var             => z(x)
    case c: Const           => primConst(c)
    case RecTerm(f, params) => primFunc(f)(params map (secondaryMap(_, z)))
  }

  def changeVar(x: Var, a: A, z: Mbar): Mbar =
    (y: Var) => if (x == y) a else z(y)

  /** Variation of z along x */
  def variation(x: Var, z: Mbar): LazyList[Mbar] = M map (changeVar(x, _, z))

  def varFormula(x: Var, z: Mbar, phi: Formula): LazyList[Boolean] =
    variation(x, z) map (secondaryMap(phi, _))

  /** Secondary map for formulas, depending of parameters for variables */
  def secondaryMap(phi: Formula, z: Mbar): Boolean = phi match {
    case AtomFormula(f, params) => primPred(f)(params map (secondaryMap(_, z)))
    case f: RecFormula          => recValue(phi, (p: Formula) => secondaryMap(p, z))
    case ExQuantFormula(x, p)   => bigOr(varFormula(x, z, p))
    case UnivQuantFormula(x, p) => bigAnd(varFormula(x, z, p))
  }

  val zhead: Mbar = (x: Var) => M.head

  /** Check formula in interpretation after Universal quantification of free variables */
  def check(f: Formula): Boolean = secondaryMap(f.sentence, zhead)
}

object Model{
    /** And over a stream, stops with first false */
  def bigAnd(bs: LazyList[Boolean]): Boolean = {
    if (bs.isEmpty) true
    else {
      if (!bs.head) false else bigAnd(bs.tail)
    }
  }

  /** Or over a stream, stops with first true */
  def bigOr(bs: LazyList[Boolean]): Boolean = {
    if (bs.isEmpty) false
    else {
      if (bs.head) true else bigAnd(bs.tail)
    }
  }


  type OnParams[A, B] = PartialFunction[List[A], B]

}