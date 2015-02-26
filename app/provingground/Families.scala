package provingground
import HoTT._
import ScalaUniverses._
import math._

/**
 * @author gadgil
 */
object Families {
  
  /**
   * a single trait to hold all type patterns, independent of U.
   */
  trait FmlyPtnLike{
    /**
     * the universe containing the type
     */
    val univLevel : Int

    /**
     * scala type (upper bound)
     */
    type PtnType <:  Term

    /**
     * returns the type corresponding to the pattern, such as A -> W, given the (inductive) type W
     */
    def apply(tp : Typ[Term]) : Typ[PtnType]

    /**
    * function induced by f: W -> X of type (A -> W) -> (A -> X) etc
    *
    * @param f function from which to induce
    *
    * @param W the inductive type
    *
    * @param X codomain of the given function
    */
    def induced(W : Typ[Term], X : Typ[Term])(f : Term => Term) : PtnType => PtnType

    /**
    * dependent function induced by dependent f: W -> X(s) of type (A -> W) -> ((a : A) ~> Xs(a)) etc
    *
    * @param f dependent function from which to induce
    *
    * @param W the inductive type
    *
    * @param Xs family of codomains of the given dependent function
    */
    def inducedDep(W : Typ[Term], Xs : Term => Typ[Term])(f : Term => Term) : PtnType => PtnType
  }
  
      /**
   * A pattern for families, e.g. of inductive types to be defined
   * for instance A -> B -> W, where W is the type to be defined;
   * ends with the type with members.
   * the pattern is a function of the type W.
   *
   * @param U (upper bound on) scala type of an object with the pattern - especially functions.
   * this is needed to ensure that families have a common scala type that can be used inductively.
   */
  sealed trait FmlyPtn[U <: Term] extends FmlyPtnLike{
    /**
     * scala type (upper bound)
     */
       type PtnType = U

       /**
        * function induced by f: W -> X of type (A -> W) -> (A -> X) etc
        */
        def induced(W : Typ[Term], X : Typ[Term])(f : Term => Term) : PtnType => PtnType

        /**
        * dependent function induced by dependent f: W -> X(s) of type (A -> W) -> (A ~> X(s)) etc
        */
        def inducedDep(W : Typ[Term], Xs : Term => Typ[Term])(f : Term => Term) : PtnType => PtnType
  }


    /**
   * The identity family
   */
  case object IdFmlyPtn extends FmlyPtn[Term]{
    def apply(W : Typ[Term]) = W

    val univLevel = 0

    /**
     * induced function is the given one.
     */
    def induced(W : Typ[Term], X : Typ[Term])(f : Term => Term) = f

    /**
     * induced function is the given one.
     */
    def inducedDep(W : Typ[Term], Xs : Term => Typ[Term])(f : Term => Term) = f
  }
  
  trait RecFmlyPtn[V <: Term with Subs[V]] extends FmlyPtn[FuncLike[Term, V]]{
    val tail : Typ[Term]
    
    val headfibre: Term => FmlyPtn[V]
  }
  
  case class FuncFmlyPtn[V <: Term with Subs[V]](tail : Typ[Term], head : FmlyPtn[V])/*(
        implicit su: ScalaUniv[V])*/ extends RecFmlyPtn[V]{
    def apply(W: Typ[Term]) = FuncTyp[Term, head.PtnType](tail, head(W))

    val headfibre = (arg: Term) => head
    
    val univLevel = max(head.univLevel, univlevel(tail.typ))

    /**
     * inductively defining the induced function.
     * maps (g : tail --> head(W)) to func : tail --> head(X) given (head(W) --> head(X))
     *
     */
    def induced(W : Typ[Term], X: Typ[Term])(f : Term => Term) : PtnType => PtnType = {
      (g : PtnType) =>
        val func =((t : Term) => head.induced(W, X)(f) (g(t)))
        val codomain = head(X)
        FuncDefn[Term, head.PtnType](func, tail, codomain)
    }

    /**
     * inductively defining the induced function.
     * maps (g : tail --> head(W)) to func : (t : tail) ~> head(Xs(t)) given (head(W) --> (t: tail) ~> head(Xs(t)))
     *
     */
    def inducedDep(W : Typ[Term], Xs: Term => Typ[Term])(f : Term => Term) : PtnType => PtnType = {
      (g : PtnType) =>
        val func =((t : Term) => head.inducedDep(W, Xs)(f) (g(t)))
        val section = (t : Term) => head(Xs(t))
        val x = "x" :: tail
        val fiber = lmbda(x)(section(x))
     //   val fiber = typFamily[Term, head.PtnType](tail, section)
        DepFuncDefn[Term, head.PtnType](func, tail, fiber)
    }
  }

    /**
   * Extending by a constant type A a family of type patterns depending on (a : A).
   *
   */
  case class DepFuncFmlyPtn[V <: Term with Subs[V]](tail: Typ[Term],
      headfibre : Term => FmlyPtn[V], headlevel: Int = 0)
      /*(implicit su: ScalaUniv[V])*/ extends RecFmlyPtn[V]{
    def apply(W : Typ[Term]) = {
      val x = "x" :: tail
      val fiber = lmbda(x)(headfibre(x)(W))
   //   val fiber = typFamily(tail,  (t : Term) => headfibre(t)(W))
      PiTyp[Term, V](fiber)
    }

    val head = headfibre(tail.symbObj(""))

//    type PtnType = FuncLike[Term, head.PtnType]

     def induced(W : Typ[Term], X: Typ[Term])(f : Term => Term) : PtnType => PtnType = {
      (g : PtnType) =>
        val func =((t : Term) => headfibre(t).induced(W, X)(f) (g(t)))
        val x = "x" :: tail
        val fiber = lmbda(x)(headfibre(x)(X))
    //    val fiber = typFamily[Term, V](tail,  (t : Term) => headfibre(t)(X))
        DepFuncDefn[Term, V](func, tail, fiber)
    }

    def inducedDep(W : Typ[Term], Xs: Term => Typ[Term])(f : Term => Term) : PtnType => PtnType = {
      (g : PtnType) =>
        val func =((t : Term) => headfibre(t).induced(W, Xs(t))(f) (g(t)))
        val x = "x" :: tail
        val fiber = lmbda(x)(headfibre(x)(Xs(x)))
      //  val fiber = typFamily[Term, V](tail, (t : Term) => headfibre(t)(Xs(t)))
        DepFuncDefn[Term, V](func, tail, fiber)
    }

    val univLevel = max(univlevel(tail.typ), headlevel)
  }
  
  trait FmlyMember[U <: Term]{
    val value: Term
    
    val W : Typ[Term]
    
    val fmlyPtn : FmlyPtn[U]
  }
  
  case class SelfMember(value: Term, W: Typ[Term]) extends FmlyMember[Term]{
    val fmlyPtn = IdFmlyPtn
  }
  
  case class FuncFmlyMember[U <: Term with Subs[U]](tail: Typ[Term],
      arg: Term, headfibre: Term => FmlyMember[U]) extends FmlyMember[FuncLike[Term, U]]{
    val value = headfibre(arg).value
    
    val W = headfibre(arg).W
    
    val fmlyPtn = FuncFmlyPtn(tail, headfibre(arg).fmlyPtn)
  }
  
  case class DepFuncFmlyMember[U <: Term with Subs[U]](tail: Typ[Term],
      arg: Term, headfibre: Term => FmlyMember[U]) extends FmlyMember[FuncLike[Term, U]]{
    val value = headfibre(arg).value
    
    val W = headfibre(arg).W
    
    val fmlyPtn = DepFuncFmlyPtn(tail, (x: Term) => headfibre(x).fmlyPtn)
  }
  
/*  case class RecMember(
      arg: Term, : RecFmlyPtn[V], W: Typ[Term]) extends FmlyMember{
  //  val value = 
  }*/
}