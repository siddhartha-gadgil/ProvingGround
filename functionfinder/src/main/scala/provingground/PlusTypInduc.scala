package provingground
import provingground.HoTT._
import ScalaRep._
import scala.reflect.runtime.universe.{
  Try => UnivTry,
  Function => FunctionUniv,
  _
}

object PlusTypInduc {
  import PlusTyp.{FirstIncl, ScndIncl}

  case class PlusExtendedFunction[V <: Term with Subs[V]](
      first: Typ[Term],
      second: Typ[Term],
      codom: Typ[V],
      firstfn: Func[Term, V],
      scndfn: Func[Term, V])
      extends Func[Term, V]
      with Subs[PlusExtendedFunction[V]] {

    val dom = pair(first, second)

    val typ = (dom: Typ[Term]) ->: codom

    def newobj = throw new IllegalArgumentException(s"trying to use the constant $this as a variable (or a component of one)")

    def act(u: Term) = u match {
      case FirstIncl(`first`, a: Term) => firstfn(a)
      case ScndIncl(`second`, b: Term) => scndfn(b)
      case _                           => codom.symbObj(ApplnSym(this, u))
    }

//	  // val domobjtpe: reflect.runtime.universe.Type = typeOf[Term]

//	  // val codomobjtpe: reflect.runtime.universe.Type = typeOf[V]

    def subs(x: provingground.HoTT.Term, y: provingground.HoTT.Term) =
      PlusExtendedFunction(first.subs(x, y),
                           second.subs(x, y),
                           codom.subs(x, y),
                           firstfn.subs(x, y),
                           scndfn.subs(x, y))
  }

  case class PlusExtendedDepFunction[V <: Term with Subs[V]](
      first: Typ[Term],
      second: Typ[Term],
      depcodom: Func[Term, Typ[V]],
      firstfn: FuncLike[Term, V],
      scndfn: FuncLike[Term, V])
      extends FuncLike[Term, V]
      with Subs[PlusExtendedDepFunction[V]] {

    val dom = pair(first, second)

    val typ = PiDefn(depcodom)

    def newobj = throw new IllegalArgumentException(s"trying to use the constant $this as a variable (or a component of one)")

    def act(u: Term) = u match {
      case FirstIncl(`first`, a: Term) => firstfn(a)
      case ScndIncl(`second`, b: Term) => scndfn(b)
      case _                           => depcodom(u).symbObj(ApplnSym(this, u))
    }

//	  // val domobjtpe: reflect.runtime.universe.Type = typeOf[Term]

//	  // val codomobjtpe: reflect.runtime.universe.Type = typeOf[V]

    def subs(x: provingground.HoTT.Term, y: provingground.HoTT.Term) =
      PlusExtendedDepFunction(first.subs(x, y),
                              second.subs(x, y),
                              depcodom.subs(x, y),
                              firstfn.subs(x, y),
                              scndfn.subs(x, y))
  }

  val A = "A" :: Type

  val B = "B" :: Type

  val C = "C" :: Type

  val f = "f" :: A ->: C

  val g = "g " :: A ->: C

  val rec = lambda(A)(
    lambda(B)(
      lambda(C)(
        lambda(f)(
          lambda(g)(PlusExtendedFunction(A, B, C, f, g))
        ))))

  val AplusB = PlusTyp(A, B)

  import AplusB.{i, j}

  val Cs = "C" :: AplusB ->: Type

  val a = "A" :: A

  val b = "B" :: B

  val C_a = lambda(a)(Cs(i(a)))

  val C_b = lambda(b)(Cs(j(b)))

  val fdep = "f" :: (a !: A) ~>: Cs(i(a))

  val gdep = "g" :: (b !: B) ~>: Cs(j(b))

  val induc = lambda(A)(
    lambda(B)(
      lambda(Cs)(
        lambda(fdep)(
          lambda(gdep)(PlusExtendedDepFunction(A, B, Cs, fdep, gdep))
        ))))
}
