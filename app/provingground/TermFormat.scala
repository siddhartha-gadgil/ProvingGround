package provingground
import HoTT._

object TermFormat {
  trait TermFormat[U] {
    def fromString(str: String): U

    def appln(func: U, arg: U): U

    def arrow(dom: U, codom: U): U

    def lambda(variable: U, value: U): U

    def pi(fibre: U): U

    def sigma(fibre: U): U

    def plus(first: U, scnd: U): U

    def symbobj(term: SymbObj[Term]): U

    def symbtyp(term: SymbTyp): U

    def apply(term: Term): U = term match {
      case applptnterm(func, arg) => appln(apply(func), apply(arg))
      case LambdaFixed(x, y: Term) => lambda(apply(x), apply(y))
      case Lambda(x, y: Term) => lambda(apply(x), apply(y))
      case PiTyp(fibre) => pi(apply(fibre))
      case SigmaTyp(fibre) => sigma(apply(fibre))
      case PlusTyp(fisrt, scnd) => plus(apply(fisrt), apply(scnd))
      case fn: FuncTyp[_, _] => arrow(apply(fn.dom), apply(fn.codom))
      case sym: SymbObj[_] => symbobj(sym)
      case sym: SymbTyp => symbtyp(sym)
      case _ => fromString(term.toString)
    }
  }

  implicit class StringFormat(syms: TermSyms) extends TermFormat[String] {
    def fromString(str: String): String = str

    def appln(func: String, arg: String): String = func+ "(" + arg +")" 

    def arrow(dom: String, codom: String): String = dom + syms.Arrow + codom

    def lambda(variable: String, value: String): String = variable + syms.MapsTo + value

    def pi(fibre: String): String = syms.Pi + fibre

    def sigma(fibre: String): String = syms.Sigma + fibre

    def plus(first: String, scnd: String): String = first+"+" + scnd

    def symbobj(term: SymbObj[Term]): String = term.name.toString +" : "+ this(term.typ)

    def symbtyp(typ: SymbTyp): String = typ.toString + " : _"
  }
}