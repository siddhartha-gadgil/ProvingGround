package provingground

import HoTT._

import scala.reflect.runtime.universe

import scala.util._

/**
  * Reflection based picking for case objects which are terms.
  */
object ReflPickle {
  def pickle(t: Term) = t.getClass.getName

  lazy val mirror = universe.runtimeMirror(getClass.getClassLoader)

  def unpickle(str: String) = {
    val module = mirror.staticModule(str)
    val obj = mirror.reflectModule(module)
    obj.instance.asInstanceOf[Term]
  }
}

object TermObj {
  import ReflPickle._

  def apply(t: Term) = {
    val p = pickle(t)
    require(Try(unpickle(p)) == Success(t),
            s"could not pickle $t as term object, may not be a singleton")
    p
  }

  def unapply(str: String) = Try(unpickle(str)).toOption

  val objCase: PartialFunction[String, Term] = {
    case TermObj(str) => str
  }
}
