package provingground

import HoTT._

import scala.reflect.runtime.universe

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