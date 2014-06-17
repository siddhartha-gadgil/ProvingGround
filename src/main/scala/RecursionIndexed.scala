package provingGround

import provingGround.HoTT._
import provingGround.Contexts._

//import provingGround.Context.Context.empty

import scala.reflect.runtime.universe.{Try => UnivTry, Function => FunctionUniv, _}
import annotation._
/*
 * Similar to Recursion, but in families.
 * We import the indexed version of Inductive Types and associated patterns.
 */
object RecursionIndexed{
   val indxind =  new IndexedInductiveTypes[Typ[Term]]
   import indxind._
   
   
  
}