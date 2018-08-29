package provingground.library
import provingground._
import HoTT._
import induction._
import implicits._
import shapeless._
import Fold._

import interface._
import monix.eval.Task
object LeanMemo {
  def defMap: Map[trepplein.Name, Term] = Map()
  def indMap: Map[trepplein.Name, TermIndMod] = Map()

  def defTaskMap: Map[trepplein.Name, Task[Term]] = Map()

  def indTaskMap: Map[trepplein.Name, Task[TermIndMod]] = Map()

}
