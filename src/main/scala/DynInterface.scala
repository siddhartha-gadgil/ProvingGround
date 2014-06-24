package provingGround

import play.api.libs.json._

import play.api.libs.EventSource

import play.api.Play.current

import scala.concurrent._

import play.api.libs.iteratee._
import play.api.libs.EventSource
import play.api.libs.concurrent._

import akka.actor._
import play.api.libs.concurrent.Execution.Implicits._

object DynInterface{
  class Generator[A](init: A, initdyn: A => A, initIsStable : (A, A) => Boolean = ((_ : A, _ : A) => false)){
    var isStable = initIsStable
    var state = init
    var dyn = initdyn
    def genOpt = future{
      val next = dyn(state)
      if (isStable(state, next)) None else {state = next; Some(next)}
    }
    
    def enumerator = Enumerator.generateM(genOpt)
    
    def source(implicit jsEnum : Enumeratee[A, String]) = enumerator &> jsEnum &> EventSource() 
  }
}