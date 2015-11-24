package provingground.andrewscurtis

import provingground._

import akka.stream._


import ACFlow._

import ACElem._

import akka.stream.scaladsl.{Source => Src, _}

import akka.actor._


/**
 * Saving the results of Andrews-Curtis runs
 * abstract methods are for the various saves and updates
 * concrete methods give sinks, a flow that does all the saving.
 */
trait ACWriter {
  def addElem(el: ACElem)

  def addThm(thm: ACThm)

  def addMoveWeight(wts: ACMoveWeights)

  def updateLoops(name: String, loops: Int)

  val elemsSink = elemsFlow to Sink.foreach(addElem)

  val thmsSink = thmsFlow to Sink.foreach(addThm)

  val moveWeightsSink = moveWeightsFlow to Sink.foreach(addMoveWeight)

  val loopsSink =
    loopsFlow to Sink.foreach({case (name, loops) => updateLoops(name, loops)})

  val writerFlow =
    fl alsoTo
    elemsSink alsoTo
    thmsSink alsoTo
    moveWeightsSink alsoTo
    loopsSink

  /**
   *  ActorRef from materialized flow saving various things in Casbah mongo database
   */
  def writerRef[M](
      interface: Sink[Snap
        , M]
        = Sink.foreach(
            (x : Snap) =>{})) = {
    val sink = writerFlow to interface
    sink.runWith(src)
  }
}
