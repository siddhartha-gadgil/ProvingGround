package controllers

import play.api._
import play.api.mvc._


import play.api.Play.current

import play.api.libs.iteratee._
import play.api.libs.EventSource

import provingground._

import andrewscurtis._

import ACevolution._

import FiniteDistribution._

/**
 * @author gadgil
 */
object AndrewsCurtisController extends Controller{
  val (acOut, acChannel) = Concurrent.broadcast[String]

  def acstream = Action {
      implicit request => {
          Ok.feed(acOut &> EventSource()).as("text/event-stream")
      }
  }

  def acpost(pickled: String) = 
    acChannel.push(pickled)
  
}