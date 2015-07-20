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

import SimpleAcEvolution._

import scala.concurrent._

import play.api.libs.concurrent.Execution.Implicits._

import upickle.default._
/**
 * @author gadgil
 */
object AndrewsCurtisController extends Controller{
  def andrewscurtis = Action {
    Ok(views.html.jsview("andrews-curtis","ProvingGround: Andrews-Curtis"))
  }

  val (acOut, acChannel) = Concurrent.broadcast[String]

  def acstream = Action {
      implicit request => {
          Ok.feed(acOut &> EventSource()).as("text/event-stream")
      }
  }

  def acQuery = Action (parse.text) {implicit request =>
    val (header, message) = read[(String, String)](request.body)
    header match {
      case Header.evolve =>
        {
          val (rank, steps) = read[(Int, Int)](message)
          acPost(pickleInit(rank))
          evolvePost(rank, steps)
          Ok("evolution started")
        }
      case _ => Ok("TODO")
    }


  }

  def writePath(p: Path)  = write(p.pickle)

  def acPost(pickled: String) =
    acChannel.push(pickled)

  def evolvePost(rank: Int, steps: Int)  = {
    val resultFut = Future(evolve(rank, steps))
    resultFut onSuccess{
      case result =>{
        println(result)
        val pickled = pickleTriple(result, rank)
      acPost(pickled)
    }}
  }

}
