package controllers

import play.api._
import play.api.mvc._
//import provingground.Logic._

import play.api.data._
import play.api.data.Forms._


// Play Json imports
import play.api.libs.json._

import play.api.Play.current

import play.api.libs.iteratee._
import play.api.libs.EventSource

import provingground.andrewscurtis.AndrewsCurtis._
import provingground.andrewscurtis.AndrewsCurtisInterface._

import models.AndrewsCurtisModel._
import provingground.MoveLearner._

object Application extends Controller {



  def index = Action {
    Ok(views.html.index("Your new application is ready."))
  }

  def jstest = Action {
    Ok(views.html.jstest("Test of Scala Js."))
  }

  def redirect = Action {
    Redirect("build/web/provingground.html")
  }

  def acLoopStart = Action {
    Ok(views.html.acLoopStart())
  }

  
  def acLoop = Action {implicit request =>
    val presGen = presentationGenForm.bindFromRequest.get
    val learnLoop = learnerForm(presGen.feedback).bindFromRequest.get
    val finalDst = learnLoop.outerlearn(defaultdstbn)
    Ok(views.html.acLoop(???))
    }
/*
  def ACupdate = Action {
    implicit request => {
      val params = ACform.bindFromRequest.get
      params.updateGen()
      Ok(params.toString)
    }
  }
  */
  /*

  def dstbnstream = Action {
      implicit request => {
          Ok.feed(ACsource).as("text/event-stream")
      }
  }
  *
  *
  */
  /*
  def dstbnstream = Action {
      implicit request => {
          Ok.feed(dstbnout &> EventSource()).as("text/event-stream")
      }
  }
  *
  */

  val (bounceOut, bounceChannel) = Concurrent.broadcast[String]

  def bouncestream = Action {
      implicit request => {
          Ok.feed(bounceOut &> EventSource()).as("text/event-stream")
      }
  }

  case class bouncePair(value: String, mult: Int){
    def send = (0 to mult) foreach ((_) => bounceChannel.push(value))
  }

  val bounceForm = Form(
      mapping(
          "value" -> text,
          "mult" -> number)
          (bouncePair.apply)(bouncePair.unapply)
          )

  def bounce = Action {
    implicit request =>
 //     val p = bounceForm.bindFromRequest.get
      bounceChannel.push("tick:")
      println(request.body)
  //    p.send
      Ok("bounced")
  }

}
