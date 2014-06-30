package controllers

import play.api._
import play.api.mvc._
import provingGround.Logic._

import play.api.data._
import play.api.data.Forms._


// Play Json imports
import play.api.libs.json._

import play.api.Play.current

import play.api.libs.iteratee._
import play.api.libs.EventSource

import provingGround.AndrewsCurtis._
import provingGround.AndrewsCurtisInterface._
import provingGround.AndrewsCurtisModel._

object Application extends Controller {
  
  
  
  def index = Action {
    Ok(views.html.index("Your new application is ready."))
  }
  
  
  
  def redirect = Action {
    Redirect("build/web/provingground.html")
  }
  
  def ACupdate = Action {
    implicit request => {
      val params = ACform.bindFromRequest.get
      params.updateGen()
      Ok(params.toString)
    }
  }
  
  def dstbnstream = Action {
      implicit request => {                   
          Ok.feed(ACsource).as("text/event-stream")
      }
  }
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
      val p = bounceForm.bindFromRequest.get
      bounceChannel.push("tick")
      p.send
      Ok("bounced")    
  }

}
