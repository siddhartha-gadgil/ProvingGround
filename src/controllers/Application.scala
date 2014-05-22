package controllers

import play.api._
import play.api.mvc._
import provingGround.Logic._


// Play Json imports
import play.api.libs.json._

import play.api.Play.current

import play.api.libs.iteratee._
import play.api.libs.EventSource

import provingGround.AndrewsCurtis._
import provingGround.AndrewsCurtisInterface._

object Application extends Controller {
  
  def index = Action {
    Ok(views.html.index("Your new application is ready."))
  }
  
  def redirect = Action {
    Redirect("build/web/provingground.html")
  }
  
  def dstbnstream = Action {
      implicit request => {                   
          Ok.feed(dstbnout &> EventSource()).as("text/event-stream")
      }
  }
}
