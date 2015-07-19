package controllers

import models.AndrewsCurtisModel


import play.api._
import play.api.mvc._
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json._
import scala.concurrent.Future

import provingground._
import andrewscurtis._

import SimpleAcEvolution._

// Reactive Mongo imports
import reactivemongo.api._

// Reactive Mongo plugin, including the JSON-specialized collection
import play.modules.reactivemongo.MongoController
import play.modules.reactivemongo.json.collection.JSONCollection

import play.api.Logger
import play.api.mvc.{ Action, Controller }
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.functional.syntax._
import play.api.libs.json._

// Reactive Mongo imports
import reactivemongo.api.Cursor






object DataStore extends Controller //with MongoController
{
  import play.api.libs.json.Json
//  implicit val stateFormat = new Json.format[State]

//  implicit val pathFormat = Json.Format[PickledPath]

  object AndrewsCurtis{
  /*
   * Chains with weights and meta-data, saved if the head has significant weight.
   */
   /*
  def chainCollection: JSONCollection = db.collection[JSONCollection]("ACchains")

  def saveChain(chain: JsValue) = chainCollection.insert(chain)*/
  }

}
