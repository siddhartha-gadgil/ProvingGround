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



import javax.inject.Inject

import play.modules.reactivemongo.{ // ReactiveMongo Play2 plugin
  MongoController,
  ReactiveMongoApi,
  ReactiveMongoComponents
}

import play.api.libs.functional.syntax._
import play.api.libs.json._
import play.api.libs.json.Json

class DataStore  @Inject() (val reactiveMongoApi: ReactiveMongoApi)  extends
Controller with MongoController with ReactiveMongoComponents{
  import play.api.libs.json.Json

  case class Dummy(x: String, y: List[Double])

  import provingground._

  import Collections._

  case class WS(elem: String, wt : Double)

  implicit val wsF = Json.format[WS]

//  implicit val wtdF = Json.format[Weighted[String]] // does not work

  case class FDS(d: List[WS])

  implicit val ff = Json.format[FDS]

  implicit val df = Json.format[Dummy]
//  implicit val stateFormat = Json.format[PickledState]

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
