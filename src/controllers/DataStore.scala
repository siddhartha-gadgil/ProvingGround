package controllers

import provingground.AndrewsCurtisModel


import play.api._
import play.api.mvc._
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json._
import scala.concurrent.Future

// Reactive Mongo imports
import reactivemongo.api._

// Reactive Mongo plugin, including the JSON-specialized collection
import play.modules.reactivemongo.MongoController
import play.modules.reactivemongo.json.collection.JSONCollection

object DataStore extends Controller with MongoController{
  object AndrewsCurtis{
  /*
   * Chains with weights and meta-data, saved if the head has significant weight.
   */
  def chainCollection: JSONCollection = db.collection[JSONCollection]("ACchains")

  def saveChain(chain: JsValue) = chainCollection.insert(chain)
  }

}
