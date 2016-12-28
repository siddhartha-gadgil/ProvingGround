package controllers

import models.AndrewsCurtisModel

import provingground._
import andrewscurtis._

import SimpleAcEvolution._

import javax.inject.Inject

import scala.concurrent.Future

import play.api.Logger
import play.api.mvc.{Action, Controller}
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.functional.syntax._
import play.api.libs.json._

// Reactive Mongo imports
import reactivemongo.api.Cursor

import play.modules.reactivemongo.{ // ReactiveMongo Play2 plugin
  MongoController,
  ReactiveMongoApi,
  ReactiveMongoComponents
}

// BSON-JSON conversions/collection
import play.modules.reactivemongo.json._
import play.modules.reactivemongo.json.collection._

class DataStore @Inject()(val reactiveMongoApi: ReactiveMongoApi)
    extends Controller
    with MongoController
    with ReactiveMongoComponents {

  object AndrewsCurtis {
    /*
     * Chains with weights and meta-data, saved if the head has significant weight.
     */
    /*
  def chainCollection: JSONCollection = db.collection[JSONCollection]("ACchains")

  def saveChain(chain: JsValue) = chainCollection.insert(chain)*/
  }

}
