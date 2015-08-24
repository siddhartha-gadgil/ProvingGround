package provingground

import reactivemongo.api._
import scala.concurrent.ExecutionContext.Implicits.global

import akka.actor.ActorSystem

object Hub{
  // gets an instance of the driver
  // (creates an actor system)
  val driver = new MongoDriver
  val connection = driver.connection(List("localhost"))

  // Gets a reference to the database "plugin"
  val db = connection("proving-ground")

  val system = ActorSystem("proving-ground")

}
