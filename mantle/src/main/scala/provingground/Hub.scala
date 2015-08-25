package provingground

import reactivemongo.api._
import scala.concurrent.ExecutionContext.Implicits.global

import akka.actor.ActorSystem

import com.typesafe.config._

object Hub{
  // gets an instance of the driver
  // (creates an actor system)

  val conf = ConfigFactory.load("hub.conf")

  val cnf = conf.
    withValue("akka.loglevel", ConfigValueFactory.fromAnyRef("INFO")).
     withValue("logger.reactivemongo", ConfigValueFactory.fromAnyRef("INFO")).
     withValue("mongo-async-driver.akka.loglevel", ConfigValueFactory.fromAnyRef("INFO")).
    withValue("reactivemongo-akka.loglevel", ConfigValueFactory.fromAnyRef("INFO")).
    withValue("reactivemogo-akka.actor.loglevel", ConfigValueFactory.fromAnyRef("INFO"))

  println(cnf.getString("akka.loglevel"))

  val driver = new MongoDriver(Some(cnf))
  val connection = driver.connection(List("localhost"))

  // Gets a reference to the database "plugin"
  implicit val db : DefaultDB = connection("proving-ground")

  val system = ActorSystem("proving-ground")

}
