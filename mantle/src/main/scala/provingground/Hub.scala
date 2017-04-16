package provingground

// import com.mongodb.casbah._
import reactivemongo.api._
import scala.concurrent.ExecutionContext.Implicits.global

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer

import com.typesafe.config._
// import com.mongodb.casbah.Imports

object Hub {
  // gets an instance of the driver
  // (creates an actor system)

  val conf = ConfigFactory.load("hub.conf")

  val cnf = conf
    .withValue("akka.loglevel", ConfigValueFactory.fromAnyRef("INFO"))
    .withValue("logger.reactivemongo", ConfigValueFactory.fromAnyRef("INFO"))
    .withValue("mongo-async-driver.akka.loglevel",
               ConfigValueFactory.fromAnyRef("INFO"))
    .withValue("mongo-async-driver.loglevel",
               ConfigValueFactory.fromAnyRef("INFO"))
    .withValue("reactivemongo-akka.loglevel",
               ConfigValueFactory.fromAnyRef("INFO"))
    .withValue("mongo-async-driver.akka.stdout-loglevel",
               ConfigValueFactory.fromAnyRef("INFO"))
    .withValue("mongo-async-driver.stdout-loglevel",
               ConfigValueFactory.fromAnyRef("INFO"))
    .withValue("reactivemogo-akka.actor.loglevel",
               ConfigValueFactory.fromAnyRef("INFO"))

  println(
    cnf
      .getConfig("mongo-async-driver")
      .getConfig("akka")
      .getString("loglevel"))
  println(cnf.getConfig("mongo-async-driver").getString("loglevel"))
  println(
    cnf
      .getConfig("mongo-async-driver")
      .getConfig("akka")
      .getString("stdout-loglevel"))
  println(cnf.getConfig("mongo-async-driver").getString("stdout-loglevel"))

  object ReactiveMongo {
    lazy val driver     = new MongoDriver(Some(cnf))
    lazy val connection = driver.connection(List("localhost"))

    // Gets a reference to the database "plugin"
    implicit lazy val db: DefaultDB = connection("provingground")
  }

  // object Casbah {
  //   lazy val mongoClient = MongoClient()
  //   lazy val db          = mongoClient("provingground")
  // }

  implicit val system = ActorSystem("provingground")

  implicit val materializer = ActorMaterializer()

  implicit val executionContext = system.dispatcher
}
