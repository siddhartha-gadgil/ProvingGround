package provingground.andrewscurtis

import provingground._, learning._

import ACLooper._

//import ammonite.ops._

import upickle.default.{read => uread, write => uwrite, _}

import ACMongo._

import ACElem.Snap

import scala.concurrent.ExecutionContext.Implicits.global

//import reactivemongo.api._
import reactivemongo.bson._

import scala.concurrent._

import akka.stream.scaladsl._

import StartData._

import upickle.default.{ReadWriter => RW, macroRW}

/**
  * data for spawning and starting an andrews-curtis runner.
  */
case class StartData(name: String,
                     rank: Int = 2,
                     size: Int = 1000,
                     wrdCntn: Double = 0.1, //spawn parameters
                     steps: Int = 3,
                     strictness: Double = 1,
                     epsilon: Double = 0.1, //start parameters
                     smooth: Boolean = false) {

  val p = Param(rank, size, wrdCntn)

  /**
    * initial data, from database or default
    */
  def initFut = getFutStateDefault(name, rank)

  /**
    * spawns running actor
    */
  def runner(
      init: (FiniteDistribution[AtomicMove], FiniteDistribution[Moves]),
      sse: Sink[Snap, Future[akka.Done]] = Sink.foreach((a: Snap) => {})) =
    if (!smooth)
      spawn(name, rank, size, wrdCntn, init, ACMongo.writerRef(sse), p)
    else
      smoothSpawn(name, rank, size, wrdCntn, init, ACMongo.writerRef(sse), p)

  def log = {
    val query = BSONDocument("name" -> name)
    val prevStarts = actorsDB.flatMap(_.
      find(query)
      .cursor[BSONDocument]()
      .headOption
      .map(
        (docOpt) =>
          (docOpt flatMap
            (_.getAs[String]("start-data") map ((s: String) =>
              uread[List[StartData]](s))))
            .getOrElse(List())))
    val updatedStartsFut = prevStarts map (this :: _)
    val futDoc =
      updatedStartsFut map
        ((us: List[StartData]) =>
          BSONDocument("name"       -> name,
                       "loops"      -> 0,
                       "start-data" -> uwrite(us)))
    val res = futDoc.map(
      (doc) =>
        //println(doc)
        actorsDB.foreach(_.insert(doc)))
    (futDoc, res)
  }

  /**
    * spawns and starts runner for data.
    */
  def run(
      sse: Sink[Snap, Future[akka.Done]] = Sink.foreach((a: Snap) => {})) = {
    val rs = initFut map ((x) => runner(x, sse))
    rs.foreach(FDHub.start(_, steps, strictness, epsilon)(quickhub))
    log
    rs
  }
}

object StartData {

  implicit def rw: RW[StartData] = macroRW

  import spray.json._

  import DefaultJsonProtocol._

  implicit val quickhub: _root_.akka.actor.ActorRef =
    FDHub.startHub(s"FD-QuickStart-Hub")

  /**
    * reads with defaults start paratmeters from JSON.
    */
  def fromJson(st: String) = {
    val map  = st.parseJson.asJsObject.fields
    val name = map("name").convertTo[String]
    //      val dir = (map.get("dir") map (_.convertTo[String])) getOrElse("acDev")
    val rank  = (map.get("rank") map (_.convertTo[Int])) getOrElse (2)
    val size  = (map.get("size") map (_.convertTo[Int])) getOrElse (1000)
    val steps = (map.get("steps") map (_.convertTo[Int])) getOrElse (3)
    val wrdCntn =
      (map.get("wrdCntn") map (_.convertTo[Double])) getOrElse (0.1)
    val epsilon =
      (map.get("epsilon") map (_.convertTo[Double])) getOrElse (0.1)
    val smooth =
      (map.get("smooth") map (_.convertTo[Boolean])) getOrElse (false)
    val strictness =
      (map.get("strictness") map (_.convertTo[Double])) getOrElse (1.0)
    StartData(name, rank, size, wrdCntn, steps, strictness, epsilon, smooth)
  }
}
