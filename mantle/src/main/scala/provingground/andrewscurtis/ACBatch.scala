package provingground.andrewscurtis

import provingground._

import ACrunner._
import FDactor._

import akka.actor._

import FDhub.start

import ACData._

import ammonite.ops._

import upickle.default.{read => uread, write => uwrite, _}

import MoveGenerator._

import spray.json._

import DefaultJsonProtocol._

import ACFlowSaver._

import ACMongo._

import ACElem.Snap

import com.github.nscala_time.time.Imports._

import scala.concurrent.ExecutionContext.Implicits.global

import reactivemongo.api._
import reactivemongo.bson._

import scala.concurrent._

import akka.stream.scaladsl._

import StartData._
  /**
   * data for spawning and starting an andrews-curtis runner.
   */
  case class StartData(name: String,
      rank: Int = 2, size : Int = 1000, wrdCntn: Double = 0.1, //spawn parameters
      steps: Int = 3, strictness : Double = 1, epsilon: Double = 0.1, //start parameters
      smooth: Boolean = false
       ){

//    import StartData.quickhub
       
//    @deprecated("use mongo initialized", "to remove")
//    def initOld = (getState(name) orElse
//      (ls(wd / dir) find (_.name == name+".acstate") map (loadState))).
//      getOrElse((learnerMoves(rank), eVec))

    val p = Param(rank, size, wrdCntn)

    /**
     * initial data, from database or default
     */
    def initFut = getFutStateDefault(name, rank)

    /**
     * spawns running actor
     */
    def runner(init: (FiniteDistribution[AtomicMove], FiniteDistribution[Moves]), 
        sse: Sink[Snap, Future[Unit]] = Sink.foreach((a: Snap) => {})
        ) =
      if (!smooth)
        rawSpawn(name, rank, size, wrdCntn, init,
        ACMongo.writerRef(sse), p)
      else
        smoothSpawn(name, rank, size, wrdCntn, init,
        ACMongo.writerRef(sse), p)

//    @deprecated("use mongo initialized", "to remove") 
//    def runOld(implicit hub: ActorRef) = {
//      val r = runner(initOld)
//      start(r, steps, strictness, epsilon)(hub)
//      r
//    }

    def log = {
      val query = BSONDocument("name" -> name)
      val prevStarts = 
        actorsDB.find(query).cursor[BSONDocument]().headOption.
          map ((docOpt) => (docOpt flatMap (_.getAs[String]("start-data") map (uread[List[StartData]]))).getOrElse(List()))
      val updatedStartsFut = prevStarts map (this :: _) 
      val futDoc =  updatedStartsFut map(
          (us : List[StartData]) => BSONDocument("name" -> name, "loops" ->0, "start-data" -> uwrite(us)))
      futDoc map ((doc) => actorsDB.insert(doc))
    }
    
    /**
     * spawns and starts runner for data.
     */
    def run(sse: Sink[Snap, Future[Unit]] = Sink.foreach((a: Snap) => {})) =
      {
        val rs = initFut map ((x) => runner(x, sse))
        rs.foreach(start(_, steps, strictness, epsilon)(quickhub))
        log
        rs
      }
  }

  object StartData{
    
    implicit val quickhub = FDhub.startHub(s"FD-QuickStart-Hub")
    /**
     * reads with defaults start paratmeters from JSON.
     */
    def fromJson(st: String) = {
      val map = st.parseJson.asJsObject.fields
      val name = map("name").convertTo[String]
//      val dir = (map.get("dir") map (_.convertTo[String])) getOrElse("acDev")
      val rank = (map.get("rank") map (_.convertTo[Int])) getOrElse(2)
      val size = (map.get("size") map (_.convertTo[Int])) getOrElse(1000)
      val steps = (map.get("steps") map (_.convertTo[Int])) getOrElse(3)
      val wrdCntn = (map.get("wrdCntn") map (_.convertTo[Double])) getOrElse(0.1)
      val epsilon = (map.get("epsilon") map (_.convertTo[Double])) getOrElse(0.1)
      val smooth = (map.get("smooth") map (_.convertTo[Boolean])) getOrElse(false)
      val strictness = (map.get("strictness") map (_.convertTo[Double])) getOrElse(1.0)
      StartData(name, rank, size, wrdCntn, steps, strictness, epsilon, smooth)
    }
  }


object ACBatch {
  val wd = cwd / 'data



  /**
   * start data before json parsing 
   */
  def loadRawStartData(dir: String = "acDev", file: String = "acbatch.json") = {
    val jsFile = if (file.endsWith(".json")) file else file+".json"
    val js  = ammonite.ops.read.lines(wd / dir/ jsFile) filter((l) => !(l.startsWith("#")))
    println(js)
    js
  }

  /**
   * start data from file.
   */
  def loadStartData(dir: String = "acDev", file: String = "acbatch.json") =
    {
    val d = loadRawStartData(dir, file)
    d map (StartData.fromJson)
    }

  

//  @deprecated("use mongo initialized", "to remove")
//  def quickStartOld(dir: String = "acDev", file: String = "acbatch.json") = {
//    val ds = loadStartData(dir, file)
//    ds foreach ((d) => write.append(wd / dir /file, s"# Started: $d at ${DateTime.now}"))
//    val runners = ds map (_.runOld(StartData.quickhub))
//    runners
//  }

  /**
   * Load start data, start actors, return references to these.
   */
  def quickStart(dir: String = "acDev", file: String = "acbatch.json") = {
    val ds = loadStartData(dir, file)
    val runners = ds map (_.run())
    runners
  }
}
