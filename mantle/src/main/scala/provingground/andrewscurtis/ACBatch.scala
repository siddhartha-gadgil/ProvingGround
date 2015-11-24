package provingground.andrewscurtis

import provingground._

import ACrunner._
import FDactor._

import akka.actor._

import FDhub.start

import ACData._

import ammonite.ops._

import upickle.default.{read => uread, write => uwrite, _}

import SimpleAcEvolution._

import spray.json._

import DefaultJsonProtocol._

import ACFlowSaver._

import ACMongo._

import com.github.nscala_time.time.Imports._

import scala.concurrent.ExecutionContext.Implicits.global

object ACBatch {
  val wd = cwd / 'data
  
  case class StartData(name: String, dir : String = "acDev",
      rank: Int = 2, size : Int = 1000, wrdCntn: Double = 0.1, //spawn parameters
      steps: Int = 3, strictness : Double = 1, epsilon: Double = 0.1, //start parameters
      smooth: Boolean = false
       ){

    def initOld = (getState(name) orElse
      (ls(wd / dir) find (_.name == name+".acstate") map (loadState))).
      getOrElse((learnerMoves(rank), eVec))

    val p = Param(rank, size, wrdCntn, dir)
    
    def initFut = getFutState(name, rank)
    
    def runner(init: (FiniteDistribution[AtomicMove], FiniteDistribution[Moves])) = 
      if (!smooth) 
        rawSpawn(name, rank, size, wrdCntn, init, 
        ACMongo.writerRef(), p)
      else
        smoothSpawn(name, rank, size, wrdCntn, init, 
        ACMongo.writerRef(), p)
    
    def runOld(implicit hub: ActorRef) = {
      val r = runner(initOld)
      start(r, steps, strictness, epsilon)(hub)
      r
    }
    
    def run =
      {
        val rs = initFut map (runner)
        rs.foreach(start(_, steps, strictness, epsilon))
        rs
      }
  }
  
  object StartData{
    def fromJson(st: String) = {
      val map = st.parseJson.asJsObject.fields
      val name = map("name").convertTo[String]
      val dir = (map.get("dir") map (_.convertTo[String])) getOrElse("acDev")
      val rank = (map.get("rank") map (_.convertTo[Int])) getOrElse(2)
      val size = (map.get("size") map (_.convertTo[Int])) getOrElse(1000)
      val steps = (map.get("steps") map (_.convertTo[Int])) getOrElse(3)
      val wrdCntn = (map.get("wrdCntn") map (_.convertTo[Double])) getOrElse(0.1)
      val epsilon = (map.get("epsilon") map (_.convertTo[Double])) getOrElse(0.1)
      val smooth = (map.get("smooth") map (_.convertTo[Boolean])) getOrElse(false)
      val strictness = (map.get("strictness") map (_.convertTo[Double])) getOrElse(1.0)
      StartData(name, dir, rank, size, wrdCntn, steps, strictness, epsilon, smooth)
    }
  }
  
  
  
  def loadRawStartData(dir: String = "acDev", file: String = "acbatch.json") = {
    val jsFile = if (file.endsWith(".json")) file else file+".json"
    val js  = ammonite.ops.read.lines(wd / dir/ jsFile) filter((l) => !(l.startsWith("#")))
    println(js)
    js
  }
  
  def loadStartData(dir: String = "acDev", file: String = "acbatch.json") =
    {
    val d = loadRawStartData(dir, file) 
    d map (StartData.fromJson)
    }
  
  implicit val quickhub = FDhub.startHub(s"FD-QuickStart-Hub")
  
  def quickStartOld(dir: String = "acDev", file: String = "acbatch.json") = {
    val ds = loadStartData(dir, file)
    ds foreach ((d) => write.append(wd / dir /file, s"# Started: $d at ${DateTime.now}"))
    val runners = ds map (_.runOld(quickhub))
    runners
  }
  
  def quickStart(dir: String = "acDev", file: String = "acbatch.json") = {
    val ds = loadStartData(dir, file)
    val runners = ds map (_.run)
    runners
  }
}