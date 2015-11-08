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

object ACBatch {
  val wd = cwd / 'data
  
  case class StartData(name: String, dir : String = "acDev",
      rank: Int = 2, size : Int = 1000, wrdCntn: Double = 0.1, //spawn parameters
      steps: Int = 3, strictness : Double = 1, epsilon: Double = 0.1, //start parameters
      smooth: Boolean = false
       ){
    def init = 
      (ls(wd / dir) find (_.name == name+".acstate") map (loadState)).
      getOrElse((learnerMoves(rank), eVec))
    val p = Param(rank, size, wrdCntn, dir)
    val runner = 
      if (!smooth) 
        rawSpawn(name, rank, size, wrdCntn, init, 
        fileSave(name, dir, (_ : Unit) => (), rank))
      else
        smoothSpawn(name, rank, size, wrdCntn, init, 
        fileSave(name, dir, (_ : Unit) => (), rank))
    def run(implicit hub: ActorRef) = {
      start(runner, steps, strictness, epsilon)
      runner
    }
  }
    
  def loadStartData(dir: String = "acDev", file: String = "acbatch.json") = {
    val jsFile = if (file.endsWith(".json")) file else file+".json"
    val js = read(wd / jsFile)
    uread[List[StartData]](js)
  }
  
  def quickStart(dir: String = "acDev", file: String = "acbatch.json") = {
    val ds = loadStartData(dir, file)
    implicit val hub = FDhub.startHub(s"FD-QuickStart-Hub:$dir:$file")
    val runners = ds map (_.run(hub))
    (runners, hub)
  }
}