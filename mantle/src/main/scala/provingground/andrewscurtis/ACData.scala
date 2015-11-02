package provingground.andrewscurtis

import ammonite.ops._

import provingground._

import Collections._

import upickle.default.{write => uwrite, read => uread, _}

import LinearStructure._

import FiniteDistribution._

import SimpleAcEvolution._

import ACrunner._

import akka.actor._

import ACData._

case class ACData(
    paths: Map[String, Vector[(FiniteDistribution[AtomicMove], FiniteDistribution[Moves])]],
    dir : String) extends ACresults(paths){

  def revive(name : String, p : ACrunner.Param = Param())(implicit hub: ActorRef) = {
    import p.{dir => d, _}
    import SimpleAcEvolution._
    val state = states(name)
    val ref = rawSpawn(name, rank, size, wrdCntn, state, ACData.fileSave(name, dir, alert))
//    FDhub.start(ref)
    ref
  }

  def reviveAll(p : ACrunner.Param = Param())(implicit hub: ActorRef) = {
    val refs = for (name <- names) yield revive(name, p)
    refs
  }
  
  def restartAll(file: String = "acstates.dat", p : ACrunner.Param = Param())(implicit hub: ActorRef) = {
    val states = loadStates(dir, file)
    val refs = for (name <- names) yield {
      val runner = revive(name, p)
      val optstate = states.get(name)
      optstate map ((state) => FDhub.start(runner, state.steps, state.strictness, state.epsilon))
      runner
    }
    
    refs
  }

  def spawn(name : String, p : ACrunner.Param = Param()) = {
    import p.{dir => d, _}
    import SimpleAcEvolution._
    rawSpawn(name, rank, size, wrdCntn, blended, ACData.fileSave(name, dir, alert))
  }

  def spawns(name: String, mult : Int = 4, p: Param = Param()) = {
    for (j <- 1 to mult) yield spawn(name+"."+j.toString, p)
  }

  def resetFiles() = {
    for ((name, data) <- paths) yield {
      write.over(wd /dir / name, pickle(data.last))
    }
  }
}

object ACData {


  def pickle(state: (FiniteDistribution[AtomicMove], FiniteDistribution[Moves])) = {
    val fdM = state._1
    val fdV = state._2
    val pmfM = for (Weighted(m, p) <- fdM.pmf) yield (PickledWeighted(uwrite(m), p))
    val pmfV = for (Weighted(v, p) <- fdV.pmf) yield (PickledWeighted(uwrite(v), p))
    val s = uwrite((pmfM, pmfV))
//    println(unpickle(s)) // a test
    s
  }

  def unpickle(str: String) = {
    val fdStrings = uread[(Vector[PickledWeighted], Vector[PickledWeighted])](str)
    val pmfM = fdStrings._1 map (
        (w : PickledWeighted) => w map ((x) => uread[AtomicMove](x)))
    val pmfV = fdStrings._2 map (
        (w : PickledWeighted) => w map ((x) => uread[Moves](x)))
    (FiniteDistribution(pmfM), FiniteDistribution(pmfV))
  }

  val wd = cwd / "data"
  def fileSave(name: String, dir : String ="acDev", alert : Unit => Unit = (x) => ())
    (fdM : FiniteDistribution[AtomicMove], fdV : FiniteDistribution[Moves]) = {
      val file = wd / dir / (name+".acrun")
      write.append(file, s"${pickle(fdM, fdV)}\n")
      alert(())
  }

  def load(name: String, dir : String ="acDev") = {
    val file = wd / dir / (name+".acrun")
    val lines = read.lines(file)
    lines map (unpickle)
  }

  def loadAll(dir : String ="acDev") = {
    val fileNames = ls(wd / dir) filter (_.ext == "acrun") map (_.name.dropRight(6))
    (for (name <- fileNames) yield (name, load(name, dir))).toMap
  }

  def loadData(dir : String ="acDev") = ACData(loadAll(dir), dir)

  def saveFD[T](file: String, dir: String = "0.5-output", fd: FiniteDistribution[T])={
    for (Weighted(x, p) <- fd.pmf) write.append(wd / file, s"$x, $p\n")
  }

  def saveEntropy(file: String, dir: String = "0.5-output", ent: List[Weighted[String]]) = {
    for (Weighted(x, p) <- ent) write.append(wd / file, s"$x, $p\n")
  }
  
  import FDhub._
  
  import Hub.system
  
  import scala.concurrent.ExecutionContext.Implicits.global
  
  def saveStates(st : Map[String, FDactor.State], dir: String = "acDev", file: String = "acstates.dat") = {
    rm(wd / dir /file)
    st.foreach {ns => {
      val pickled = uwrite(ns)
      write.over(wd /dir /file, pickled)
      }
       }
  }
  
  def saveHubStates(dir: String = "acDev", file: String = "acstates.dat")(implicit hub : ActorRef) = {    
    val s = states
    s.foreach(saveStates(_, dir, file))            
  }
  
  def loadStates(dir: String = "acDev", file: String = "acstates.dat") = 
    (read.lines(wd /dir /file) map (uread[(String, FDactor.State)])).toMap
 
  def restart(states: Map[String, State])(implicit hub: ActorRef) = {
    states.foreach {
      case (name, st) =>
    //    start(name)
    }
  }
    
  def restartAll(dir: String = "acDev", file: String = "acstates.dat")(implicit hub: ActorRef) = {
    
  }
  
  def run(dir: String = "acDev", file: String = "acstates.dat", rank: Int = 2, size : Int = 1000, wrdCntn: Double = 0.5) = {
    implicit val hub = FDhub.startHub
    
    val data = loadData()
    
    data.restartAll(file, ACrunner.Param(rank, size, wrdCntn, dir))
  }
    
}
