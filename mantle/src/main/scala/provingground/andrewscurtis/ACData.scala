package provingground.andrewscurtis

import ammonite.ops._

import provingground._

import Collections._

import upickle.default.{write => uwrite, read => uread, _}

import LinearStructure._

import FiniteDistribution._

import SimpleAcEvolution._

import ACrunner._

import scala.io.Source

import akka.actor._

import FreeGroups._

import ACData._

import akka.stream._

import akka.stream.scaladsl.{Source => Src, _}

import com.mongodb.casbah.Imports._

case class ACData(
    paths: Map[String, Stream[(FiniteDistribution[AtomicMove], FiniteDistribution[Moves])]],
    dir : String) extends ACresults(paths){

  def last = ACStateData(states, dir)

  def take(n: Int) = ACData(paths mapValues(_ take (n)), dir)



  def resetFiles() = {
    for ((name, data) <- paths) yield {
      write.over(wd /dir / name, pickle(data.toVector.last))
    }
  }

  def thmCSV(name: String, rank: Int = 2) = {
    val supp = thmSupp(name, rank)
    val file = wd / dir / s"$name.csv"
    rm(file)
    val tVec = thmVec(name, rank)
    def pVec(p : FreeGroups.Presentation) =
      tVec map ((fd) => fd(p))
    supp.foreach((p) => {
      write.append(file, s""""$p",${pVec(p).mkString(",")}\n""")
    })
  }
}

case class ACStateData(
    states: Map[String, (FiniteDistribution[AtomicMove], FiniteDistribution[Moves])],
    dir : String) extends ACStates{
    def revive(name : String, p : ACrunner.Param = Param())(implicit hub: ActorRef) = {
    import p.{dir => d, _}
    import SimpleAcEvolution._
    val state = states(name)
    val ref = rawSpawn(name, rank, size, wrdCntn, state, ACData.srcRef(dir, rank), p)
//    FDhub.start(ref)
    ref
  }

  def reviveAll(p : ACrunner.Param = Param())(implicit hub: ActorRef) = {
    val refs = for (name <- names) yield revive(name, p)
    refs
  }


  def spawn(name : String, p : ACrunner.Param = Param()) = {
    import p.{dir => d, _}
    import SimpleAcEvolution._
    rawSpawn(name, rank, size, wrdCntn, blended, ACData.srcRef(dir, rank), p)
  }

  def spawns(name: String, mult : Int = 4, p: Param = Param()) = {
    for (j <- 1 to mult) yield spawn(name+"."+j.toString, p)
  }
}

import FDactor._

class ACFileSaver(dir: String = "acDev", rank: Int = 2) extends FDsrc[(FiniteDistribution[AtomicMove], FiniteDistribution[Moves]), Param] {
  def save =
    (snap : SnapShot[(FiniteDistribution[AtomicMove], FiniteDistribution[Moves]), Param]) =>
      fileSave(snap.name, dir, rank)(snap.state._1, snap.state._2)
}


object ACFileSaver{
  def props(dir: String = "acDev", rank: Int) = Props(new ACFileSaver(dir, rank))


  def actorRef(dir: String = "acDev", rank: Int) = Hub.system.actorOf(props(dir, rank))
}

case class ACElem(name: String, moves: Moves, rank: Int, pres: Presentation, weight: Double, loops: Int)

object ACElem{
  def fromSnap =
    (snap: SnapShot[(FiniteDistribution[AtomicMove], FiniteDistribution[Moves]), Param]) =>
  {
    val d = snap.state._2
    d.supp map ((x) => {
      val rank = snap.param.rank
      ACElem(snap.name, x, rank, Moves.actOnTriv(rank)(x).get, d(x), snap.loops)
      } )
  }
}

case class ACThm(name: String, pres: Presentation, weight: Double, loops: Int)

object ACThm{
  def fromSnap =
    (snap: SnapShot[(FiniteDistribution[AtomicMove], FiniteDistribution[Moves]), Param]) =>
  { val rank = snap.param.rank
    val d = toPresentation(rank, snap.state._2)
    d.supp map ((x) => {      
      ACThm(snap.name, x, d(x), snap.loops)
      } )
  }
}

object ACFlowSaver{
  implicit val system = Hub.system

  implicit val mat  = ActorMaterializer()

  val src =
    Src.actorRef[SnapShot[(FiniteDistribution[AtomicMove], FiniteDistribution[Moves]), Param]](100, OverflowStrategy.dropHead)

  def fileSaver(dir: String = "acDev", rank: Int = 2) =
    Sink.foreach {
    (snap: SnapShot[(FiniteDistribution[AtomicMove], FiniteDistribution[Moves]), Param]) =>
      fileSave(snap.name, dir, rank)(snap.state._1, snap.state._2)
  }

  val loopsFlow = Flow[SnapShot[(FiniteDistribution[AtomicMove], FiniteDistribution[Moves]), Param]] map {
    (snap) => (snap.name, snap.loops)
  }
  
  val thmsFlow = 
    Flow[SnapShot[(FiniteDistribution[AtomicMove], FiniteDistribution[Moves]), Param]] mapConcat(ACThm.fromSnap)

  def elemsFlow =
    Flow[SnapShot[(FiniteDistribution[AtomicMove], FiniteDistribution[Moves]), Param]] mapConcat(ACElem.fromSnap)

  def fdMFlow =     
    Flow[SnapShot[(FiniteDistribution[AtomicMove], FiniteDistribution[Moves]), Param]] map {
    (snap) => (snap.name, snap.state._1)
  }

    def actorRef(dir: String = "acDev", rank: Int = 2) =
  {
    val sink = fileSaver(dir, rank)
    sink.runWith(src)
  }

  import Hub.Casbah._

  val elems = db("ACElems")

  val actorData = db("ACActorData")
  
  val fdMdb = db("AC-FDM")
  
  val thmsdb = db("AC-THMS")

  def saveElem(elem: ACElem) = {
    import elem._
    val obj =
      MongoDBObject(
        "name" -> name,
        "moves" -> uwrite(moves),
        "presentation" -> uwrite(pres),
        "loops" -> loops)
    elems.insert(obj)
  }

  def saveThm(thm: ACThm) = {
    import thm._
    val obj =
      MongoDBObject(
        "name" -> name,
        "presentation" -> uwrite(pres),
        "loops" -> loops)
    elems.insert(obj)
  }
  
  def elemsCashbahSave =
    elemsFlow to Sink.foreach(saveElem)

  def thmsCashbahSave =
    thmsFlow to Sink.foreach(saveThm)  
    
  def saveLoops(name: String, loops: Int) = {
    val query = MongoDBObject("name" -> name)
    val update = $set("loops" -> loops)
    val exists = !(actorData.findOne(query).isEmpty)
    if (exists) actorData.update(query, update)
    else actorData.insert(MongoDBObject("name" -> name, "loops" -> loops))
  }
  
  def saveFDM(name: String, fdM : FiniteDistribution[AtomicMove]) = {
    val query = MongoDBObject("name" -> name)
    val entry = MongoDBObject("name" -> name, "fdM" -> uwrite(fdM))
    fdMdb.update(query, entry, upsert = true)
  }

  val loopsCasbahSave =
    loopsFlow to Sink.foreach {case (name, loops) => saveLoops(name, loops)}

  val fdmCasbahSave = 
    fdMFlow to Sink.foreach {case (name, fdm) => saveFDM(name, fdm)}
    
  def mongoSaveRef(ranks: Map[String, Int] = Map()) = {
    val fl = Flow[SnapShot[(FiniteDistribution[AtomicMove], FiniteDistribution[Moves]), Param]]
    val sink = fl alsoTo loopsCasbahSave alsoTo fdmCasbahSave alsoTo thmsCashbahSave  to elemsCashbahSave
    sink.runWith(src)
  }
  
  def presWeights(name: String, pres: Presentation) = 
  {
    val query = MongoDBObject("presentation" -> uwrite(pres), "name" -> name)
    val sorter = MongoDBObject("loops" -> 1)
    val cursor = elems.find(query)
    for (e <- cursor) yield e.getAs[Double]("weight")
  }
  
  def FDV(name: String, loops: Int) = {
    val query = MongoDBObject("loops" -> loops, "name" -> name)
    val cursor = elems.find(query)
    val pmf = 
      (for (c <- cursor) yield {
        for (pp <- c.getAs[String]("moves");
        wt <- c.getAs[Double]("weight"))
          yield Weighted(uread[Moves](pp), wt)
      }).toVector.flatten
    FiniteDistribution(pmf)
  }
  
  def FDM(name: String) = {
    val query = MongoDBObject("name" -> name)
    (fdMdb.findOne(query).
          flatMap (_.getAs[String]("fdM"))) map (uread[FiniteDistribution[AtomicMove]])
  }
}


object ACData {
  def srcRef(batch: String = "acDev", rank: Int) = ACFlowSaver.actorRef(batch, rank)


  def thmFileCSV(dir : String = "acDev", file: String,
       rank: Int = 2, lines: Option[Int] = None) = {
    val source = wd / dir / s"${file}.acthms"
    val target = wd / dir / s"${file}.acthms.csv"
    val l =
      (lines map ((n) =>
        ((read.lines!!(source)).take(n)).toVector
        )).
        getOrElse(read.lines(source))
    val pmfs = l map (uread[Vector[(String, Double)]])
    val tVec =
      pmfs map ((pmf => FiniteDistribution(pmf map ((xp) => Weighted(xp._1, xp._2)))))
    val supp = (tVec map (_.supp.toSet) reduce (_ union _)).toVector
    def pVec(p : String) =
      tVec map ((fd) => fd(p))
    supp.foreach((p) => {
      write.append(target, s""""$p",${pVec(p).mkString(",")}\n""")
    })
  }

  def thmFiles(dir : String = "acDev", s : String => Boolean = (x) => true) = {
    ls( wd / dir) filter ((file) => file.ext == "acthms" && s(file.name.dropRight(7)))
  }

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
  def fileSave(name: String, dir : String ="acDev",
      rank : Int = 2)
    (fdM : FiniteDistribution[AtomicMove], fdV : FiniteDistribution[Moves]) = {
      val file = wd / dir / (name+".acrun")
      val statefile = wd / dir / (name+".acstate")
      val thmfile = wd / dir / (name+".acthms")
      write.append(file, s"${pickle(fdM, fdV)}\n")
      write.over(statefile, s"${pickle(fdM, fdV)}\n")
      def writethms = {
        val thms = (toPresentation(2, fdV) map (_.toString)).
          flatten.pmf map {case Weighted(a, p) => (a, p)}
        write.append(thmfile, s"${uwrite(thms)}\n")
      }
      import scala.concurrent._
      import scala.concurrent.ExecutionContext.Implicits.global
      Future(writethms)
  }

  def load(name: String, dir : String ="acDev") = {
    val file = wd / dir / (name+".acrun")
    val lines = (read.lines!!(file)).toStream
    lines map (unpickle)
  }

  def loadFinal(name: String, dir : String ="acDev") = {
    val file = wd / dir / (name+".acrun")
    val line = (read.lines(file)).last
    unpickle(line)
  }

  def loadAllFinal(name: String, dir : String ="acDev") = {
    val fileNames = ls(wd / dir) filter (_.ext == "acstate") map (_.name.dropRight(8))
    val states = (for (name <- fileNames) yield (name, loadFinal(name, dir))).toMap
    ACStateData(states, dir)
  }

  def loadState(file : ammonite.ops.Path) = {
    unpickle(read(file))
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



  def resetFile(file : ammonite.ops.Path) = {
    val lastline = read.lines(file).last
    write.over(file, s"$lastline\n")
  }

  def lastLine(source: ammonite.ops.Path, target: ammonite.ops.Path) = {
    val lastline = read.lines(source).last
    write.over(target, s"$lastline\n")
  }

  def succFile(source: ammonite.ops.Path) = {
    val target = source / up / s"succ-${source.name}"
    lastLine(source, target)
  }
}
