package provingground.andrewscurtis

import ammonite.ops._

import provingground._

import Collections._

import upickle.default.{write => uwrite, read => uread, _}

import LinearStructure._

import FiniteDistribution._

import SimpleAcEvolution._

import ACrunner._

case class ACData(
    paths: Map[String, Vector[(FiniteDistribution[AtomicMove], FiniteDistribution[Moves])]]){

  def names = (paths map (_._1)).toList

  def sizes = for ((name, data) <- paths) yield (name -> data.size)

  def states = for ((name, data) <- paths) yield (name -> data.last)

  def combined = vBigSum(states.values.toList)

  def blended = combined |*| (1.0/ paths.size)

  def proofs = blended._2

  def moveWeights = blended._1

  def thms(rank: Int = 2) = toPresentation(rank, proofs)

  def revive(name : String, p : ACrunner.Param = Param()) = {
    import p._
    import SimpleAcEvolution._
    val state = states(name)
    rawSpawn(name, rank, size, wrdCntn, state, ACData.fileSave(name, dir, alert))
  }

  def reviveAll(p : ACrunner.Param = Param()) = {
    for (name <- names) revive(name, p)
  }

  def spawn(name : String, p : ACrunner.Param = Param()) = {
    import p._
    import SimpleAcEvolution._
    rawSpawn(name, rank, size, wrdCntn, blended, ACData.fileSave(name, dir, alert))
  }

  def spawns(name: String, mult : Int = 4, p: Param = Param()) = {
    for (j <- 1 to mult) yield spawn(name+"."+j.toString, p)
  }
}

object ACData {


  def pickle(state: (FiniteDistribution[AtomicMove], FiniteDistribution[Moves])) = {
    val fdM = state._1
    val fdV = state._2
    val pmfM = for (Weighted(m, p) <- fdM.pmf) yield (PickledWeighted(uwrite(m), p))
    val pmfV = for (Weighted(v, p) <- fdV.pmf) yield (PickledWeighted(uwrite(v), p))
    val s = uwrite((pmfM, pmfV))
    println(unpickle(s)) // a test
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
  def fileSave(name: String, dir : String ="0.5", alert : Unit => Unit = (x) => ())
    (fdM : FiniteDistribution[AtomicMove], fdV : FiniteDistribution[Moves]) = {
      val file = wd / dir / (name+".acrun")
      write.append(file, s"${pickle(fdM, fdV)}\n")
      alert(())
  }

  def load(name: String, dir : String ="0.5") = {
    val file = wd / dir / (name+".acrun")
    val lines = read.lines(file)
    lines map (unpickle)
  }

  def loadAll(dir : String ="0.5") = {
    val fileNames = ls(wd / dir) filter (_.ext == "acrun") map (_.name.dropRight(6))
    (for (name <- fileNames) yield (name, load(name, dir))).toMap
  }

  def loadData(dir : String ="0.5") = ACData(loadAll(dir))

  def saveFD[T](file: String, dir: String = "0.5-output", fd: FiniteDistribution[T])={
    for (Weighted(x, p) <- fd.pmf) write.append(wd / file, s"$x, $p\n")
  }

  def saveEntropy(file: String, dir: String = "0.5-output", ent: List[Weighted[String]]) = {
    for (Weighted(x, p) <- ent) write.append(wd / file, s"$x, $p\n")
  }
}
