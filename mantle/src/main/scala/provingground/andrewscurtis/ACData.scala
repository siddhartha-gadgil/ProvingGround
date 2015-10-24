package provingground.andrewscurtis

import ammonite.ops._

import provingground._

import Collections._

import upickle.default.{write => uwrite, read => uread, _}

import LinearStructure._

import FiniteDistribution._

import SimpleAcEvolution._

case class ACData(
    paths: Map[String, Vector[(FiniteDistribution[AtomicMove], FiniteDistribution[Moves])]]){

  def names = (paths map (_._1)).toList

  def sizes = for ((name, data) <- paths) yield (name -> data.size)

  def states = for ((name, data) <- paths) yield (name -> data.last)

  def combined = vBigSum(states.values.toList)

  def blended = combined |*| (1.0/ paths.size)
}

object ACData {

  
  def pickle(state: (FiniteDistribution[AtomicMove], FiniteDistribution[Moves])) = {
    val fdM = state._1
    val fdV = state._2
    val pmfM = (for (Weighted(m, p) <- fdM.pmf) yield PickledWeighted(m.toPlainString, p))
    val pmfV = (for (Weighted(v, p) <- fdV.pmf) yield (PickledWeighted(uwrite(v.moves.map(_.toString)), p)))
    uwrite((pmfM, pmfV))
  }

  def unpickle(str: String) = {
    val fdStrings = uread[(Vector[PickledWeighted], Vector[PickledWeighted])](str)
    val pmfM = fdStrings._1 map (
        (w : PickledWeighted) => w map ((x) => AtomicMove.fromString(x).get))
    val pmfV = fdStrings._2 map (
        (w : PickledWeighted) => w map ((x) => Moves.fromString(uread[Vector[String]](x)).get))
    (FiniteDistribution(pmfM), FiniteDistribution(pmfV))
  }

  val wd = cwd / "data"
  def save(name: String, dir : String ="0.5")
    (fdM : FiniteDistribution[AtomicMove], fdV : FiniteDistribution[Moves]) = {
      val file = wd / dir / (name+".acrun")
      write.append(file, uwrite(pickle(fdM, fdV)))
      write.append(file, "/n")
  }

  def load(name: String, dir : String ="0.5") = {
    val file = wd / dir / (name+".acrun")
    val lines = read.lines(file)
    lines map (unpickle)
  }

  def loadAll(dir : String ="0.5") = {
    val fileNames = ls(wd / dir) filter (_.ext == ".acrun") map (_.name.dropRight(6))
    (for (name <- fileNames) yield (name, load(name, dir))).toMap
  }

  def loadData(dir : String ="0.5") = ACData(loadAll(dir))
}
