package provingground.learning

import org.datavec.api.util.ClassPathResource
import org.deeplearning4j.models.embeddings.WeightLookupTable
import org.deeplearning4j.models.embeddings.inmemory.InMemoryLookupTable
import org.deeplearning4j.models.embeddings.learning.impl.elements.SkipGram
import org.deeplearning4j.models.embeddings.loader.VectorsConfiguration
import org.deeplearning4j.models.sequencevectors.SequenceVectors
import org.deeplearning4j.models.sequencevectors.iterators.AbstractSequenceIterator
import org.deeplearning4j.models.sequencevectors.sequence._
import scala.collection.JavaConverters._
import org.deeplearning4j.models.sequencevectors.iterators._
import org.deeplearning4j.models.word2vec.wordstore.inmemory.AbstractCache

case class IntElem(n: Int) extends SequenceElement() {
  def getLabel() = n.toString
  def toJSON     = s"""{"index" -> "$n"}"""
}

class RepresentationLearner[A](
    data: Vector[Vector[A]],
    vectorLength: Int = 150,
    minWordFrequency: Int = 5,
    batchSize: Int = 250
) {
  val elems: Vector[A] = data.flatten.distinct

  val elemMap
      : Map[Int, A] = elems.zipWithIndex.map { case (a, n) => (n, a) }.toMap

  val elemCount: Map[A, Int] = elems.groupBy(identity(_)).mapValues(_.size).toMap

  val indexMap: Map[A, Int] = elems.zipWithIndex.toMap

  def seq(v: Vector[A]) = new Sequence(v.map(a => IntElem(indexMap(a))).asJava)

  lazy val sequenceIterator =
    new AbstractSequenceIterator.Builder(data.map(seq).asJava).build()

  lazy val vocabCache = new AbstractCache.Builder[IntElem].build()

  lazy val lookupTable = new InMemoryLookupTable.Builder[IntElem]
    .vectorLength(vectorLength)
    .useAdaGrad(false)
    .cache(vocabCache)
    .build()

  lazy val vectors = {
    val base =
      new SequenceVectors.Builder[IntElem](new VectorsConfiguration())
        .minWordFrequency(minWordFrequency)
        .lookupTable(lookupTable)
        .iterate(sequenceIterator)
        .vocabCache(vocabCache)
        .batchSize(batchSize)
        .iterations(1)
        .epochs(1)
        .resetModel(true)
        .trainElementsRepresentation(true)
        .trainSequencesRepresentation(false)
        .elementsLearningAlgorithm(new SkipGram[IntElem]())
        .build()
    base.fit()
    base
  }

  lazy val vectorMap: Map[A, Vector[Double]] =
    elems.map { a =>
      a -> vectors.getWordVector(indexMap(a).toString).toVector
    }.toMap

}

object RepresentationLearner {
  def equationNodesBranchedSampled(
      eqs: Set[EquationNode],
      numPaths: Int,
      length: Int,
      vectorLength: Int = 150,
      minWordFrequency: Int = 5,
      batchSize: Int = 250
  ): RepresentationLearner[GeneratorVariables.Variable[_]] = {
    val m     = EquationNode.backMap(eqs)
    val bp    = new BackPaths(m)
    val paths = bp.randomPaths(numPaths, length)
    new RepresentationLearner(paths, vectorLength, minWordFrequency, batchSize)
  }

  import Expression.Coeff

  def equationNodesBackSampled(
      eqs: Set[EquationNode],
      numPaths: Int,
      length: Int,
      coeffWeights: Coeff[_] => Double = (_) => 1,
      vectorLength: Int = 150,
      minWordFrequency: Int = 5,
      batchSize: Int = 250
  ): RepresentationLearner[GeneratorVariables.Variable[_]] = {
    val m     = EquationNode.backCoeffMap(eqs)
    val bp    = WeightedBackPaths(m, coeffWeights)
    val paths = bp.unifRandomPaths(numPaths, length)
    new RepresentationLearner(paths, vectorLength, minWordFrequency, batchSize)
  }

  def equationNodesSampled(
      eqs: Set[EquationNode],
      numPaths: Int,
      length: Int,
      coeffWeights: Coeff[_] => Double = (_) => 1,
      backWeight: Double = 0.8,
      vectorLength: Int = 150,
      minWordFrequency: Int = 5,
      batchSize: Int = 250
  ): RepresentationLearner[GeneratorVariables.Variable[_]] = {
    val bm     = EquationNode.backCoeffMap(eqs)
    val fm = EquationNode.forwardCoeffMap(eqs)
    val bp    = WeightedBiPaths(bm, fm, coeffWeights, backWeight)
    val paths = bp.unifRandomPaths(numPaths, length)
    new RepresentationLearner(paths, vectorLength, minWordFrequency, batchSize)
  }

  import TypedPostResponse._

    def eqnsToRep(numPaths: Int,
      length: Int,
      coeffWeights: Coeff[_] => Double = (_) => 1,
      backWeight: Double = 0.8,
      vectorLength: Int = 150,
      minWordFrequency: Int = 5,
      batchSize: Int = 250
  ) : PostResponse[HoTTPost, HoTTPost.ID] = 
    MicroBot.simple[
      Set[EquationNode], 
      Map[GeneratorVariables.Variable[_], Vector[Double]],  
      HoTTPost, 
      HoTTPost.ID](
    (eqs: Set[EquationNode]) => 
      equationNodesSampled(eqs, numPaths, length, coeffWeights, backWeight, vectorLength, minWordFrequency, batchSize).vectorMap
  )
}
