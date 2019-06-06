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
    minWordFrequency: Int = 5,
    batchSize: Int = 250
) {
  val elems: Vector[A] = data.flatten.distinct

  val elemMap
      : Map[Int, A] = elems.zipWithIndex.map { case (a, n) => (n, a) }.toMap

  val indexMap: Map[A, Int] = elems.zipWithIndex.toMap

  def seq(v: Vector[A]) = new Sequence(v.map(a => IntElem(indexMap(a))).asJava)

  lazy val sequenceIterator =
    new AbstractSequenceIterator.Builder(data.map(seq).asJava).build()

    lazy val vocabCache = new AbstractCache.Builder[IntElem].build()


  lazy val lookupTable = new InMemoryLookupTable.Builder[IntElem]
    .vectorLength(150)
    .useAdaGrad(false)
    .cache(vocabCache)
    .build()

  
  lazy val vectors =
    {val base = 
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

    lazy val vectorMap : Map[A, Array[Double]] = 
        elems.map{a => a -> vectors.getWordVector(indexMap(a).toString)}.toMap

}

object RepresentationLearner{
  def fromEquationNodes(eqs: Set[EquationNode], numPaths: Int, length: Int) : RepresentationLearner[GeneratorVariables.Variable[_]] = 
    {
      val m = EquationNode.backMap(eqs)
      val bp = new BackPaths(m)
      val paths = bp.randomPaths(numPaths, length)
      new RepresentationLearner(paths)
    }
}