package deepwalk4s

import org.deeplearning4j.graph
import org.deeplearning4j.graph.models.embeddings.GraphVectorsImpl
import org.deeplearning4j.graph.models.deepwalk._
import scala.collection.JavaConverters._
import org.deeplearning4j.graph.iterator.parallel.WeightedRandomWalkGraphIteratorProvider

object Graph{
  case class IndexedVertex[A](index: Int, label: A) extends graph.api.Vertex(index, label)
  
  case class Edge[A](initial: A, terminal : A, weight: Double = 1.0, oriented: Boolean = false)
  
  case class IndexEdge(init: Int, term: Int, weight: java.lang.Double, oriented: Boolean) extends graph.api.Edge(init, term, weight, oriented)
  
  case class Graph[V](vertices: List[V], edges: List[Edge[V]]){
    val index = vertices.zipWithIndex.toMap
    
    val vert = index map {case (x, n) => (n, x)}
    
    val jVertices = (for ((n, v) <- vert) yield (IndexedVertex(n, v) : graph.api.Vertex[V])).toList.asJava
    
    val jEdges = edges map ((e) => 
      IndexEdge(index(e.initial), index(e.terminal), e.weight, e.oriented)
      )
    
    val jGraph : graph.graph.Graph[V, java.lang.Double] = {
      val base = new graph.graph.Graph[V, java.lang.Double](jVertices)
      jEdges.foreach(base.addEdge(_))
      base
    }
    
   def graphVectors(learningRate: Double = 0.01,
       vectorSize: Int = 100,
       windowSize: Int = 5,
       walkLength: Int = 20): GraphVectorsImpl[V, java.lang.Double] = {
     val base = new DeepWalk.Builder[V, java.lang.Double]()
     val builder = base.learningRate(learningRate).vectorSize(vectorSize).windowSize(windowSize)
     val deepLearn = builder.build()
     val provider = new WeightedRandomWalkGraphIteratorProvider(jGraph, walkLength)
     deepLearn.initialize(jGraph)
     deepLearn.fit(provider)
     deepLearn
   }
  }
}
