package deepwalk4s

import org.deeplearning4j.graph
import org.deeplearning4j.graph.models.embeddings.GraphVectorsImpl
import org.deeplearning4j.graph.models.deepwalk._
import scala.jdk.CollectionConverters._
import org.deeplearning4j.graph.iterator.parallel.WeightedRandomWalkGraphIteratorProvider

object Graph {
  case class IndexedVertex[A](index: Int, label: A)
      extends graph.api.Vertex(index, label)

  case class Edge[A](initial: A,
                     terminal: A,
                     weight: Double = 1.0,
                     oriented: Boolean = false)

  case class IndexEdge(init: Int,
                       term: Int,
                       weight: java.lang.Double,
                       oriented: Boolean)
      extends graph.api.Edge(init, term, weight, oriented)

  def fromMap[V](map: Map[V, List[V]],
                 weight: Double = 1.0,
                 oriented: Boolean = false) = {
    val vertices = map.keys.toList ++ map.values.toList.flatten
    val edges    = for ((x, l) <- map; y <- l) yield Edge(x, y, weight, oriented)
    Graph(vertices.toSet, edges.toList)
  }

  def fromSeqs[V](ss: Seq[Seq[V]],
                  weight: Double = 1.0,
                  oriented: Boolean = false) = {
    val map = (ss map ((row) => (row.head, row.tail.toList))).toMap
    fromMap(map, weight, oriented)
  }

  def fromLines(lines: Seq[String],
                weight: Double = 1.0,
                oriented: Boolean = false,
                separator: String = "\t") = {
    val ss = lines map (_.split(separator).toList)
    fromSeqs(ss, weight, oriented)
  }
}

import Graph._

case class Graph[V](vertices: Set[V], edges: List[Edge[V]]) {
  val index = vertices.zipWithIndex.toMap

  val vert = index map { case (x, n) => (n, x) }

  val vertex = (for ((n, v) <- vert)
    yield ((v, IndexedVertex(n, v): graph.api.Vertex[V]))).toMap

  val jVertices = vertex.values.toList.asJava

  val jEdges =
    edges map
      ((e) =>
        IndexEdge(index(e.initial), index(e.terminal), e.weight, e.oriented))

  val jGraph: graph.graph.Graph[V, java.lang.Double] = {
    val base = new graph.graph.Graph[V, java.lang.Double](jVertices)
    jEdges.foreach(base.addEdge(_))
    base
  }

  def graphVectors(learningRate: Double = 0.01,
                   vectorSize: Int = 100,
                   windowSize: Int = 5,
                   walkLength: Int = 20): DeepWalk[V, java.lang.Double] = {
    val base = new DeepWalk.Builder[V, java.lang.Double]()
    val builder = base
      .learningRate(learningRate)
      .vectorSize(vectorSize)
      .windowSize(windowSize)
    val deepLearn = builder.build()
    val provider = new WeightedRandomWalkGraphIteratorProvider(
      jGraph,
      walkLength,
      12345,
      org.deeplearning4j.graph.api.NoEdgeHandling.SELF_LOOP_ON_DISCONNECTED)
    deepLearn.initialize(jGraph)
    deepLearn.fit(provider)
    deepLearn
  }

  def getVector(gv: GraphVectorsImpl[V, java.lang.Double])(
      v: V): Vector[Double] = {
    gv.getVertexVector(index(v)).data.asDouble().toVector
  }

  def vectorRep(learningRate: Double = 0.01,
                vectorSize: Int = 100,
                windowSize: Int = 5,
                walkLength: Int = 20) =
    getVector(graphVectors(learningRate, vectorSize, windowSize, walkLength)) _
}
