import ammonite.ops._
import deepwalk4s._
val coF = pwd / 'data /"co-occurences.tsv"
val pairs = read.lines(coF) map (_.split("\t")) map {case Array(a, b, n) => (a, b, n.toInt)}
val edges = pairs map {case (a, b, n) => Graph.Edge(a, b, n)}
val verts = (pairs map (_._1)).toSet

val cg = Graph(verts, edges.toList)
val cgv = cg.graphVectors(vectorSize = 20)

val s = verts.head
val vecs = cg.vectorRep(vectorSize = 20)

val rep = verts map {(v) => (v, vecs(v))}
val repStr = rep map {case (s, v) => s"""$s\t${v.mkString("\t")}\n"""}
val repF = pwd / 'data / "vecreps.tsv"
repStr.foreach(write.append(repF, _))
