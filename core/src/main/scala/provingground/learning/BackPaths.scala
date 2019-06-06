package provingground.learning
import scala.util._

class BackPaths[A](val generators: Map[A, Set[Set[A]]]){
    val rnd = new Random()

    val initialValues = generators.keys.toVector

    def randomPathsFromInit(init: A, length: Int) : Set[Vector[A]] = 
        if (length == 0) Set(Vector())
        else {
            val branches : Vector[Set[A]] = generators.getOrElse(init, Set.empty[Set[A]]).toVector
            if (branches.isEmpty) Set(Vector(init))
            else {
                val gens = branches(rnd.nextInt(branches.size))
                gens.flatMap{
                    a => randomPathsFromInit(a, length -1).map{v => init +: v}
                }
            }
        }

    def randomPaths(initNumber: Int, length: Int) : Vector[Vector[A]] = 
        {
            val inits : Vector[A] = (1 to initNumber).toVector.map(_ => initialValues(rnd.nextInt(initialValues.size)))
            inits.flatMap(a => randomPathsFromInit(a, length).toVector)
        }
}