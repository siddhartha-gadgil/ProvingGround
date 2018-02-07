package provingground

object Frankl{
  case class SetSystem(sets: Set[Set[Int]]){
    val size = sets.size

    lazy val suppSet = sets.foldLeft[Set[Int]](Set())(_ union _)

    lazy val suppMaxOpt : Option[Int] =
      if (suppSet.isEmpty) None else Some(suppSet.max)

    def freq(k: Int) = sets.filter(_.contains(k)).size

    lazy val frequencies =
      suppMaxOpt.map{
        (suppMax) =>
      (1 to suppMax).toVector.map((k) => (k, freq(k))).sortBy{case (k, n) => -n}
    }.getOrElse(Vector())

    lazy val normalize = {
      val perm : Map[Int, Int] =
        {
          frequencies.map(_._1).zipWithIndex.map{case (a, b) => (b + 1, a)}
        }.toMap
      val permSets: Set[Set[Int]] = sets.map((s) => s.map(perm))
      SetSystem(permSets)
    }

    def union(that: SetSystem) = SetSystem(sets union (that.sets))

    def join(that: SetSystem) =
      SetSystem(
        for {s <- sets; t <- that.sets} yield s union t
      )

    def addElem(n: Int) = SetSystem(sets.map (_ + n))

    lazy val isUnionClosed =
      {
        val notClosedBools =
          for {
            s <- sets
            t <- sets
          } yield (sets.contains(s union t))
        notClosedBools.foldLeft(true)(_ && _)
      }


    def buildWith(that: SetSystem, n: Int): SetSystem = {
      val b = that.addElem(n)
      union(b).union(join(b))

    }
  }

  object SetSystem{
    def apply(sets: Set[Int]*): SetSystem = SetSystem(sets.toSet)


    val trivial = SetSystem(Set(Set[Int]()))

    def buildWithAll(coll1: Set[SetSystem], coll2: Set[SetSystem], n: Int) : Set[SetSystem] =
      for {
        sys1 <- coll1
        sys2 <- coll2
      } yield sys1.buildWith(sys2, n)

    lazy val allSystems: Stream[Set[SetSystem]] =
      Stream.from(0).map{
        (n) =>
          if (n == 0) Set[SetSystem](trivial)
          else buildWithAll(allSystems.take(n).toSet.flatten, normalSystems.take(n).toSet.flatten, n)
      }

    lazy val normalSystems =
      allSystems.map{
        (s) =>
          s.map(_.normalize)
      }
    }
}
