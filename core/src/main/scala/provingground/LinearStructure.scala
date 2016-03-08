package provingground

case class LinearStructure[A](zero: A, sum: (A, A) => A, mult : (Double, A) => A){
  def diff(frm: A, remove: A) = sum(frm, mult(-1.0, remove))
}

object LinearStructure{

  def vzero[T](implicit ls: LinearStructure[T]) = ls.zero

  def vsum[T](implicit ls: LinearStructure[T]) = ls.sum

  def vprod[T](implicit ls: LinearStructure[T]) = ls.mult

  def vdiff[T](implicit ls: LinearStructure[T]) = ls.diff _

  def vBigSum[T](xs: Traversable[T])(implicit ls: LinearStructure[T]) = {
  (xs :\ ls.zero)(ls.sum)
  }

  def vAverage[T](xs: Traversable[T])(implicit ls: LinearStructure[T]) ={
  ls.mult(1.0/xs.size, vBigSum(xs))
  }

  def nrec[X](base: X, ind: Int => X => X)(implicit ls: LinearStructure[X]): Int => X = {
    case 0 => base
    case n if n <0 => ls.zero
    case n => nrec(base, ind)(ls)(n-1)
  }

  implicit class VectorOps[A : LinearStructure](a: A){
    val vs = implicitly[LinearStructure[A]]

    def |+|(b: A) = vs.sum(a, b)

    def |*|(c: Double) : A = vs.mult(c, a)
  }

  implicit val RealsAsLinearStructure = LinearStructure[Double](0, (_+_), (_*_))

  implicit def VectorPairs[A, B](implicit lsa: LinearStructure[A], lsb: LinearStructure[B]): LinearStructure[(A, B)] = {
    def sumpair(fst: (A, B), scnd: (A, B)) =(lsa.sum(fst._1, scnd._1), lsb.sum(fst._2, scnd._2))

    def multpair(sc: Double, vect: (A, B)) = (lsa.mult(sc, vect._1), lsb.mult(sc, vect._2))

    LinearStructure((lsa.zero, lsb.zero), sumpair, multpair)
  }



  implicit def FuncLinearStructure[A, B](implicit lsB : LinearStructure[B]) : LinearStructure[A => B] = {
    def sumfn(fst: A => B, scnd: A => B) = (a : A) => lsB.sum(fst(a), scnd(a))

    def multfn(sc: Double, vect : A => B) = (a: A) => lsB.mult(sc, vect(a))

    def zerofn = (a: A) => lsB.zero

    LinearStructure(zerofn, sumfn, multfn)
  }

}
