package provingground

import scala.language.higherKinds

import Functor.Id

trait OptNat[F[_]] {
  def optF[A](fo: F[Option[A]]): Option[F[A]]
}

object OptNat {
  def mkOpt[A, F[_]: OptNat](fo: F[Option[A]]) =
    implicitly[OptNat[F]].optF(fo)

  implicit object ListON extends OptNat[List] {
    def optF[A](fo: List[Option[A]]) =
      if (fo contains None) None else Some(fo.flatten)
  }

  implicit object IdON extends OptNat[Id] {
    def optF[A](fo: Option[A]) = fo
  }

  implicit object OptionON extends OptNat[Option] {
    def optF[A](fo: Option[Option[A]]) = fo
  }

  // class ComposeON[X[_]: OptNat: Functor, Y[_]: OptNat] {
  //   type Z[A] = X[Y[A]]
  //
  //   def OpN: OptNat[Z] = new OptNat[Z] {
  //     def optF[A](fo: Z[Option[A]]) = {
  //       val inner = implicitly[OptNat[Y]].optF[A] _
  //       val first = (implicitly[Functor[X]].map(_: X[Y[Option[A]]])(inner))(fo)
  //       implicitly[OptNat[X]].optF(first)
  //     }
  //   }
  // }

  implicit def composeON[X[_]: OptNat: Functor, Y[_]: OptNat] =
    new OptNat[({type Z[A] = X[Y[A]]})#Z]{
      def optF[A](fo: X[Y[Option[A]]]) = {
        val inner = implicitly[OptNat[Y]].optF[A] _
        val first = (implicitly[Functor[X]].map(_: X[Y[Option[A]]])(inner))(fo)
        implicitly[OptNat[X]].optF(first)
      }

    }

  class ConstON[C] {
    type Z[A] = C

    def OpN: OptNat[Z] = new OptNat[Z] {
      def optF[A](fo: C) : Option[C] = Some(fo)
    }
  }

  implicit def constantON[C] =
    new OptNat[({type Z[A] = C})#Z]{
      def optF[A](fo: C) : Option[C] = Some(fo)
    }
    // new ConstON[C].OpN


  // class T2[X1[_]: OptNat, X2[_]: OptNat] {
  //   type Z[A] = (X1[A], X2[A])
  //
  //   def OpN: OptNat[Z] = new OptNat[Z] {
  //     def optF[A](fo: Z[Option[A]]) =
  //       for (x1 <- implicitly[OptNat[X1]].optF(fo._1);
  //            x2 <- implicitly[OptNat[X2]].optF(fo._2)) yield (x1, x2)
  //   }
  // }

  implicit def t2ON[X1[_]: OptNat, X2[_]: OptNat] =
    new OptNat[({type Z[A] = (X1[A], X2[A])})#Z]{
      def optF[A](fo: (X1[Option[A]], X2[Option[A]])) =
        for (x1 <- implicitly[OptNat[X1]].optF(fo._1);
             x2 <- implicitly[OptNat[X2]].optF(fo._2)) yield (x1, x2)
    }
    // new T2[X1, X2].OpN

  // class T3[X1[_]: OptNat, X2[_]: OptNat, X3[_]: OptNat] {
  //   type Z[A] = (X1[A], X2[A], X3[A])
  //
  //   def OpN: OptNat[Z] = new OptNat[Z] {
  //     def optF[A](fo: Z[Option[A]]) =
  //       for (x1 <- implicitly[OptNat[X1]].optF(fo._1);
  //            x2 <- implicitly[OptNat[X2]].optF(fo._2);
  //            x3 <- implicitly[OptNat[X3]].optF(fo._3)) yield (x1, x2, x3)
  //   }
  // }

  implicit def t3ON[X1[_]: OptNat, X2[_]: OptNat, X3[_]: OptNat] =
    new OptNat[({type Z[A] = (X1[A], X2[A], X3[A])})#Z]{
      def optF[A](fo: (X1[Option[A]], X2[Option[A]], X3[Option[A]])) =
        for (x1 <- implicitly[OptNat[X1]].optF(fo._1);
             x2 <- implicitly[OptNat[X2]].optF(fo._2);
             x3 <- implicitly[OptNat[X3]].optF(fo._3)) yield (x1, x2, x3)

    }
    // new T3[X1, X2, X3].OpN

  // class T4[X1[_]: OptNat, X2[_]: OptNat, X3[_]: OptNat, X4[_]: OptNat] {
  //   type Z[A] = (X1[A], X2[A], X3[A], X4[A])
  //
  //   def OpN: OptNat[Z] = new OptNat[Z] {
  //     def optF[A](fo: Z[Option[A]]) =
  //       for (x1 <- implicitly[OptNat[X1]].optF(fo._1);
  //            x2 <- implicitly[OptNat[X2]].optF(fo._2);
  //            x3 <- implicitly[OptNat[X3]].optF(fo._3);
  //            x4 <- implicitly[OptNat[X4]].optF(fo._4)) yield (x1, x2, x3, x4)
  //   }
  // }

  implicit def t4ON[X1[_]: OptNat, X2[_]: OptNat, X3[_]: OptNat, X4[_]: OptNat] =
    new OptNat[({type Z[A] = (X1[A], X2[A], X3[A], X4[A])})#Z]{
      def optF[A](fo: (X1[Option[A]], X2[Option[A]], X3[Option[A]], X4[Option[A]])) =
        for (x1 <- implicitly[OptNat[X1]].optF(fo._1);
             x2 <- implicitly[OptNat[X2]].optF(fo._2);
             x3 <- implicitly[OptNat[X3]].optF(fo._3);
             x4 <- implicitly[OptNat[X4]].optF(fo._4)) yield (x1, x2, x3, x4)
    }

    // new T4[X1, X2, X3, X4].OpN

  // class T5[X1[_]: OptNat,
  //          X2[_]: OptNat,
  //          X3[_]: OptNat,
  //          X4[_]: OptNat,
  //          X5[_]: OptNat] {
  //   type Z[A] = (X1[A], X2[A], X3[A], X4[A], X5[A])
  //
  //   def OpN: OptNat[Z] = new OptNat[Z] {
  //     def optF[A](fo: Z[Option[A]]) =
  //       for (x1 <- implicitly[OptNat[X1]].optF(fo._1);
  //            x2 <- implicitly[OptNat[X2]].optF(fo._2);
  //            x3 <- implicitly[OptNat[X3]].optF(fo._3);
  //            x4 <- implicitly[OptNat[X4]].optF(fo._4);
  //            x5 <- implicitly[OptNat[X5]].optF(fo._5)) yield
  //         (x1, x2, x3, x4, x5)
  //   }
  // }

  implicit def t5ON[X1[_]: OptNat,
                    X2[_]: OptNat,
                    X3[_]: OptNat,
                    X4[_]: OptNat,
                    X5[_]: OptNat] =
            new OptNat[({type Z[A] = (X1[A], X2[A], X3[A], X4[A], X5[A])})#Z]{
                  def optF[A](fo: (X1[Option[A]], X2[Option[A]], X3[Option[A]], X4[Option[A]], X5[Option[A]])) =
                    for (x1 <- implicitly[OptNat[X1]].optF(fo._1);
                         x2 <- implicitly[OptNat[X2]].optF(fo._2);
                         x3 <- implicitly[OptNat[X3]].optF(fo._3);
                         x4 <- implicitly[OptNat[X4]].optF(fo._4);
                         x5 <- implicitly[OptNat[X5]].optF(fo._5)) yield
                      (x1, x2, x3, x4, x5)
                      }

    // new T5[X1, X2, X3, X4, X5].OpN

    import Functor.Named

    implicit object NamedOptNat extends OptNat[Functor.Named]{
      def optF[A](fo: Named[Option[A]]): Option[Named[A]] =
        fo._2 map ((fo._1, _))
    }


  implicit val co = t3ON[Functor.S, Functor.Id, List]

  object Tests {
    val ii = implicitly[OptNat[Functor.II]]

    val li = implicitly[OptNat[Functor.IL]]

    val ll = implicitly[OptNat[Functor.LL]]

    val coo = implicitly[OptNat[Functor.Coded]]
  }
}
