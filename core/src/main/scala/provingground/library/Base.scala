package provingground

import HoTT._

object Base {
  val A = "A" :: Type
  val a = "a" :: A

  val B = "B" :: Type
  val b = "b" :: B
  val f = "f" :: (A ->: B)

  val g  = "g" :: (A ->: B)
  val hA = "h_A" :: (A ->: A)
  val hB = "h_B" :: (B ->: B)

  val id = lambda(A)(lambda(a)(a))

  val Bs = "B(_ : A)" :: (A ->: Type)

  val fdep = "f" :: PiDefn(Bs)

  val gdep = "g" :: PiDefn(Bs)

  val AimplB = "_:A->B" :: (A ->: B)

  val mp = lambda(A)(
    lambda(B)(
      lambda(a !: A)(
        lambda(AimplB !: (A ->: B))(
          AimplB(a) !: B
        )
      )
    )
  )

  val fmly = (a !: A) ~>: (Bs(a) ->: A)

  val switch = lambda(pair(a, b))(
    pair(b, a)
  )
}
