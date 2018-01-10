import functionfinder._
import spire.implicits._
import NatRing.{Literal => nat, power => _, _}, FreeGroup.{Literal => elem, _}

import provingground.andrewscurtis.FreeGroups._

lazy val n = "n" :: NatTyp
lazy val m = "m" :: NatTyp
lazy val thm = n :-> ((FreeGroup.power(g)(m) |+| FreeGroup.power(g)(n)) =:= FreeGroup.power(g)(NatRing.sum(m)(n)) )

lazy val hyp = "hyp" :: thm(n)

lazy val base = FreeGroup.power(g)(m).refl
lazy val step = n :~> (hyp :-> (FreeGroup.rm(g) *: hyp ))
lazy val pf = Induc(thm, base, step)
assert(pf.typ == n ~>: (thm(n)))
