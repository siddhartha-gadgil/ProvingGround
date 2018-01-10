import functionfinder._
import spire.implicits._
import NatRing.{Literal => nat, power => _, _}, FreeGroup.{Literal => elem, _}

lazy val thm = n :-> ((power(g |+| h |+| g.inverse)(n)) =:= (g |+| power(h)(n)  |+| g.inverse ))
lazy val n = "n" :: NatTyp

import provingground.andrewscurtis.FreeGroups._

lazy val hyp = "hyp" :: thm(n)
lazy val c= g |+| h |+| g.inverse
lazy val base = elem(Word.e).refl
lazy val step = n :~> (hyp :-> (FreeGroup.rm(c) *: hyp ))
lazy val pf = Induc(thm, base, step)
assert(pf.typ == n ~>: (thm(n)))
