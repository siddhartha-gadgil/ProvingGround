package provingground.library
import provingground._
import HoTT._
import induction._
import implicits._
import shapeless._
import Fold._
object nat$succ_le_succ {
  lazy val value = lambda("'o_806067969" :: "nat" :: Type)(
    lambda("'p_1140173959" :: "nat" :: Type)(
      lmbda(
        "_" :: ("nat.less_than_or_equal" :: FuncTyp("nat" :: Type,
                                                    FuncTyp("nat" :: Type,
                                                            Prop)))(
          "'o_806067969" :: "nat" :: Type)("'p_1140173959" :: "nat" :: Type))(
        "_" :: ("nat.less_than_or_equal" :: FuncTyp("nat" :: Type,
                                                    FuncTyp("nat" :: Type,
                                                            Prop)))(
          ("nat.succ" :: FuncTyp("nat" :: Type, "nat" :: Type))(
            "'o_806067969" :: "nat" :: Type))(
          ("nat.succ" :: FuncTyp("nat" :: Type, "nat" :: Type))(
            "'p_1140173959" :: "nat" :: Type)))))
}
