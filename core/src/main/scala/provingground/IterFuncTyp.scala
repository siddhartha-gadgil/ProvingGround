package provingground

import HoTT._

trait IterFuncTypTarget[F <: Term with Subs[F], H <: Term with Subs[H], C<: Term with Subs[C]]{
  type Target <: Term with Subs[Target]
}

object IterFuncTypTarget{
  case class Head[H <: Term with Subs[H], C <: Term with Subs[C]]() extends IterFuncTypTarget[H, H, C]{
    type Target = C
  }

  implicit def head[H <: Term with Subs[H], C <: Term with Subs[C]] : IterFuncTypTarget[H, H, C] =
    Head[H, C]
}


abstract class IterFuncTyp[F <: Term with Subs[F], H <: Term with Subs[H], C <: Term with Subs[C]](
  implicit val tt: IterFuncTypTarget[F, H, C]){
    def target(X: Typ[C]) : tt.Target
  }

object IterFuncTyp{
  import IterFuncTypTarget._

  case class Head[H <: Term with Subs[H], C <: Term with Subs[C]]() extends IterFuncTyp[H, H, C]{
    def target(X: Typ[C])  = ??? // scala fails to see inner type
  }
}
