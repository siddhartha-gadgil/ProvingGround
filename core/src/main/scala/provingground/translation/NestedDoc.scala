package provingground.translation

trait NestedDoc[T] {
  def append(t: T): NestedDoc[T]

  def hasLabel: Boolean

  def sameLabel[X](label: X): Boolean

  def beginSimple = NestedDoc.AppendOpenBlock(this, NestedDoc.SimpleEmpty[T])

  def begin[L](label: L) =
    NestedDoc.AppendOpenBlock(this, NestedDoc.LabelledEmpty(label))

  def endSimple: NestedDoc[T]

  def end[L](label: L): NestedDoc[T]

  /**
    * export to one level up only;
    * determined by whether the current level is labelled.
    */
  def export: Scoped[T] => Scoped[T]

  /**
    * scoped statements, recursively defined as seen from just outside the block.
    */
  def read: Vector[Scoped[T]]

  def contexts: Map[Scoped[T], Vector[Scoped[T]]]
}

object NestedDoc {
  trait ClosedDoc[T] extends NestedDoc[T] {
    def append(t: T) = Append(this, t)

    def endSimple =
      throw new IllegalArgumentException(s"trying to close closed block $this")

    def end[L](label: L) =
      throw new IllegalArgumentException(s"trying to close closed block $this")
  }

  trait Empty[T] extends ClosedDoc[T] {
    def read = Vector()

    def contexts = Map()
  }

  case class LabelledEmpty[L, T](label: L) extends Empty[T] {
    def sameLabel[X](l: X) = label == l

    def hasLabel = true

    def export = Scoped.Cons(label, _)
  }

  case class SimpleEmpty[T]() extends Empty[T] {
    def sameLabel[X](label: X) = false

    def hasLabel = false

    def export = (t) => t
  }

  trait RecDoc[T] {
    def init: NestedDoc[T]

    def sameLabel[X](label: X) = init.sameLabel(label)

    def hasLabel = init.hasLabel

    def export = init.export
  }

  trait RecBlockDoc[T] extends RecDoc[T] {
    def last: NestedDoc[T]

    def read = init.read ++ (last.read map (export))

    def contexts =
      init.contexts ++
        (last.contexts map {
          case (x, l) =>
            (export(x), (l map export) ++ init.read)
        })
  }

  case class Append[T](init: NestedDoc[T], last: T)
      extends ClosedDoc[T]
      with RecDoc[T] {
    def read = init.read.+:(export(Scoped.Outer(last)))

    def contexts = init.contexts.+(export(Scoped.Outer(last)) -> init.read)
  }

  case class AppendBlock[T](init: NestedDoc[T], last: NestedDoc[T])
      extends ClosedDoc[T]
      with RecBlockDoc[T]

  case class AppendOpenBlock[T](init: NestedDoc[T], last: NestedDoc[T])
      extends NestedDoc[T]
      with RecBlockDoc[T] {
    def append(t: T) = AppendOpenBlock(init, last.append(t))

    def close = AppendBlock(init, last)

    def endSimple = {
      assert(!hasLabel, s"applying unlabelled end on $this which has label")
      close
    }

    def end[L](label: L) = {
      assert(hasLabel, s"applying labelled end on $this which has no label")
      assert(sameLabel(label),
             s"applying end on $this with incorrect label $label")
      close
    }
  }
}
