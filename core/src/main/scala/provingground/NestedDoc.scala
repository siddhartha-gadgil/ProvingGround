package provingground

trait NestedDoc[T] {
  def append(t: T) : NestedDoc[T]
  
  def hasLabel: Boolean
    
  def sameLabel[X](label: X) : Boolean
  
  def beginSimple = NestedDoc.AppendOpenBlock(this, NestedDoc.SimpleEmpty[T])
  
  def begin[L](label: L) =
    NestedDoc.AppendOpenBlock(this, NestedDoc.LabelledEmpty(label))
  
  def endSimple : NestedDoc[T]
  
  def end[L](label: L) : NestedDoc[T]
}

object NestedDoc{
  trait ClosedDoc[T] extends NestedDoc[T]{
    def append(t: T) = Append(this, t)
    
    def endSimple = 
      throw new IllegalArgumentException(s"trying to close closed block $this")
    
    def end[L](label: L) =
      throw new IllegalArgumentException(s"trying to close closed block $this")
  }
  
  trait Empty[T] extends ClosedDoc[T]
  
  case class LabelledEmpty[L, T](label: L) extends Empty[T]{
    def sameLabel[X](l: X) = label == l 
    
    def hasLabel = true
  }
  
  case class SimpleEmpty[T]() extends Empty[T]{
    def sameLabel[X](label: X) = false
    
    def hasLabel = false
  }
  
  trait RecDoc[T]{
    def init: NestedDoc[T]
    
    def sameLabel[X](label: X) = init.sameLabel(label)
    
    def hasLabel = init.hasLabel
  }
  
  case class Append[T](init : NestedDoc[T], last: T) extends ClosedDoc[T] with RecDoc[T]
  
  case class AppendBlock[T](init : NestedDoc[T], last: NestedDoc[T]) extends ClosedDoc[T] with RecDoc[T]
  
  case class AppendOpenBlock[T](init : NestedDoc[T], last: NestedDoc[T]) extends NestedDoc[T] with RecDoc[T]{
    def append(t: T) = AppendOpenBlock(init, last.append(t))
    
    def close = AppendBlock(init, last)
    
    def endSimple = 
    {
      assert(!hasLabel, s"applying unlabelled end on $this which has label")
      close
    }
    
    def end[L](label: L) =
      {
      assert(hasLabel, s"applying labelled end on $this which has no label")
      assert(sameLabel(label), s"applying end on $this with incorrect label $label")
      close
      }
  }
}