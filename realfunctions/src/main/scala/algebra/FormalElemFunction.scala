package algebra

import PointWise._


/**
 * @author gadgil
 */
sealed trait FormalElemFunction {
  def as[A : FieldOps : ElementaryFunctions : Circ]: A
}

case object Sin extends FormalElemFunction{
  def as[A : FieldOps : ElementaryFunctions : Circ] = {
    implicitly[ElementaryFunctions[A]].sin
  }
}

case object Cos extends FormalElemFunction{
  def as[A : FieldOps : ElementaryFunctions: Circ] = {
    implicitly[ElementaryFunctions[A]].cos
  }
}

case object Log extends FormalElemFunction{
  def as[A : FieldOps : ElementaryFunctions : Circ] = {
    implicitly[ElementaryFunctions[A]].log
  }
}

case class Proj(i: Int) extends FormalElemFunction{
  def as[A : FieldOps : ElementaryFunctions : Circ] = {
    implicitly[ElementaryFunctions[A]].proj(i)
  }
}


case object Exp extends FormalElemFunction{
  def as[A : FieldOps : ElementaryFunctions : Circ] = {
    implicitly[ElementaryFunctions[A]].exp
  }
}

case object One extends FormalElemFunction{
  def as[A : FieldOps : ElementaryFunctions : Circ] = {
    implicitly[FieldOps[A]].one
  }
}

case object Zero extends FormalElemFunction{
  def as[A : FieldOps : ElementaryFunctions : Circ] = {
    implicitly[FieldOps[A]].one
  }
}

case class Plus(x: FormalElemFunction, y: FormalElemFunction) extends FormalElemFunction {
  def as[A : FieldOps : ElementaryFunctions : Circ] = {
    implicitly[FieldOps[A]].plus(x.as[A], y.as[A])
  }
}

case class Times(x: FormalElemFunction, y: FormalElemFunction) extends FormalElemFunction{
  def as[A : FieldOps : ElementaryFunctions : Circ] = {
    implicitly[FieldOps[A]].times(x.as[A], y.as[A])
  }
}

case class Div(x: FormalElemFunction, y: FormalElemFunction) extends FormalElemFunction{
  def as[A : FieldOps : ElementaryFunctions : Circ] = {
    implicitly[FieldOps[A]].div(x.as[A], y.as[A])
  }
}

case class Negate(x: FormalElemFunction) extends FormalElemFunction{
  def as[A : FieldOps : ElementaryFunctions : Circ] = {
    implicitly[FieldOps[A]].negate(x.as[A])
  }
}

case class Compose(x: FormalElemFunction, y: FormalElemFunction) extends FormalElemFunction{
  def as[A : FieldOps : ElementaryFunctions : Circ] = {
    implicitly[Circ[A]].circ(x.as[A], y.as[A])
  }
}


object FormalElemFunctions{
  implicit val FormalElemFunc = new ElementaryFunctions[FormalElemFunction]{
    val sin : FormalElemFunction = Sin
    val cos : FormalElemFunction = Cos
    val exp : FormalElemFunction = Exp
    val log : FormalElemFunction = Log
    
    val proj : Int =>  FormalElemFunction = (i: Int) => Proj(i)
  }
  
  implicit val formalFieldOps = new FieldOps[FormalElemFunction]{
    def negate(x: FormalElemFunction): FormalElemFunction = Negate(x)
    
    def zero : FormalElemFunction = Zero
    
    def one : FormalElemFunction = One
    
    def plus(x: FormalElemFunction, y: FormalElemFunction): FormalElemFunction = Plus(x, y)
    
    def times(x: FormalElemFunction, y: FormalElemFunction): FormalElemFunction = Times(x, y)
    
    def div(x: FormalElemFunction, y: FormalElemFunction) : FormalElemFunction = Div(x, y)
  }
  
  implicit val formalCompose = new Circ[FormalElemFunction]{
    def circ(x: FormalElemFunction, y: FormalElemFunction): FormalElemFunction = Compose(x, y)
  }
  
  import FieldOpsSyms._
  
  import FormalElemFunc._
  
  import Circ._

  val tan = sin/cos
  
  val x= proj(0)
  
  val y = proj(1)
  
  val z = proj(2)
}