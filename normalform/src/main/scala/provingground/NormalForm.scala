package provingground.normalform

import scala.language.implicitConversions
import TopLevel._

object TopLevel {

  def listToMap[B](list: List[B]): Map[B, Int] = {
    def countInstances(x: B): Int = {
      (list filter (_ == x)).length
    }
    (list map ((x: B) => (x, countInstances(x)))).toMap
  }

  implicit def convertToRep[A](obj: A): Representation[A] = {
    CommRep(Map((obj, 1)))
  }

  implicit def toNumber(num: java.lang.Number): Number = {
    Number(num)
  }

  implicit def toSigma(term: Term): Sigma = {
    term match {
      case Sigma(_) => term
      case x: Term => Sigma(CommRep(Map((x, 1))))
    }
  }

  implicit def toPi(term: Term): Pi = {
    term match {
      case Pi(_) => term
      case x: Term => Pi(CommRep(Map((x, 1))))
    }
  }

  type CommTerm = CommRep[Term]

  def CommTerm(representation: Map[Term, Int]): CommTerm = {
    CommRep[Term](representation)
  }

  type AssocTerm = AssocRep[Term]

  def AssocTerm(representation: List[Term]): AssocTerm = {
    AssocRep[Term](representation)
  }

  /*
  def convertToSigma[A](oppair: OpPair[A], obj: A): Sigma[A] = {
    Sigma(oppair, obj)
  }

  def convertToPi[A](oppair: OpPair[A], obj: A): Pi[A] = {
    Pi(oppair, obj)
  }*/
}

trait Representation[A] {
  def map[B](f: A=>B): Representation[B]
  def filter(pred: A=>Boolean): Representation[A]
  def reduce(op: (A, A)=>A): A
  def combine(that: Representation[A]): Representation[A]
  implicit def toCommRep(): CommRep[A]
  implicit def toAssocRep(): AssocRep[A]
}

/* 
 * CommRep represents operators which are both commutative and associative.
 * AssocRep represents operators which are only associative
 */

case class CommRep[A](representation: Map[A, Int]) extends Representation[A] {
  val listRep = toList(representation)

/* toList and listToMap change to and from the internal Map
 * representation to a list form like that of AssocRep.
 * This makes implementing map and reduce much simpler
 */

  def toList(rep: Map[A, Int]): List[A] = {
    def createInstances(x: (A, Int)): List[A] = {
      (0 until x._2).toList map (_ => x._1)
    }
    rep.toList flatMap createInstances
  }
  
  override def map[B](f: A=>B): CommRep[B] = {
    CommRep(listToMap(listRep map f))
  }

  override def filter(pred: A=>Boolean): Representation[A] = {
    CommRep(listToMap(listRep filter pred))
  }

  override def reduce(op: (A, A)=>A): A = {
    listRep reduce op
  }

  override def combine(that: Representation[A]): Representation[A] = {
    that match {
      case CommRep(x) => CommRep(listToMap(listRep:::toList(x)))
      case AssocRep(x) => AssocRep(listRep ::: x)
    }
  }

  override implicit def toCommRep(): CommRep[A] = {
    this
  }

  override implicit def toAssocRep(): AssocRep[A] = {
    AssocRep(listRep)
  }
}

case class AssocRep[A](representation: List[A]) extends Representation[A] {
  override def map[B](f: A=>B): AssocRep[B] = {
    AssocRep(representation map f)
  }

  override def filter(pred: A=>Boolean): Representation[A] = {
    AssocRep(representation filter pred)
  }

  override def reduce(op: (A, A)=>A): A = {
    representation reduce op
  }

  override def combine(that: Representation[A]): Representation[A] = {
    that match {
      case comm @ CommRep(x) => AssocRep(representation ::: comm.listRep)
      case AssocRep(x) => AssocRep(representation ::: x)
    }
  }

  override implicit def toCommRep(): CommRep[A] = {
    CommRep(listToMap(representation))
  }

  override implicit def toAssocRep(): AssocRep[A] = {
    this
  }
}


trait Term

trait BigOp extends Term

case class Number(num: java.lang.Number) extends Term {
  //def +(that: Number): Number = Number(num + that.num)
  //def *(that: Number): Number = Number(num * that.num)
}

case class Sigma(representation: Representation[Term]) extends BigOp {
  
  def +(that: Term): Term = {
    that match {
      case Sigma(thatRep) => Sigma(representation combine thatRep)
      case x: Term => this + toSigma(x)
    }
  }

  def *(that: Term): Term = {
    that match {
      case Sigma(thatRep) => Sigma(thatRep map ((x: Term) => this * x))
      case x: Term  => Sigma(representation map ((y: Term) => toPi(y) * x))
    }
  }
}

case class Pi(representation: Representation[Term]) extends BigOp {

  def +(that: Term): Term = {
    that match {
      case sigma @ Sigma(_) => toSigma(this) + sigma
      case x: Term => toSigma(this) + toSigma(that)
    }
  }

  def *(that: Term): Term = {
    that match {
      case Pi(thatRep) => Pi(representation combine thatRep)
      case Sigma(thatRep) => Sigma(thatRep map ((x: Term) => this * x))
      case x: Term => this * toPi(x)
    }
  }
}

