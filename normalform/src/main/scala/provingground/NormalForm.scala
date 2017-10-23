package provingground.normalform

import scala.language.implicitConversions
import NormalForm._
import spire.math._
import spire.implicits._

object NormalForm {

  def listToMap[B](list: List[B]): Map[B, Int] = {
    def countInstances(x: B): Int = {
      (list filter (_ == x)).length
    }
    (list map ((x: B) => (x, countInstances(x)))).toMap
  }

  implicit def convertToRep[A](obj: A): Representation[A] = {
    CommRep(Map((obj, 1)))
  }

  implicit def toSigma(term: Term): Sigma = {
    term match {
      case Sigma(_) => term
      case x: Term  => Sigma(CommRep(Map((x, 1))))
    }
  }

  implicit def toPi(term: Term): Pi = {
    term match {
      case Pi(_)   => term
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

  def addOp(x: Term, y: Term): Term = {
    (x, y) match {
      case (NumTerm(a), NumTerm(b)) => NumTerm(a + b)
      case _                        => x
    }
  }

  def mulOp(x: Term, y: Term): Term = {
    (x, y) match {
      case (NumTerm(a), NumTerm(b)) => NumTerm(a * b)
      case _                        => x
    }
  }

  def numTermMatch(term: Term): Boolean = {
    term match {
      case NumTerm(_) => true
      case _          => false
    }
  }

  def clumpTerms[A](list: List[A], pred: A => Boolean): List[List[A]] = {
    @annotation.tailrec
    def intermediate(current: List[A], acc: List[List[A]]): List[List[A]] = {
      if (current.isEmpty) {
        acc
      } else if (acc.isEmpty) {
        intermediate(current.tail, List(List(current.head)))
      } else {
        val next = current.head
        if (pred(next) == pred(acc.head.head)) {
          intermediate(current.tail, (next :: acc.head) :: acc.tail)
        } else {
          intermediate(current.tail, List(next) :: acc)
        }
      }
    }
    intermediate(list.reverse, Nil)
  }

  def commReduce(op: (Term, Term) => Term,
                 rep: CommRep[Term]): CommRep[Term] = {
    val onlyNumbers = rep filter numTermMatch
    val theRest     = rep filter (numTermMatch(_) == false)
    if (onlyNumbers.isEmpty) {
      theRest
    } else {
      val finalNumber = onlyNumbers reduce op
      val out         = theRest combine CommRep[Term](Map((finalNumber, 1)))
      out match {
        case x: CommRep[Term] => x
        case _                => rep
      }
    }
  }

  def assocReduce(op: (Term, Term) => Term,
                  rep: AssocRep[Term]): AssocRep[Term] = {
    val grouped = clumpTerms(rep.representation, numTermMatch)
    def action(list: List[Term]): List[Term] = {
      if (numTermMatch(list.head)) {
        List(list reduce op)
      } else {
        list
      }
    }
    AssocRep[Term](grouped flatMap action)
  }

  def semiReduce(term: Term): Term = {
    term match {
      case Sigma(rep) =>
        rep match {
          case x: CommRep[Term] =>
            Sigma(commReduce(addOp, x map semiReduce)).fullReduce
          case x: AssocRep[Term] =>
            Sigma(assocReduce(addOp, x map semiReduce)).fullReduce
        }
      case Pi(rep) =>
        rep match {
          case x: CommRep[Term] =>
            Pi(commReduce(mulOp, x map semiReduce)).fullReduce
          case x: AssocRep[Term] =>
            Pi(assocReduce(mulOp, x map semiReduce)).fullReduce
        }
      case _ => term
    }
  }
}

sealed trait Representation[A] {
  def toString(): String
  def map[B](f: A => B): Representation[B]
  def filter(pred: A => Boolean): Representation[A]
  def reduce(op: (A, A) => A): A
  def combine(that: Representation[A]): Representation[A]
  def isEmpty(): Boolean
  def length(): Int
  def head(): A
  implicit def toCommRep(): CommRep[A]
  implicit def toAssocRep(): AssocRep[A]
}

/*
 * CommRep represents operators which are both commutative and associative.
 * AssocRep represents operators which are only associative
 */

case class CommRep[A](representation: Map[A, Int]) extends Representation[A] {

  /* toList and listToMap change to and from the internal Map
   * representation to a list form like that of AssocRep.
   * This makes implementing map and reduce much simpler
   */

  val listRep = toList(representation)

  override def toString(): String = {
    "Comm" + representation.toString.drop(3)
  }

  def toList(rep: Map[A, Int]): List[A] = {
    def createInstances(x: (A, Int)): List[A] = {
      (0 until x._2).toList map (_ => x._1)
    }
    rep.toList flatMap createInstances
  }

  override def map[B](f: A => B): CommRep[B] = {
    CommRep(listToMap(listRep map f))
  }

  override def filter(pred: A => Boolean): CommRep[A] = {
    CommRep(listToMap(listRep filter pred))
  }

  def reduce(op: (A, A) => A): A = {
    listRep reduce op
  }

  override def combine(that: Representation[A]): Representation[A] = {
    that match {
      case CommRep(x)  => CommRep(listToMap(listRep ::: toList(x)))
      case AssocRep(x) => AssocRep(listRep ::: x)
    }
  }

  override def isEmpty(): Boolean = representation.isEmpty

  override def length(): Int = listRep.length

  override def head(): A = listRep.head

  override implicit def toCommRep(): CommRep[A] = {
    this
  }

  override implicit def toAssocRep(): AssocRep[A] = {
    AssocRep(listRep)
  }
}

case class AssocRep[A](representation: List[A]) extends Representation[A] {
  override def toString(): String = {
    "Assoc" + representation.toString.drop(4)
  }

  override def map[B](f: A => B): AssocRep[B] = {
    AssocRep(representation map f)
  }

  override def filter(pred: A => Boolean): AssocRep[A] = {
    AssocRep(representation filter pred)
  }

  override def reduce(op: (A, A) => A): A = {
    representation reduce op
  }

  override def combine(that: Representation[A]): Representation[A] = {
    that match {
      case comm @ CommRep(x) => AssocRep(representation ::: comm.listRep)
      case AssocRep(x)       => AssocRep(representation ::: x)
    }
  }

  override def isEmpty(): Boolean = representation.isEmpty

  override def length(): Int = representation.length

  override def head(): A = representation.head

  override implicit def toCommRep(): CommRep[A] = {
    CommRep(listToMap(representation))
  }

  override implicit def toAssocRep(): AssocRep[A] = {
    this
  }
}

/**
  * A trait encompassing all terms.
  *
  * Terms can be numbers, variables or big operations like
  * sigma or pi.
  */
sealed trait Term {
  def toString(): String
  def fullReduce(): Term
  def +(that: Term): Term
  def *(that: Term): Term
}

trait BigOp extends Term

case class NumTerm(num: Number) extends Term {
  override def toString(): String = {
    num.toString
  }

  override def fullReduce(): NumTerm = this

  override def +(that: Term): Term = {
    val out = that match {
      case NumTerm(x) => NumTerm(num + x)
      case x: Term    => toSigma(this) + x
    }
    semiReduce(out)
  }

  override def *(that: Term): Term = {
    val out = that match {
      case NumTerm(x) => NumTerm(num * x)
      case x: Term    => toPi(this) * x
    }
    semiReduce(out)
  }
}

case class Variable(symbol: String) extends Term {
  override def toString()             = symbol
  override def fullReduce(): Variable = this
  override def +(that: Term): Term    = semiReduce(toSigma(this) + that)
  override def *(that: Term): Term    = semiReduce(toPi(this) * that)
}

case class Sigma(representation: Representation[Term]) extends BigOp {

  override def toString(): String = {
    "Σ(" + representation.toString() + ")"
  }

  def fullReduce(): Term = {
    if (representation.length == 1) {
      representation.head
    } else {
      this
    }
  }

  def +(that: Term): Term = {
    val out = that match {
      case Sigma(thatRep) => Sigma(representation combine thatRep)
      case x: Term        => this + toSigma(x)
    }
    semiReduce(out)
  }

  def *(that: Term): Term = {
    val out = that match {
      case Sigma(thatRep) =>
        Sigma(
          (thatRep map ((x: Term) => this * x)) reduce
            ((x: Term, y: Term) => x + y))
      case x: Term => Sigma(representation map ((y: Term) => toPi(y) * x))
    }
    semiReduce(out)
  }
}

case class Pi(representation: Representation[Term]) extends BigOp {

  override def toString(): String = {
    "Π(" + representation.toString() + ")"
  }

  def fullReduce(): Term = {
    if (representation.length == 1) {
      representation.head
    } else {
      this
    }
  }

  def +(that: Term): Term = {
    val out = that match {
      case sigma @ Sigma(_) => toSigma(this) + sigma
      case x: Term          => toSigma(this) + toSigma(that)
    }
    semiReduce(out)
  }

  def *(that: Term): Term = {
    val out = that match {
      case Pi(thatRep)    => Pi(representation combine thatRep)
      case Sigma(thatRep) => Sigma(thatRep map ((x: Term) => this * x))
      case x: Term        => this * toPi(x)
    }
    semiReduce(out)
  }
}
