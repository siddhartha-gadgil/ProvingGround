package provingground.codeexperiments

import annotation.tailrec
//import scala.swing._
import akka.actor._
import scala.concurrent._
import scala.concurrent.duration._
import scala.util._
import akka.util.Timeout.durationToTimeout
import scala.math.BigInt.int2bigInt
import akka.pattern.ask
import scala.language.postfixOps

/** This is to experiment with using actors for background computations
		We use a Naive algorithm as this suits our purposes
  */
object FactorActors {

  trait ActorMessage

  trait ActorTask extends ActorMessage

  trait ActorResponse extends ActorMessage

  case class Query(task: ActorTask, asker: ActorRef)

  case class TimedOut(task: ActorTask) extends ActorMessage

  case class Factorise(target: BigInt) extends ActorTask

  case class PartialFactorisation(target: BigInt,
                                  factors: List[BigInt],
                                  unfactored: BigInt)

  case class Factorisation(target: BigInt, factors: List[BigInt])
      extends ActorMessage {
    def show: String =
      target.toString + " = " +
      ((factors map (_.toString)) reduce (_ + " * " + _))
  }

  case class FindPrimeFactor(target: BigInt,
                             lower: BigInt = BigInt(2),
                             upper: Option[BigInt] = None)
      extends ActorTask

  case class PrimeFactor(target: BigInt, prime: BigInt) extends ActorMessage

  case class NoPrimeFactor(target: BigInt, lower: BigInt, upper: BigInt)

  case class IsPrime(prime: BigInt) extends ActorMessage

  case object Updates extends ActorMessage

  def bigIntOpt(s: String): Option[BigInt] = {
    try {
      Some(BigInt(s))
    } catch {
      case _: Throwable => None
    }
  }

  def bigInt(s: String) = Try(BigInt(s))

  def tooBig(n: BigInt)(implicit bound: Option[BigInt]) = bound match {
    case None => false
    case Some(b: BigInt) if (n > b) => true
    case _ => false
  }

  @tailrec private def findPrimeFactor(
      n: BigInt, m: BigInt = 2, bound: Option[BigInt] = None): BigInt = {
    if (m * m > n || tooBig(m)(bound)) n
    else if (n % m == 0) m
    else findPrimeFactor(n, m + 1)
  }

  @tailrec private def findPrimeFactorOpt(
      n: BigInt,
      m: BigInt = 2,
      bound: Option[BigInt] = None): Option[BigInt] = {
    if (m * m > n) Some(n)
    else if (tooBig(m)(bound)) None
    else if (n % m == 0) Some(m)
    else findPrimeFactorOpt(n, m + 1)
  }

  implicit val noBound: Option[BigInt] = None

  class Factoriser(implicit val system: ActorSystem) {

    class FactorActor extends Actor {
      var tasks: Set[Query] = Set.empty
      var timedout: Set[ActorTask] = Set.empty
      var messages: Set[ActorMessage] = Set.empty

      var partialFactors: Set[PartialFactorisation] = Set.empty
      var factorisations: Set[Factorisation] = Set.empty
      def receive = {
        case Factorise(n: BigInt) =>
          tasks += Query(Factorise(n), sender)
          primeFinder ! FindPrimeFactor(n)
        case PrimeFactor(p: BigInt, n: BigInt) =>
          for (Query(Factorise(m: BigInt), asker: ActorRef) <- tasks
                                                                  if m == n) {
            partialFactors += PartialFactorisation(n, List(p), n / p)
            primeFinder ! FindPrimeFactor(n / p)
          }
          for (PartialFactorisation(
          m: BigInt,
          l: List[BigInt],
          unfactored: BigInt) <- partialFactors if unfactored == n) {
            partialFactors += PartialFactorisation(n, p :: l, unfactored / p)
            primeFinder ! FindPrimeFactor(unfactored / p)
          }
        case IsPrime(n: BigInt) =>
          for (Query(Factorise(m: BigInt), asker: ActorRef) <- tasks
                                                                  if m == n) {
            factorisations += Factorisation(n, List(n))
            asker ! Factorisation(n, List(n))
          }
          for (PartialFactorisation(
          m: BigInt,
          l: List[BigInt],
          unfactored: BigInt) <- partialFactors if unfactored == n) {
            factorisations += Factorisation(n, n :: l)
            for (Query(Factorise(k: BigInt), asker: ActorRef) <- tasks
                                                                    if k == m) {
              asker ! Factorisation(n, n :: l)
            }
          }
        case TimedOut(task: ActorTask) => timedout += task
        case Updates =>
          for (Factorisation(n, l) <- factorisations if
                                     (timedout contains Factorise(n))) sender ! Factorisation(
              n, l)
      }
    }

    class PrimeFactorFinder extends Actor {
      var tasks: Set[Query] = Set.empty
      val increment = 1000
      val primeInInterval = context.actorOf(Props[PrimeInInterval])
      val knownPrime = context.actorOf(Props[KnownPrime])
      def receive = {
        case FindPrimeFactor(n: BigInt, _, _) =>
          tasks += Query(FindPrimeFactor(n), sender)
          primeInInterval ! FindPrimeFactor(n, 2, Some(increment))
          knownPrime ! FindPrimeFactor(n)
        case NoPrimeFactor(n: BigInt, lower: BigInt, upper: BigInt) =>
          sender ! FindPrimeFactor(n,
                                   lower + increment,
                                   Some(upper + increment))
        case PrimeFactor(prime: BigInt, n: BigInt) =>
          for (Query(FindPrimeFactor(m, _, _), asker) <- tasks if m == n)
          (asker ! PrimeFactor(prime, n))
        case IsPrime(n: BigInt) =>
          for (Query(FindPrimeFactor(m, _, _), asker) <- tasks if m == n)
          (asker ! IsPrime(n))
      }
    }

    class PrimeInInterval extends Actor {
      def receive = {
        case FindPrimeFactor(n: BigInt, lower: BigInt, Some(upper: BigInt)) =>
          findPrimeFactorOpt(n, lower, Some(upper)) match {
            case None => sender ! NoPrimeFactor(n, lower, upper)
            case Some(m: BigInt) if (m < n) => sender ! PrimeFactor(m, n)
            case _ => IsPrime(n)
          }
      }
    }

    class KnownPrime extends Actor {
      var primes: Set[BigInt] = Set.empty
      def receive = {
        case IsPrime(n: BigInt) => primes += n
        case FindPrimeFactor(n: BigInt, _, _) =>
          for (p <- primes if n % p == 0) sender ! PrimeFactor(p, n)
      }
    }

    import system.dispatcher

    val factorActor = system.actorOf(Props[FactorActor], "FactorActor")

    val primeFinder = system.actorOf(Props[PrimeFactorFinder], "PrimeFinder")

    def askFactors(n: BigInt): Future[Factorisation] = {

      factorActor.ask(Factorise(n))(5 seconds).mapTo[Factorisation]
    }

    def apply(n: BigInt) = askFactors(n)
  }
}
