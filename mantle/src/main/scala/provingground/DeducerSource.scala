package provingground

//import akka.stream._
import akka.stream.scaladsl._

import scala.concurrent._

import HoTT._

import Deducer._

import provingground.{FiniteDistribution => FD}

import scala.concurrent.ExecutionContext.Implicits.global

class DeducerSource(ded: Deducer, initDist: FD[Term],
                  initBatch: Int,
                  batchSize: Int,
                  smooth: FD[Term] => FD[Term] = identity){
    import Hub.{system, materializer}

    import ded._

    def firstBatchFut = Future{nextDistribution(initDist, initBatch, false, Vector(), smooth)}

    def firstBatchConc(threads: Int) = Future.sequence{
      (1 to threads) map ((_) => Future{nextDistribution(initDist, initBatch, false, Vector(), smooth)})
    }.map ((fdsInvMap) => fdsInvMap.fold((FD.empty[Term], Vector()))(
      (fdI1, fdI2) => (fdI1._1 ++ fdI2._1, fdI1._2 ++ fdI2._2)
    ))

    def initSource = Source.fromFuture(firstBatchFut)

    def deducBatches(fdInit: FD[Term], invMap : InvMap) =
      Source.unfold(fdInit -> (invMap)){
        case (fd, invMap) =>
          val next = nextDistribution(
              fd, batchSize, true, invMap, smooth)
          Some(next -> fd)}

    def deducBatchesConc(threads: Int)(fdInit: FD[Term], invMap : InvMap) =
      Source.unfoldAsync(fdInit -> (invMap)){
        case (fd, invMap) =>
          val nextFut =
            Future.sequence{
              (1 to threads) map ((_) => Future{nextDistribution(fd, batchSize, true, Vector(), smooth)})
            }.map ((fdsInvMap) => fdsInvMap.fold((FD.empty[Term], Vector()))(
              (fdI1, fdI2) => (fdI1._1 ++ fdI2._1, fdI1._2 ++ fdI2._2)
            ))
          nextFut map ((x) => Some(x -> fd))
          }

    def deduc = initSource flatMapConcat((pair) => deducBatches(pair._1, pair._2))

    def deducConc(threads: Int) = initSource flatMapConcat((pair) => deducBatchesConc(threads)(pair._1, pair._2))

    def deducResult = deduc.fold(FD.empty[Term]){case (_, result) => result}

    def lastResult = Flow[FD[Term]].fold(FD.empty[Term]){case (_, result) => result}

    def learnBatches(fdInit: FD[Term], invMap : InvMap) =
      {
      val theorems = (fdInit filter (isTyp) map { case tp: Typ[u] => tp }).flatten.normalized()
      Source.unfold(fdInit -> (invMap)){
        case (fd, invMap) =>
          val next = learnerNextDistribution(
              fd, theorems, batchSize, true, invMap, smooth)
          Some(next -> fd)}
        }

    def learnBatchesConc(threads: Int)(fdInit: FD[Term], invMap : InvMap) = {
      val theorems = (fdInit filter (isTyp) map { case tp: Typ[u] => tp }).flatten.normalized()
      Source.unfoldAsync(fdInit -> (invMap)){
        case (fd, invMap) =>
          val nextFut =
            Future.sequence{
              (1 to threads) map ((_) => Future{learnerNextDistribution(fd, theorems, batchSize, true, Vector(), smooth)})
            }.map ((fdsInvMap) => fdsInvMap.fold((FD.empty[Term], Vector()))(
              (fdI1, fdI2) => (fdI1._1 ++ fdI2._1, fdI1._2 ++ fdI2._2)
            ))
          nextFut map ((x) => Some(x -> fd))
          }
        }

    def learnFlow = lastResult flatMapConcat{
      (fd) => learnBatches(fd, Vector())
    }

    def learnFlowConc(threads: Int) = lastResult flatMapConcat{
      (fd) => learnBatchesConc(threads)(fd, Vector())
    }

    def withTimeSeries(terms: => Traversable[Term]) = {
      Flow[FD[Term]].scan(FD.empty[Term] -> (Map() : Map[Term, Vector[Double]])){
        case ((_, m), fd) => (
          fd,
          (terms map ((t) =>
            (t,  m.getOrElse(t,Vector()) :+ fd(t))
          )).toMap
        )
      }
          }

    def display = {
      import WebServer._
      withTimeSeries(viewTerms).
      to(
        Sink.foreach {
          case (fd, m) => {
            showDist(fd)
            m.foreach{case (t, v) => showTimeSeries(t, v)}
          }
        })
    }

    def loopy(dedLoops: Int, learnLoops: Int) =
      deduc.take(dedLoops).alsoTo(display).via(learnFlow).runWith(display)

}
