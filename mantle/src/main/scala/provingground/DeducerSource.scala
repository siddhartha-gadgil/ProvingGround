package provingground

//import akka.stream._
import akka.stream.scaladsl._

import scala.concurrent._

import HoTT._

import Deducer._

import provingground.{FiniteDistribution => FD}

import scala.concurrent.ExecutionContext.Implicits.global

import ammonite.ops._

import scala.concurrent.duration._

class DeducerSource(ded: Deducer, initDist: FD[Term],
                  initBatch: Int,
                  batchSize: Int,
                  smooth: FD[Term] => FD[Term] = identity){
    import DeducerSource._

    import Hub.{system, materializer}

    import ded._

    def firstBatchFut = Future{nextDistribution(initDist, initBatch, false, Vector(), smooth)}

    def firstBatchConc(threads: Int) = Future.sequence{
      (1 to threads) map ((_) => Future{nextDistribution(initDist, initBatch, false, Vector(), smooth)})
    }.map ((fdsInvMap) => fdsInvMap.fold((FD.empty[Term], Vector()))(
      (fdI1, fdI2) => (fdI1._1 ++ fdI2._1, fdI1._2 ++ fdI2._2)
    ))

    def initSource = Source.fromFuture(firstBatchFut)

    def initSourceConc(threads: Int) = Source.fromFuture(firstBatchConc(threads))

    def deducBatches(fdInit: FD[Term], invMap : InvMap) =
      Source.unfold(fdInit -> (invMap)){
        case (fd, invM) =>
          val next = nextDistribution(
              fd, batchSize, true, invM, smooth)
          Some(next -> fd)}

    def deducBatchesConc(threads: Int)(fdInit: FD[Term], invMap : InvMap) =
      Source.unfoldAsync(fdInit -> (invMap)){
        case (fd, invM) =>
          val nextFut =
            Future.sequence{
              (1 to threads) map ((_) => Future{nextDistribution(fd, batchSize, true, invM, smooth)})
            }.map ((fdsInvMap) => fdsInvMap.fold((FD.empty[Term], Vector()))(
              (fdI1, fdI2) => (fdI1._1 ++ fdI2._1, fdI1._2 ++ fdI2._2)
            ))
          nextFut map ((x) => Some(x -> fd))
          }

    def deduc = initSource flatMapConcat((pair) => deducBatches(pair._1, pair._2))

    def deducConc(threads: Int) = (initSourceConc(threads)) flatMapConcat((pair) => deducBatchesConc(threads)(pair._1, pair._2))

    def deducResult = deduc.fold(FD.empty[Term]){case (_, result) => result}

    def learnBatches(fdInit: FD[Term], invMap : InvMap) =
      {
      val theorems = (fdInit filter (isTyp) map { case tp: Typ[u] => tp }).flatten.normalized()
      Source.unfold(fdInit -> (invMap)){
        case (fd, invM) =>
          val next = learnerNextDistribution(
              fd, theorems, batchSize, true, invM, smooth)
          Some(next -> fd)}
        }

    def learnBatchesConc(threads: Int)(fdInit: FD[Term], invMap : InvMap) = {
      val theorems = (fdInit filter (isTyp) map { case tp: Typ[u] => tp }).flatten.normalized()
      Source.unfoldAsync(fdInit -> (invMap)){
        case (fd, invM) =>
          val nextFut =
            Future.sequence{
              (1 to threads) map ((_) => Future{learnerNextDistribution(fd, theorems, batchSize, true, invM, smooth)})
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


    def loopy(dedLoops: Long, learnLoops: Long) =
      deduc.take(dedLoops).alsoTo(display).via(learnFlow).take(learnLoops).runWith(display)


    def loopyConc(dedLoops: Long, learnLoops: Long, threads: Int = 3) =
      deducConc(threads).take(dedLoops).alsoTo(display).via(learnFlowConc(threads)).alsoTo(display).take(learnLoops).runWith(display)

    def loopySaved(dedLoops: Long, learnLoops: Long, name : String) =
      deduc.take(dedLoops).alsoTo(display).alsoTo(saveDeduc(name)).
      via(learnFlow).take(learnLoops).alsoTo(display).alsoTo(saveLearn(name)). runWith(Sink.ignore)

    def timedRun(dedTime: FiniteDuration, learnTime: FiniteDuration, name: String) = {
      deduc.takeWithin(dedTime).alsoTo(display).alsoTo(saveDeduc(name)).
      alsoTo(Sink.foreach((fd) => println(s"Deducing: ${fd.supp.size}"))).
      via(learnFlow).takeWithin(learnTime).alsoTo(display).alsoTo(saveLearn(name)).
      alsoTo(Sink.foreach((fd) => println(s"Learning: ${fd.supp.size}"))).
      runWith(Sink.ignore)
    }

    def timedRunConc(dedTime: FiniteDuration, learnTime: FiniteDuration, name: String, threads: Int = 3) = {
      deducConc(threads).takeWithin(dedTime).alsoTo(display).alsoTo(saveDeduc(name)).
      via(learnFlowConc(threads)).takeWithin(learnTime).alsoTo(display).alsoTo(saveLearn(name)).runWith(Sink.ignore)
    }
}


object DeducerSource{
    def lastResult = Flow[FD[Term]].fold(FD.empty[Term]){case (_, result) => result}

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
            m.foreach{case (t, v) => showTimeSeries(t, v map (-math.log(_)))}
          }
        })
    }

    import FreeExprLang._

    def saveDeduc(name: String) = {
      println("saving")
      val file = cwd / 'data / s"${name}.deduc"
      println(s"saving to: $file")
      Sink.foreach{(fd: FD[Term]) =>
          println("Dummy save: see dist")
          println(scala.util.Try(writeDist(fd)))
    //    write.append(file, writeDist(fd)+"\n")
      }
    }

    def saveLearn(name: String) = {
      val file = cwd / 'data / s"${name}.learn"
      Sink.foreach{(fd: FD[Term]) =>
        write.append(file, writeDist(fd)+"\n")}
    }

    def loadDeduc(name: String) = {
      val file = cwd / 'data / s"${name}.deduc"
      val it = read.lines.iter(file) map (readDist)
      Source.fromIterator { () => it }
    }

    def loadLearn(name: String) = {
      val file = cwd / 'data / s"${name}.learn"
      val it = read.lines.iter(file) map (readDist)
      Source.fromIterator { () => it }
    }

}
