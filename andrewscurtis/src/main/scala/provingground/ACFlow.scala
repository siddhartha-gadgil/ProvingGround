package provingground.andrewscurtis

import provingground._

import learning.{SnapShot, _}

import akka.stream._

import akka.stream.scaladsl.{Source => Src, _}

import ACElem._

object ACFlow {
  implicit val system = Hub.system

  implicit val mat = ActorMaterializer()

  //  type Snap = SnapShot[(FiniteDistribution[AtomicMove], FiniteDistribution[Moves]), Param]

  val fl = Flow[Snap]

  /**
    * A source from an actor, to which messages are sent to start the flow.
    * Actor reference only after materialization.
    */
  val src = Src.actorRef[SnapShot[(FiniteDistribution[AtomicMove],
                                   FiniteDistribution[Moves]),
                                  Param]](100, OverflowStrategy.dropHead)

  /**
    * Flow extracting actor names and number of loops from snapshot.
    */
  val loopsFlow =
    fl map { (snap) => (snap.name, snap.loops)
    }

  /**
    * Flow extracting all theorems (with weights etc) at a stage from the snapshot at that stage.
    */
  val thmsFlow = fl mapConcat (ACThm.fromSnap)

  /**
    * Flow extracting all elements, i.e., sequences of moves, (with weights etc) at a stage from the snapshot at that stage.
    */
  def elemsFlow =
    fl mapConcat (ACElem.fromSnap)

  /**
    * Flow extracting weights on atomic moves from a snapshot. Only evolved state stored.
    */
  def fdMFlow =
    fl map { (snap) => (snap.name, snap.state._1)
    }

  def moveWeightsFlow =
    fl map { (snap) => ACMoveWeights(snap.name, snap.state._1, snap.loops)
    }
}
