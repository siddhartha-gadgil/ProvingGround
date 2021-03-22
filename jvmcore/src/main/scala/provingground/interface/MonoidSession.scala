package provingground.interface
object MonoidSession {
  import provingground._, learning._, interface._, translation._, HoTT._
  import scribe._, writer._, Utils._
  import library._, MonoidSimple._
  val tg = TermGenParams.zero.copy(appW = 0.1, unAppW = 0.1)
  val goal = eqM(l)(r) 
  val ts =
    TermState(
      dist1,
      dist1.map(_.typ),
      goals = FiniteDistribution.unif(goal)
    )
  val lp = LocalProver(ts, tg)
  import HoTTBot._
  val bs = Vector(
    lpLemmas,
    scaleSplitLemmas(1),
    lemmaTangents(),
    lptToTermResult,
    termResultToFinalState,
    reportProofsSimple(Vector(goal), goalOpt = Some(goal))
  )
  val sess = new HoTTWebSession(bots = bs, completionResponse = None)
  def run() = {
    sess.post(lp, Set())
  }

  def main(args: Array[String]): Unit = {
    sess.post(lp, Set())
    while (Utils.running) {
      Thread.sleep(100)
    }
  }

}
