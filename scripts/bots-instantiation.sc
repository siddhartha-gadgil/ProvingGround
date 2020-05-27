import provingground._ , learning._, interface._, translation._, HoTT._
import provingground.{FiniteDistribution => FD}
val ts = TermState(FD.unif(One, Zero, Star, Type), FD.unif(One, Zero, Type))
repl.pprinter.bind(translation.FansiShow.fansiPrint)
val tg = TermGenParams(sigmaW = 0)
val lp = LocalProver(ts, tg)
import HoTTMessages._
import HoTTBot._
val web = new HoTTPostWeb()
val ws = WebState[HoTTPostWeb, HoTTPostWeb.ID](web)
val ws1 = ws.post(lp, Set())
val A = Type.sym
val notAll = sigma(A)(A ->: Zero)
import monix.execution.Scheduler.Implicits.global
val ws2 = ws1.flatMap(w => w.postApex(SeekGoal(notAll, Context.Empty)))
val ws3 = ws2.flatMap(w => w.act(sigmaBackward))
val ws4 = ws3.flatMap(w => w.act(instanceFromLp))
val ws5 = ws4.flatMap(w => w.act(instanceToGoal))
val ws6 = ws5.flatMap(w => w.act(goalToProver(0.3, 0.7)))
val ws7 = ws6.flatMap(w => w.act(lpToFinalState))
val ws8 = ws7.flatMap(w => w.act(postProofs))
val ws9 = ws8.flatMap(w => w.act(deducedEquations ))
val pfs = ws9.map(_.queryApex[Proved]())