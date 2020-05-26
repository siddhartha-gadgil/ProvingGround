import provingground._ , learning._, interface._, translation._, HoTT._
import monix.execution.Scheduler.Implicits.global
val A = Type.sym
val B = Type.sym
val MP = A ~>: (B ~>: (A ->: (A ->: B) ->: B))
val ts = TermState(FiniteDistribution.unif(Type), FiniteDistribution.unif(Type), goals = FiniteDistribution.unif(MP))
val tg = TermGenParams.zero.copy(appW = 0.1, unAppW = 0.1)
val lp = LocalProver(ts, tg)
import HoTTMessages._

val web = new HoTTPostWeb()
val ws = WebState[HoTTPostWeb, HoTTPostWeb.ID](web)
val ws1 = ws.post(lp, Set())
val ws2 = ws1.flatMap(w => w.postApex(SeekGoal(MP, Context.Empty)))
val ws3 = ws2.flatMap(w => w.act(HoTTBot.fullGoalInContext ))
val ws4 = ws3.flatMap(w => w.act(HoTTBot.goalToProver(0.3, 0.7)))
val ws5 = ws4.flatMap(w => w.act(HoTTBot.lpToFinalState ))
val ws6 = ws5.flatMap(w => w.act(HoTTBot.reportSuccesses ))
