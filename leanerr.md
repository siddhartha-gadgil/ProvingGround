## Error in `subtype`

The mismatch is in the argument not having an inner symbol

```scala
@ ape.domOpt.get
res9: Typ[u] = SymbProp(ApplnSym(SymbolicFunc('u, SymbTyp('t, 0), Prop), SymbObj(Name("$agynv"), SymbTyp('t, 0))))

@ ape.argType
res10: Typ[U] = SymbProp(ApplnSym(SymbolicFunc(Name("'u"), SymbTyp('t, 0), Prop), SymbObj(Name("$agynv"), SymbTyp('t, 0))))
```

In fansi notation
```scala
@ ape.func
res59: Term = subtype.mk('t)('u)($agynv)

@ ape.domOpt.get
res60: Typ[u] = 'u($agynv)

@ ape.argType
res61: Typ[U] = 'u($agynv)
```

with the expression to parse

```c
Some(
  λ {α : Sort u} {p : (∀ (a : α), Prop)}
  {C : (∀ (n_0 : @subtype.{u} α p), Sort l)} (n : @subtype.{u} α p)
  (e_1 :
    (∀ (val : α) (property : p val), C (@subtype.mk.{u} α p val property))),
      @subtype.rec.{l u} α p C e_1 n
    )
```

The actual expression is
```scala
Lam(
  Binding(Str(, "\u03b1"), Sort(Param(Str(, "u"))), Implicit),
  Lam(
    Binding(Str(, "p"), Pi(Binding(Str(, "a"), Var(0), Default), Sort(Zero)), Implicit),
    Lam(
      Binding(
        Str(, "C"),
        Pi(
          Binding(Str(, "n"), App(App(Const(Str(, "subtype"), Vector(Param(Str(, "u")))), Var(1)), Var(0)), Default),
          Sort(Param(Str(, "l")))
        ),
        Implicit
      ),
      Lam(
        Binding(Str(, "n"), App(App(Const(Str(, "subtype"), Vector(Param(Str(, "u")))), Var(2)), Var(1)), Default),
        Lam(
          Binding(
            Str(, "e_1"),
            Pi(
              Binding(Str(, "val"), Var(3), Default),
              Pi(
                Binding(Str(, "property"), App(Var(3), Var(0)), Default),
                App(
                  Var(3),
                  App(
                    App(App(App(Const(Str(Str(, "subtype"), "mk"), Vector(Param(Str(, "u")))), Var(5)), Var(4)), Var(1)),
                    Var(0)
                  )
                )
              )
            ),
            Default
          ),
          App(
            App(
              App(
                App(App(Const(Str(Str(, "subtype"), "rec"), Vector(Param(Str(, "l")), Param(Str(, "u")))), Var(4)), Var(3)),
                Var(2)
              ),
              Var(0)
            ),
            Var(1)
          )
        )
      )
    )
  )
)
```

The stack trace is:
```
provingground.HoTT$FuncLike.apply(HoTT.scala:1384),
  provingground.HoTT$FuncLike.apply$(HoTT.scala:1383),
  provingground.HoTT$SymbolicFunc.apply(HoTT.scala:1577),
  provingground.induction.ConstructorPatternMap$CnstFncPtnMap.inducDataTyp(ConstructorPatternMap.scala:298),
  provingground.induction.ConstructorPatternMap$CnstFncPtnMap.inducDataTyp(ConstructorPatternMap.scala:267),
  provingground.induction.ConstructorPatternMap$CnstDepFuncPtnMap.inducDataTyp(ConstructorPatternMap.scala:373),
  provingground.induction.ConstructorPatternMap$CnstDepFuncPtnMap.inducDataTyp(ConstructorPatternMap.scala:321),
  provingground.induction.ConstructorSeqMap$Cons.inducData(ConstructorSeqMap.scala:156),
  provingground.induction.ConstructorSeqMap$Cons.indDataCons(ConstructorSeqMap.scala:164),
  provingground.induction.ConstructorSeqMap$Cons.$anonfun$indDataCons$3(ConstructorSeqMap.scala:171),
  provingground.induction.InductiveDefinition$DataCons.subs(InductiveCaseDefinition.scala:103),
  provingground.induction.InductiveDefinition$DataCons.subs(InductiveCaseDefinition.scala:62),
  provingground.HoTT$Subs.replace(HoTT.scala:181),
  provingground.HoTT$Subs.replace$(HoTT.scala:154),
  provingground.induction.InductiveDefinition$DataCons.replace(InductiveCaseDefinition.scala:62),
  provingground.HoTT$ApplnSym.subs(HoTT.scala:1412),
  provingground.HoTT$ApplnSym.subs(HoTT.scala:1405),
  provingground.HoTT$.$anonfun$symSubs$4(HoTT.scala:564),
  scala.Option.getOrElse(Option.scala:121),
  provingground.HoTT$.$anonfun$symSubs$1(HoTT.scala:564),
  provingground.HoTT$SymbObj.subs(HoTT.scala:524),
  provingground.HoTT$Subs.replace(HoTT.scala:181),
  provingground.HoTT$Subs.replace$(HoTT.scala:154),
  provingground.HoTT$SymbObj.replace(HoTT.scala:513),
  provingground.HoTT$LambdaFixed.subs(HoTT.scala:1829),
  provingground.HoTT$LambdaFixed.subs(HoTT.scala:1794),
  provingground.HoTT$Subs.replace(HoTT.scala:181),
  provingground.HoTT$Subs.replace$(HoTT.scala:154),
  provingground.HoTT$LambdaFixed.replace(HoTT.scala:1794),
  provingground.HoTT$Subs.replace(HoTT.scala:168),
  provingground.HoTT$Subs.replace$(HoTT.scala:154),
  provingground.HoTT$LambdaFixed.replace(HoTT.scala:1794),
  provingground.HoTT$Subs.replace(HoTT.scala:168),
  provingground.HoTT$Subs.replace$(HoTT.scala:154),
  provingground.HoTT$LambdaFixed.replace(HoTT.scala:1794),
  provingground.HoTT$Subs.replace(HoTT.scala:173),
  provingground.HoTT$Subs.replace$(HoTT.scala:154),
  provingground.HoTT$LambdaFixed.replace(HoTT.scala:1794),
  provingground.HoTT$LambdaLike.subs(HoTT.scala:1722),
  provingground.HoTT$LambdaLike.subs$(HoTT.scala:1715),
  provingground.HoTT$LambdaTerm.subs(HoTT.scala:1741),
  provingground.HoTT$LambdaTerm.subs(HoTT.scala:1741),
  provingground.HoTT$Subs.replace(HoTT.scala:181),
  provingground.HoTT$Subs.replace$(HoTT.scala:154),
  provingground.HoTT$LambdaTerm.replace(HoTT.scala:1741),
  provingground.HoTT$Subs.replace(HoTT.scala:168),
  provingground.HoTT$Subs.replace$(HoTT.scala:154),
  provingground.HoTT$LambdaTerm.replace(HoTT.scala:1741),
  provingground.HoTT$Subs.replace(HoTT.scala:168),
  provingground.HoTT$Subs.replace$(HoTT.scala:154),
  provingground.HoTT$LambdaTerm.replace(HoTT.scala:1741),
  provingground.HoTT$Subs.replace(HoTT.scala:176),
  provingground.HoTT$Subs.replace$(HoTT.scala:154),
  provingground.HoTT$LambdaTerm.replace(HoTT.scala:1741),
  provingground.HoTT$Subs.replace(HoTT.scala:173),
  provingground.HoTT$Subs.replace$(HoTT.scala:154),
  provingground.HoTT$LambdaTerm.replace(HoTT.scala:1741),
  provingground.HoTT$LambdaLike.subs(HoTT.scala:1722),
  provingground.HoTT$LambdaLike.subs$(HoTT.scala:1715),
  provingground.HoTT$LambdaTerm.subs(HoTT.scala:1741),
  provingground.HoTT$LambdaTerm.subs(HoTT.scala:1741),
  provingground.HoTT$Subs.replace(HoTT.scala:181),
  provingground.HoTT$Subs.replace$(HoTT.scala:154),
  provingground.HoTT$LambdaTerm.replace(HoTT.scala:1741),
  provingground.HoTT$Subs.replace(HoTT.scala:176),
  provingground.HoTT$Subs.replace$(HoTT.scala:154),
  provingground.HoTT$LambdaTerm.replace(HoTT.scala:1741),
  provingground.HoTT$Subs.replace(HoTT.scala:173),
  provingground.HoTT$Subs.replace$(HoTT.scala:154),
  provingground.HoTT$LambdaTerm.replace(HoTT.scala:1741),
  provingground.HoTT$LambdaLike.subs(HoTT.scala:1722),
  provingground.HoTT$LambdaLike.subs$(HoTT.scala:1715),
  provingground.HoTT$LambdaTerm.subs(HoTT.scala:1741),
  provingground.HoTT$LambdaTerm.subs(HoTT.scala:1741),
  provingground.HoTT$Subs.replace(HoTT.scala:181),
  provingground.HoTT$Subs.replace$(HoTT.scala:154),
  provingground.HoTT$LambdaTerm.replace(HoTT.scala:1741),
  provingground.HoTT$.lambda(HoTT.scala:1981),
  provingground.interface.LeanParser.$anonfun$parse$13(LeanParser.scala:328),
  monix.eval.Task$Map.apply(Task.scala:2995),
  monix.eval.Task$Map.apply(Task.scala:2991),
  monix.eval.internal.TaskRunLoop$.startFull(TaskRunLoop.scala:121),
  monix.eval.internal.TaskRunLoop$RestartCallback.onSuccess(TaskRunLoop.scala:537),
  monix.eval.Callback$$anon$2.$anonfun$onSuccess$1(Callback.scala:106),
  monix.execution.schedulers.TrampolineExecutionContext.monix$execution$schedulers$TrampolineExecutionContext$$localRunLoop(TrampolineExecutionContext.scala:109),
  monix.execution.schedulers.TrampolineExecutionContext.startLoopOptimal(TrampolineExecutionContext.scala:93),
  monix.execution.schedulers.TrampolineExecutionContext.execute(TrampolineExecutionContext.scala:78),
  monix.execution.schedulers.BatchingScheduler.execute(BatchingScheduler.scala:50),
  monix.execution.schedulers.BatchingScheduler.execute$(BatchingScheduler.scala:47),
  monix.execution.schedulers.AsyncScheduler.execute(AsyncScheduler.scala:29),
  monix.execution.schedulers.ExecuteExtensions.executeTrampolined(ExecuteExtensions.scala:86),
  monix.execution.schedulers.ExecuteExtensions.executeTrampolined$(ExecuteExtensions.scala:85),
  monix.execution.Scheduler$Extensions.executeTrampolined(Scheduler.scala:256),
  monix.eval.Callback$$anon$2.onSuccess(Callback.scala:106),
  monix.eval.internal.TaskRunLoop$.startFull(TaskRunLoop.scala:117),
  monix.eval.internal.TaskRunLoop$RestartCallback.onSuccess(TaskRunLoop.scala:537),
  monix.eval.Task$$anon$3.run(Task.scala:2161),
  java.util.concurrent.ForkJoinTask$RunnableExecuteAction.exec(ForkJoinTask.java:1402),
  java.util.concurrent.ForkJoinTask.doExec(ForkJoinTask.java:289),
  java.util.concurrent.ForkJoinPool$WorkQueue.runTask(ForkJoinPool.java:1056),
  java.util.concurrent.ForkJoinPool.runWorker(ForkJoinPool.java:1692),
  java.util.concurrent.ForkJoinWorkerThread.run(ForkJoinWorkerThread.java:157)
)
```
