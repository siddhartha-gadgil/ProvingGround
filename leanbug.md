# Lean non-bug

While using lean parser with equations  for `eq_of_heq` (mapping heterogeneous to ordinary equality), we get an error. Logs are

```scala
provingground.interface.LeanParserEq#recAppEq:104 vecInter.size: 3
provingground.interface.LeanParserEq#recAppEq:105 ind.intros.size: 1
provingground.interface.LeanParserEq#recAppEq:106 ind.typFamily: eq('q)('r)
provingground.interface.LeanParserEq#recAppEq:107 target.fansi: ∏($egybk : 'q){ ∏($egxsm : eq('q)('r)($egybk)){ 's($egybk) } }
provingground.interface.LeanParserEq#recAppEq:108 recFn.fansi:
  (`InducDataSym(((eq.refl) ('q)) ('r)) : 's('r)) ↦
    ($egybk : 'q) ↦
      induc_{
         eq('q)('r) ; ($egxsl : 'q) ↦ ($egxsm : eq('q)('r)($egxsl)) ↦ 's($egxsl)
         }(`InducDataSym(((eq.refl) ('q)) ('r)))
```
