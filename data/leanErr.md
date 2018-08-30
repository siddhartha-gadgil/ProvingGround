```scala
while parsing nat.less_than_or_equal.dcases_on,
  got provingground.interface.LeanParser$ParseException:
    provingground.interface.RecFoldException:
      Failure to fold recursive Function for nat.less_than_or_equal, recursion function
      (_ : (('r) ('q)) (_)) ↦
        ((InducDataSym((nat.less_than_or_equal.step) ('q)) : ($hvi : nat ) ~> ((_ : ((nat.less_than_or_equal) ('q)) ($hvi) ) ~> (((('r) ($hvi)) (_)) → ((('r) ((nat.succ) ($hvi))) (_))))) ↦ (($hru : nat) ↦ (ind{(nat.less_than_or_equal) ('q)($hru)}{($hiz : nat) ↦ ((_ : ((nat.less_than_or_equal) ('q)) ($hiz)) ↦ ((('r) ($hiz)) (_)))}(_)(InducDataSym((nat.less_than_or_equal.step) ('q))))))
    with error provingground.HoTT$ApplnFailException:
      function (InducDataSym((nat.less_than_or_equal.step) ('q)) : ($hvi : nat ) ~> ((_ : ((nat.less_than_or_equal) ('q)) ($hvi) ) ~> (((('r) ($hvi)) (_)) → ((('r) ((nat.succ) ($hvi))) (_))))) ↦ (($hru : nat) ↦ (ind{(nat.less_than_or_equal) ('q)($hru)}{($hiz : nat) ↦ ((_ : ((nat.less_than_or_equal) ('q)) ($hiz)) ↦ ((('r) ($hiz)) (_)))}(_)(InducDataSym((nat.less_than_or_equal.step) ('q)))))
        with domain(optional)
          Some(($hvi : nat ) ~> ((_ : ((nat.less_than_or_equal) ('q)) ($hvi) ) ~> (((('r) ($hvi)) (_)) → ((('r) ((nat.succ) ($hvi))) (_)))))
        cannot act on given term
          ('u : nat) ↦ ((_ : ((nat.less_than_or_equal) ('q)) ('u)) ↦ ((_ : (_ : ((nat.less_than_or_equal) ('q)) ('u) ) ~> ((('r) ('u)) (_))) ↦ ((_ : ((nat.less_than_or_equal) ('q)) ((nat.succ) ('u))) ↦ (_))))
        with type
          ('u : nat ) ~> ((((nat.less_than_or_equal) ('q)) ('u)) → (((_ : ((nat.less_than_or_equal) ('q)) ('u) ) ~> ((('r) ('u)) (_))) → ((((nat.less_than_or_equal) ('q)) ((nat.succ) ('u))) → ((('r) ((nat.succ) ('u))) (_)))))
```

## To correct:

* Replace `(_ : (_ : ((nat.less_than_or_equal) ('q)) ('u) ) ~> ((('r) ('u)) (_)))` (a variable) by `_ : ((('r) ('u)) (_))`, i.e., at the level of types,
* replace `(_ : ((nat.less_than_or_equal) ('q)) ('u) ) ~> ((('r) ('u)) (_))` by `(('r) ('u')) (_)`
* this is valid if there is a witness in scope for the given proposition.
