
First error

```scala
while parsing bin_tree.below, got provingground.interface.LeanParser$ParseException: provingground.interface.RecFoldException: Failure to fold recursive Function for bin_tree,
recursion function
  (RecDataSym((bin_tree.empty) ('l)) : 𝒰 _0) ↦
    ((RecDataSym((bin_tree.leaf) ('l)) : ('l) → (𝒰 _0)) ↦
      ((RecDataSym((bin_tree.node) ('l)) : ((bin_tree) ('l)) → ((𝒰 _0) → (((bin_tree) ('l)) → ((𝒰 _0) → (𝒰 _0))))) ↦
        (rec((bin_tree) ('l))(𝒰 _0)(RecDataSym((bin_tree.empty) ('l)))(RecDataSym((bin_tree.leaf) ('l)))(RecDataSym((bin_tree.node) ('l))))))
with error provingground.HoTT$ApplnFailException:
  function (RecDataSym((bin_tree.node) ('l)) : ((bin_tree) ('l)) → ((𝒰 _0) → (((bin_tree) ('l)) → ((𝒰 _0) → (𝒰 _0))))) ↦
    (rec((bin_tree) ('l))(𝒰 _0)(punit)(('o : 'l) ↦
      (punit))(RecDataSym((bin_tree.node) ('l))))
with domain(optional)
  Some(
    ((bin_tree) ('l)) → ((𝒰 _0) → (((bin_tree) ('l)) → ((𝒰 _0) → (𝒰 _0))))
)
cannot act on given term
  ('o : (bin_tree) ('l)) ↦ (('p : (bin_tree) ('l)) ↦ (('q : 𝒰 _0) ↦ (('r : 𝒰 _0) ↦ (((pprod) (((pprod) (('m) ('o))) ('q))) (((pprod) (((pprod) (('m) ('p))) ('r))) (punit))))))
with type
  ((bin_tree) ('l)) → (((bin_tree) ('l)) → ((𝒰 _0) → ((𝒰 _0) → (𝒰 _0))))
```

Another Error

```scala
while parsing bin_tree.rec_on, got provingground.interface.LeanParser$ParseException: provingground.interface.RecFoldException: Failure to fold recursive Function for bin_tree,
recursion function
  (InducDataSym((bin_tree.empty) ('o)) : ('p) ((bin_tree.empty) ('o))) ↦
    ((InducDataSym((bin_tree.leaf) ('o)) : ($jmnhm : 'o ) ~> (('p) (((bin_tree.leaf) ('o)) ($jmnhm)))) ↦
      ((InducDataSym((bin_tree.node) ('o)) : ('t : (bin_tree) ('o) ) ~> ((('p) ('t)) → (('u : (bin_tree) ('o) ) ~> ((('p) ('u)) → (('p) ((((bin_tree.node) ('o)) ('t)) ('u))))))) ↦
        (ind((bin_tree) ('o))(($jmlwz : (bin_tree) ('o)) ↦ (('p) ($jmlwz)))(InducDataSym((bin_tree.empty) ('o)))(InducDataSym((bin_tree.leaf) ('o)))(InducDataSym((bin_tree.node) ('o))))))
with error provingground.HoTT$ApplnFailException:
  function (InducDataSym((bin_tree.node) ('o)) : ('t : (bin_tree) ('o) ) ~> ((('p) ('t)) → (('u : (bin_tree) ('o) ) ~> ((('p) ('u)) → (('p) ((((bin_tree.node) ('o)) ('t)) ('u))))))) ↦
    (ind((bin_tree) ('o))(($jmlwz : (bin_tree) ('o)) ↦ (('p) ($jmlwz)))('r)('s)(InducDataSym((bin_tree.node) ('o))))
with domain(optional)
  Some(('t : (bin_tree) ('o) ) ~> ((('p) ('t)) → (('u : (bin_tree) ('o) ) ~> ((('p) ('u)) → (('p) ((((bin_tree.node) ('o)) ('t)) ('u)))))))
cannot act on given term
  't
with type
  ('t : (bin_tree) ('o) ) ~> (('u : (bin_tree) ('o) ) ~> ((('p) ('t)) → ((('p) ('u)) → (('p) ((((bin_tree.node) ('o)) ('t)) ('u))))))

```


## An error with `nat`

```scala
while parsing nat.less_than_or_equal.drec_on, got provingground.interface.LeanParser$ParseException: provingground.interface.RecFoldException: Failure to fold recursive Function for nat.less_than_or_equal,
recursion function
  (_ : (('r) ('q)) (_)) ↦
    ((InducDataSym((nat.less_than_or_equal.step) ('q)) : ('u : nat ) ~> ((_ : ((nat.less_than_or_equal) ('q)) ('u) ) ~> (((('r) ('u)) (_)) → ((('r) ((nat.succ) ('u))) (_))))) ↦
      (($mabaz : nat) ↦
        (ind{(nat.less_than_or_equal) ('q)($mabaz)}{($lzzkw : nat) ↦
            ((_ : ((nat.less_than_or_equal) ('q)) ($lzzkw)) ↦ ((('r) ($lzzkw)) (_)))}(_)(InducDataSym((nat.less_than_or_equal.step) ('q))))))
with error provingground.HoTT$ApplnFailException:
  function (InducDataSym((nat.less_than_or_equal.step) ('q)) : ('u : nat ) ~> ((_ : ((nat.less_than_or_equal) ('q)) ('u) ) ~> (((('r) ('u)) (_)) → ((('r) ((nat.succ) ('u))) (_))))) ↦
    (($mabaz : nat) ↦
      (ind{(nat.less_than_or_equal) ('q)($mabaz)}{($lzzkw : nat) ↦ ((_ : ((nat.less_than_or_equal) ('q)) ($lzzkw)) ↦ ((('r) ($lzzkw)) (_)))}(_)(InducDataSym((nat.less_than_or_equal.step) ('q)))))
with domain(optional)
  Some(
    ('u : nat ) ~> ((_ : ((nat.less_than_or_equal) ('q)) ('u) ) ~> (((('r) ('u)) (_)) → ((('r) ((nat.succ) ('u))) (_)))))
cannot act on given term
  ('u : nat) ↦
    ((_ : ((nat.less_than_or_equal) ('q)) ('u)) ↦
      (('v : (_ : ((nat.less_than_or_equal) ('q)) ('u) ) ~> ((('r) ('u)) (_))) ↦
        ((_ : ((nat.less_than_or_equal) ('q)) ((nat.succ) ('u))) ↦ (_))))
with type
  ('u : nat ) ~> ((((nat.less_than_or_equal) ('q)) ('u)) → (((_ : ((nat.less_than_or_equal) ('q)) ('u) ) ~> ((('r) ('u)) (_))) → ((((nat.less_than_or_equal) ('q)) ((nat.succ) ('u))) → ((('r) ((nat.succ) ('u))) (_)))))
```

## First error with `nat.le`

All before this lean code worked
```
lemma pred_lt : ∀ {n : ℕ}, n ≠ 0 → pred n < n
| 0        h := absurd rfl h
| (succ a) h := lt_succ_of_le (less_than_or_equal.refl _)
```

For this, we got the error, which we note is _not_ with recursion folding.

```scala
while parsing nat.pred_lt, got provingground.interface.LeanParser$ParseException: provingground.HoTT$ApplnFailException:
  function (_ : ((nat.less_than_or_equal) ((nat.succ) (_))) ((nat.succ) ('w))) ↦ (_)
with domain(optional)
  Some(
    ((nat.less_than_or_equal) ((nat.succ) (_))) ((nat.succ) ('w)))
cannot act on given term
    (_ : ((nat.less_than_or_equal) (_)) ('w)) ↦ (_)
with type
  (((nat.less_than_or_equal) (_)) ('w)) → (((nat.less_than_or_equal) ((nat.succ) (_))) ((nat.succ) ('w)))

```
