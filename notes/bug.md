```scala
@ failap.func
res10: Term =
  induc_{
    eq(nat)(nat.succ('u))(nat.succ('v)) ;
    ($mfii : nat) ↦
      (_ : eq(nat)(nat.succ('u))($mfii)) ↦
        (eq(nat)(nat.succ('u))($mfii) →
          induc_{
             nat ; ($knzw : nat) ↦ 𝒰  
           }(false)(
             ('u : nat) ↦
              ('k : 𝒰 ) ↦
                ((eq(nat)('u)('u) → false) → false)
              )($mfii))
            }(
              (_ : eq(nat)(nat.succ('u))(nat.succ('u))) ↦
                induc_{
                   nat ; ($knzw : nat) ↦
                    induc_{ nat ; ($knzw : nat) ↦ 𝒰  }(
                      induc_{ nat ; ($knzw : nat) ↦ 𝒰  }(
                        (false → false)
                      )(
                        ('j : nat) ↦ ('k : 𝒰 ) ↦ false
                      )($knzw))(
                        ('j : nat) ↦
                          ('k : 𝒰 ) ↦
                            induc_{
                               nat ; ($knzw : nat) ↦ 𝒰  
                             }(false)(
                               ('j : nat) ↦
                                ('k : 𝒰 ) ↦
                                  ((eq(nat)('j)('j) → false) → false)
                                )($knzw))($knzw) }((_ : false) ↦ _)(
                                  ('j : nat) ↦
                                    ('k : induc_{
                                      nat ; ($knzw : nat) ↦ 𝒰  
                                    }(induc_{
                                       nat ; ($knzw : nat) ↦ 𝒰  
                                     }((false → false))(('j : nat) ↦ ('k : 𝒰 ) ↦ false)('j))(
                                       ('j : nat) ↦
                                        ('k : 𝒰 ) ↦ induc_{
                                          nat ; ($knzw : nat) ↦ 𝒰  }(false)(
                                            ('j : nat) ↦
                                              ('k : 𝒰 ) ↦
                                                ((eq(nat)('j)('j) → false) → false)
                                              )('j))('j)) ↦ (_b : (eq(nat)('j)('j) → false)) ↦ _)(nat.succ('u)))(_)(_)


@ failap.func.typ
res11: Typ[U] = ((eq(nat)('v)('v) → false) → false)

```
