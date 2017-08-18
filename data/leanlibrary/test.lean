prelude

inductive boolean : Type
| true : boolean
| false : boolean

definition not : boolean -> boolean := fun b, boolean.rec_on b boolean.false boolean.true
