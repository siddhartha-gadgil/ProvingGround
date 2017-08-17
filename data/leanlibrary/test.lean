prelude

inductive boolean : Type
| true : boolean
| false : boolean

definition not : boolean -> boolean := fun (x : boolean), (boolean.rec boolean.false boolean.true)
