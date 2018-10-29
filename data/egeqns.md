* `(P₁(InIsle({Terms ∈ Restrict(TypFamilyOpt)},$hw))) == (P₁(InIsle({Terms ∈ Restrict(TypFamilyOpt)},$hw)))`
* `(P₁(InIsle(A ∈ Typs,$jd))) == ((P₀(InIsle(A ∈ Typs,$jd))) * (0.6))`
* `(P₁(InIsle({Terms ∈ Restrict(FuncOpt)},$hw))) == (P₁(InIsle({Terms ∈ Restrict(FuncOpt)},$hw)))`
* `(P₁(InIsle(Wrap(f) ∈ Funcs,$ek))) == ((P₀(InIsle(Wrap(f) ∈ Funcs,$ek))) * (0.55))`
* `(P₁(InIsle(a ∈ Terms,$lt))) == ((P₀(InIsle(a ∈ Terms,$lt))) * (0.55))`
* `(P₁(InIsle(Wrap(f) ∈ Funcs,$jl))) == ((P₀(InIsle(Wrap(f) ∈ Funcs,$jl))) * (0.55))`
* `(P₁(InIsle(A ∈ Terms,$jl))) == ((P₀(InIsle(A ∈ Terms,$jl))) * (0.55))`
* `(P₁(InIsle((f) ((f) ($lt)) ∈ Terms,$lt))) == (1.0)`
* `(P₁(InIsle({(Funcs, Terms) ∈ Restrict(provingground.learning.GeneratorVariables$$Lambda$2134/300451600@42a01d1)},$lt))) == (P₁(InIsle({(Funcs, Terms) ∈ Restrict(provingground.learning.GeneratorVariables$$Lambda$2134/300451600@42a01d1)},$lt)))`
* `(P₁(InIsle(A ∈ Typs,$ld))) == ((P₀(InIsle(A ∈ Typs,$ld))) * (0.6))`
* `((P₁(f ∈ Terms)) + (P₁((a :  A) ↦ ((f) ((f) (a))) ∈ Terms))) == (P₁({Terms ∈ Restrict(FuncWithDom(A))}))`
* `(P₁({Terms ∈ Restrict(TypFamilyOpt)})) == (P₁({Terms ∈ Restrict(TypFamilyOpt)}))`
* `(P₁(InIsle({Terms ∈ Restrict(TypOpt)},$jl))) == (P₁(InIsle({Terms ∈ Restrict(TypOpt)},$jl)))`
* `(P₁(InIsle({Terms ∈ Restrict(FuncOpt)},$jd))) == (P₁(InIsle({Terms ∈ Restrict(FuncOpt)},$jd)))`
* `(P₁(InIsle((f) ((f) ($hw)) ∈ Terms,$hw))) == (1.0)`
* `(P₁(InIsle(Wrap(f) ∈ Funcs,$hw))) == ((P₀(InIsle(Wrap(f) ∈ Funcs,$hw))) * (0.55))`
* `(P₁(InIsle(f ∈ Terms,$lt))) == ((P₀(InIsle(f ∈ Terms,$lt))) * (0.55))`
* `(P₁(Wrap(f) ∈ AtCoord(FuncsWithDomain,A :: HNil))) == ((P₀(Wrap(f) ∈ AtCoord(FuncsWithDomain,A :: HNil))) * (0.55))`
* `(P₁((f) ((f) (a)) ∈ Terms)) == (((((((P₁(Wrap(f) ∈ Funcs)) * (P₁((f) (a) ∈ Terms))) / (P₁({(Funcs, Terms) ∈ Restrict(provingground.learning.GeneratorEquations$$Lambda$2204/1479577380@2f6268d9)}))) * (0.1)) + (((P₁(A ∈ TargetTyps)) * (P₁((f) ((f) (a)) ∈ AtCoord(TermsWithTyp,A :: HNil)))) * (0.05))) + (((P₁(a ∈ Terms)) * (P₁(Wrap((a :  A) ↦ ((f) ((f) (a)))) ∈ AtCoord(FuncsWithDomain,A :: HNil)))) * (0.1))) + ((((P₁(Wrap((a :  A) ↦ ((f) ((f) (a)))) ∈ Funcs)) * (P₁(a ∈ Terms))) / (P₁({(Funcs, Terms) ∈ Restrict(provingground.learning.GeneratorEquations$$Lambda$2204/1479577380@3e30dc0f)}))) * (0.1)))`
* `(P₁(InIsle(Wrap(f) ∈ Funcs,$lt))) == ((P₀(InIsle(Wrap(f) ∈ Funcs,$lt))) * (0.55))`
* `(P₁(InIsle(A ∈ Terms,$ek))) == ((P₀(InIsle(A ∈ Terms,$ek))) * (0.55))`
* `(P₁(InIsle(A ∈ Terms,$ld))) == ((P₀(InIsle(A ∈ Terms,$ld))) * (0.55))`
* `(P₁(InIsle({Terms ∈ Restrict(FuncOpt)},$ld))) == (P₁(InIsle({Terms ∈ Restrict(FuncOpt)},$ld)))`
* `(P₁(InIsle(a ∈ Terms,$jd))) == ((P₀(InIsle(a ∈ Terms,$jd))) * (0.55))`
* `((((((P₁((f) (a) ∈ Terms)) + (P₁(f ∈ Terms))) + (P₁((a :  A) ↦ ((f) ((f) (a))) ∈ Terms))) + (P₁(a ∈ Terms))) + (P₁((f) ((f) (a)) ∈ Terms))) + (P₁(A ∈ Terms))) == (1.0)`
* `(((((((P₁(Wrap((a :  A) ↦ ((f) ((f) (a)))) ∈ Funcs)) * (P₁((f) ((f) (a)) ∈ Terms))) + ((P₁(Wrap((a :  A) ↦ ((f) ((f) (a)))) ∈ Funcs)) * (P₁(a ∈ Terms)))) + ((P₁(Wrap(f) ∈ Funcs)) * (P₁(a ∈ Terms)))) + ((P₁(Wrap(f) ∈ Funcs)) * (P₁((f) (a) ∈ Terms)))) + ((P₁(Wrap((a :  A) ↦ ((f) ((f) (a)))) ∈ Funcs)) * (P₁((f) (a) ∈ Terms)))) + ((P₁(Wrap(f) ∈ Funcs)) * (P₁((f) ((f) (a)) ∈ Terms)))) == (P₁({(Funcs, Terms) ∈ Restrict(provingground.learning.GeneratorVariables$$Lambda$2134/300451600@1c5b39ec)}))`
* `(P₁(InIsle({(Funcs, Terms) ∈ Restrict(provingground.learning.GeneratorVariables$$Lambda$2134/300451600@5606c2cc)},$ek))) == (P₁(InIsle({(Funcs, Terms) ∈ Restrict(provingground.learning.GeneratorVariables$$Lambda$2134/300451600@5606c2cc)},$ek)))`
* `(P₁(InIsle({Terms ∈ Restrict(TypOpt)},$hw))) == (P₁(InIsle({Terms ∈ Restrict(TypOpt)},$hw)))`
* `(P₁(InIsle({Terms ∈ Restrict(TypOpt)},$ek))) == (P₁(InIsle({Terms ∈ Restrict(TypOpt)},$ek)))`
* `(P₁(InIsle(A ∈ Terms,$jd))) == ((P₀(InIsle(A ∈ Terms,$jd))) * (0.55))`
* `((P₁(f ∈ Terms)) + (P₁((a :  A) ↦ ((f) ((f) (a))) ∈ Terms))) == (P₁({Terms ∈ Restrict(FuncOpt)}))`
* `(P₁(A ∈ Terms)) == ((P₀(A ∈ Terms)) * (0.55))`
* `(P₁(a ∈ Terms)) == ((((P₁(A ∈ TargetTyps)) * (P₁(a ∈ AtCoord(TermsWithTyp,A :: HNil)))) * (0.05)) + ((P₀(a ∈ Terms)) * (0.55)))`
* `(P₁(InIsle(f ∈ Terms,$jl))) == ((P₀(InIsle(f ∈ Terms,$jl))) * (0.55))`
* `(P₁(InIsle(A ∈ Typs,$lt))) == ((P₀(InIsle(A ∈ Typs,$lt))) * (0.6))`
* `((P₁(Wrap((a :  A) ↦ ((f) ((f) (a)))) ∈ AtCoord(FuncsWithDomain,A :: HNil))) + (P₁(Wrap(f) ∈ AtCoord(FuncsWithDomain,A :: HNil)))) == (1.0)`
* `(P₁(A ∈ Terms)) == (P₁({Terms ∈ Restrict(TypOpt)}))`
* `(P₁(InIsle({Terms ∈ Restrict(TypOpt)},$ld))) == (P₁(InIsle({Terms ∈ Restrict(TypOpt)},$ld)))`
* `(P₁(InIsle($ld ∈ Terms,$ld))) == ((P₀(InIsle($ld ∈ Terms,$ld))) * (0.55))`
* `(P₁(A ∈ TargetTyps)) == (1.0)`
* `(P₁(($hm :  A) ↦ ((f) ((f) ($hm))) ∈ Terms)) == (((P₁(InIsle((f) ((f) ($hm)) ∈ Terms,$hm))) * (P₁(A ∈ Typs))) * (0.1))`
* `(P₁((f) ((f) (a)) ∈ AtCoord(TermsWithTyp,A :: HNil))) == (((P₁((f) ((f) (a)) ∈ Terms)) / (P₁({Terms ∈ Filter(WithTyp(typ))}))) * (0.1))`
* `(P₁(InIsle($jl ∈ Terms,$jl))) == ((P₀(InIsle($jl ∈ Terms,$jl))) * (0.55))`
* `(P₁(A ∈ TargetTyps)) == ((P₁(A ∈ Typs)) * (0.5))`
* `(P₁(InIsle(f ∈ Terms,$hw))) == ((P₀(InIsle(f ∈ Terms,$hw))) * (0.55))`
* `(((P₁((f) (a) ∈ Terms)) + (P₁(a ∈ Terms))) + (P₁((f) ((f) (a)) ∈ Terms))) == (P₁({Terms ∈ Filter(WithTyp(typ))}))`
* `(P₁(InIsle($lt ∈ Terms,$lt))) == ((P₀(InIsle($lt ∈ Terms,$lt))) * (0.55))`
* `(P₁(Wrap(($lt :  A) ↦ ((f) ((f) ($lt)))) ∈ AtCoord(FuncsWithDomain,A :: HNil))) == ((P₁(InIsle((f) ((f) ($lt)) ∈ Terms,$lt))) * (0.1))`
* `(P₁(InIsle({Terms ∈ Restrict(TypFamilyOpt)},$ld))) == (P₁(InIsle({Terms ∈ Restrict(TypFamilyOpt)},$ld)))`
* `(P₁(InIsle({(Funcs, Terms) ∈ Restrict(provingground.learning.GeneratorVariables$$Lambda$2134/300451600@40e4b8cd)},$ld))) == (P₁(InIsle({(Funcs, Terms) ∈ Restrict(provingground.learning.GeneratorVariables$$Lambda$2134/300451600@40e4b8cd)},$ld)))`
* `(P₁((f) (a) ∈ Terms)) == ((((((P₁(Wrap(f) ∈ Funcs)) * (P₁(a ∈ Terms))) / (P₁({(Funcs, Terms) ∈ Restrict(provingground.learning.GeneratorEquations$$Lambda$2204/1479577380@7cc1a777)}))) * (0.1)) + (((P₁(A ∈ TargetTyps)) * (P₁((f) (a) ∈ AtCoord(TermsWithTyp,A :: HNil)))) * (0.05))) + (((P₁(Wrap(f) ∈ Funcs)) * (P₁(a ∈ AtCoord(TermsWithTyp,A :: HNil)))) * (0.1)))`
* `(P₁(InIsle(Wrap(f) ∈ Funcs,$jd))) == ((P₀(InIsle(Wrap(f) ∈ Funcs,$jd))) * (0.55))`
* `(P₁((f) (a) ∈ AtCoord(TermsWithTyp,A :: HNil))) == (((P₁((f) (a) ∈ Terms)) / (P₁({Terms ∈ Filter(WithTyp(typ))}))) * (0.1))`
* `(P₁(InIsle(f ∈ Terms,$ld))) == ((P₀(InIsle(f ∈ Terms,$ld))) * (0.55))`
* `(P₁(f ∈ Terms)) == ((P₀(f ∈ Terms)) * (0.55))`
* `(P₁(Wrap(f) ∈ Funcs)) == ((P₀(Wrap(f) ∈ Funcs)) * (0.55))`
* `(P₁(InIsle($jd ∈ Terms,$jd))) == ((P₀(InIsle($jd ∈ Terms,$jd))) * (0.55))`
* `(P₁(InIsle($ek ∈ Terms,$ek))) == ((P₀(InIsle($ek ∈ Terms,$ek))) * (0.55))`
* `(P₁(InIsle({Terms ∈ Restrict(TypFamilyOpt)},$jd))) == (P₁(InIsle({Terms ∈ Restrict(TypFamilyOpt)},$jd)))`
* `(P₁(InIsle({Terms ∈ Restrict(FuncOpt)},$jl))) == (P₁(InIsle({Terms ∈ Restrict(FuncOpt)},$jl)))`
* `((P₁(Wrap(f) ∈ Funcs)) + (P₁(Wrap((a :  A) ↦ ((f) ((f) (a)))) ∈ Funcs))) == (1.0)`
* `(P₁(InIsle($hw ∈ Terms,$hw))) == ((P₀(InIsle($hw ∈ Terms,$hw))) * (0.55))`
* `(P₁(InIsle(f ∈ Terms,$ek))) == ((P₀(InIsle(f ∈ Terms,$ek))) * (0.55))`
* `(P₁(InIsle((f) ((f) ($jd)) ∈ Terms,$jd))) == (1.0)`
* `(P₁(InIsle({(Funcs, Terms) ∈ Restrict(provingground.learning.GeneratorVariables$$Lambda$2134/300451600@51305b68)},$hw))) == (P₁(InIsle({(Funcs, Terms) ∈ Restrict(provingground.learning.GeneratorVariables$$Lambda$2134/300451600@51305b68)},$hw)))`
* `(P₁(InIsle(A ∈ Typs,$hw))) == ((P₀(InIsle(A ∈ Typs,$hw))) * (0.6))`
* `(P₁(InIsle({Terms ∈ Restrict(TypFamilyOpt)},$lt))) == (P₁(InIsle({Terms ∈ Restrict(TypFamilyOpt)},$lt)))`
* `(P₁(InIsle({Terms ∈ Restrict(TypFamilyOpt)},$jl))) == (P₁(InIsle({Terms ∈ Restrict(TypFamilyOpt)},$jl)))`
* `(P₁(InIsle({(Funcs, Terms) ∈ Restrict(provingground.learning.GeneratorVariables$$Lambda$2134/300451600@1e073bf9)},$jd))) == (P₁(InIsle({(Funcs, Terms) ∈ Restrict(provingground.learning.GeneratorVariables$$Lambda$2134/300451600@1e073bf9)},$jd)))`
* `(P₁(A ∈ Typs)) == ((((P₁(A ∈ Terms)) / (P₁({Terms ∈ Restrict(TypOpt)}))) * (0.1)) + ((P₀(A ∈ Typs)) * (0.6)))`
* `(P₁(InIsle((f) ((f) ($jl)) ∈ Terms,$jl))) == (1.0)`
* `(P₁(InIsle(A ∈ Typs,$ek))) == ((P₀(InIsle(A ∈ Typs,$ek))) * (0.6))`
* `(P₁(InIsle(a ∈ Terms,$jl))) == ((P₀(InIsle(a ∈ Terms,$jl))) * (0.55))`
* `(P₁(InIsle(A ∈ Typs,$jl))) == ((P₀(InIsle(A ∈ Typs,$jl))) * (0.6))`
* `(P₁(InIsle((f) ((f) ($ld)) ∈ Terms,$ld))) == (1.0)`
* `(P₁(a ∈ AtCoord(TermsWithTyp,A :: HNil))) == (((P₀(a ∈ AtCoord(TermsWithTyp,A :: HNil))) * (0.55)) + (((P₁(a ∈ Terms)) / (P₁({Terms ∈ Filter(WithTyp(typ))}))) * (0.1)))`
* `(P₁(InIsle(a ∈ Terms,$ld))) == ((P₀(InIsle(a ∈ Terms,$ld))) * (0.55))`
* `(P₁(InIsle({Terms ∈ Restrict(TypOpt)},$jd))) == (P₁(InIsle({Terms ∈ Restrict(TypOpt)},$jd)))`
* `(P₁(InIsle(A ∈ Terms,$lt))) == ((P₀(InIsle(A ∈ Terms,$lt))) * (0.55))`
* `(((P₁((f) (a) ∈ AtCoord(TermsWithTyp,A :: HNil))) + (P₁(a ∈ AtCoord(TermsWithTyp,A :: HNil)))) + (P₁((f) ((f) (a)) ∈ AtCoord(TermsWithTyp,A :: HNil)))) == (1.0)`
* `(P₁(InIsle(A ∈ Terms,$hw))) == ((P₀(InIsle(A ∈ Terms,$hw))) * (0.55))`
* `(P₁(InIsle({Terms ∈ Restrict(FuncOpt)},$ek))) == (P₁(InIsle({Terms ∈ Restrict(FuncOpt)},$ek)))`
* `(P₁(InIsle({Terms ∈ Restrict(TypOpt)},$lt))) == (P₁(InIsle({Terms ∈ Restrict(TypOpt)},$lt)))`
* `(P₁(InIsle({Terms ∈ Restrict(FuncOpt)},$lt))) == (P₁(InIsle({Terms ∈ Restrict(FuncOpt)},$lt)))`
* `(P₁(InIsle(Wrap(f) ∈ Funcs,$ld))) == ((P₀(InIsle(Wrap(f) ∈ Funcs,$ld))) * (0.55))`
* `(P₁(A ∈ Typs)) == (1.0)`
* `(P₁(InIsle(a ∈ Terms,$hw))) == ((P₀(InIsle(a ∈ Terms,$hw))) * (0.55))`
* `(P₁(InIsle(f ∈ Terms,$jd))) == ((P₀(InIsle(f ∈ Terms,$jd))) * (0.55))`
* `(P₁(InIsle({(Funcs, Terms) ∈ Restrict(provingground.learning.GeneratorVariables$$Lambda$2134/300451600@425c5389)},$jl))) == (P₁(InIsle({(Funcs, Terms) ∈ Restrict(provingground.learning.GeneratorVariables$$Lambda$2134/300451600@425c5389)},$jl)))`
* `(P₁(InIsle(a ∈ Terms,$ek))) == ((P₀(InIsle(a ∈ Terms,$ek))) * (0.55))`
* `(P₁(Wrap(($ek :  A) ↦ ((f) ((f) ($ek)))) ∈ AtCoord(FuncsWithDomain,A :: HNil))) == ((P₁(InIsle((f) ((f) ($ek)) ∈ Terms,$ek))) * (0.1))`
* `(P₁(InIsle((f) ((f) ($ek)) ∈ Terms,$ek))) == (1.0)`
* `(P₁(InIsle({Terms ∈ Restrict(TypFamilyOpt)},$ek))) == (P₁(InIsle({Terms ∈ Restrict(TypFamilyOpt)},$ek)))`
