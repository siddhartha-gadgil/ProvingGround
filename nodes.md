# Term generator nodes

Verified there are no lambdas, but we can use this to look for cases

We record the cases, parameters that need enumeration and possible values.

* Init
* Atom
* Map
    * f : [Identity, Negate, GetFunc]
* MapOpt
    * f : []
* ZipMap
    * f : []
* ZipMapOpt
    * f : [UnifApplnOpt]
* FiberProductMap
    * quot : [domOf = DomFn, typeOf(_) = TypFn]
    * fiberVar : [TermsWithTyp, FuncsWithDomainFn]
    * f : [Appln, FlipAppln]
* ZipFlatMap
    * fiberVar : [TermsWithTyp]
    * f : [Proj2]
* FlatMap
    * fiberNode : [LambdaIsle, PiIsle, RecFuncsFolded, InducFuncsFolded, SigmaIsle, FoldTypFamily, LambdaTypFamilyIsle]
* FlatMapOpt
    * fiberNodeOpt : []

```scala
Cons(
  head = BaseCons(
    headGen = BasePi(nodes = InitFunc(base = FuncsWithDomain), outputFamily = FuncsWithDomain),
    headCoeff = 0.45,
    tail = BaseCons(
      headGen = BasePi(
        nodes = ConditionFunc(
          base = FiberProductMap(
            quot = domOf,
            fiberVar = TermsWithTyp,
            f = Appln,
            baseInput = Funcs,
            output = Terms
          ),
          conditionFamily = RestrictFuncWithDom,
          outputFamily = FuncsWithDomain
        ),
        outputFamily = FuncsWithDomain
      ),
      headCoeff = 0.1,
      tail = BaseCons(
        headGen = BasePi(
          nodes = ConditionFunc(
            base = ZipMapOpt(f = UnifApplnOpt, input1 = Funcs, input2 = Terms, output = Terms),
            conditionFamily = RestrictFuncWithDom,
            outputFamily = FuncsWithDomain
          ),
          outputFamily = FuncsWithDomain
        ),
        headCoeff = 0.1,
        tail = BaseCons(
          headGen = BasePi(
            nodes = ConditionFunc(
              base = FiberProductMap(
                quot = typeOf(_),
                fiberVar = FuncsWithDomainFn,
                f = FlipAppln,
                baseInput = Terms,
                output = Terms
              ),
              conditionFamily = RestrictFuncWithDom,
              outputFamily = FuncsWithDomain
            ),
            outputFamily = FuncsWithDomain
          ),
          headCoeff = 0.1,
          tail = BaseCons(
            headGen = BasePi(nodes = LambdaIsleForFuncWithDomain, outputFamily = FuncsWithDomain),
            headCoeff = 0.1,
            tail = Target(output = FuncsWithDomain)
          )
        )
      )
    )
  ),
  tail = Cons(
    head = BaseCons(
      headGen = Map(f = Identity, input = Goals, output = TargetTyps),
      headCoeff = 0.7,
      tail = BaseCons(
        headGen = Map(f = Identity, input = Typs, output = TargetTyps),
        headCoeff = 0.25000000000000006,
        tail = BaseCons(
          headGen = Map(f = Negate, input = Typs, output = TargetTyps),
          headCoeff = 0.05,
          tail = Target(output = TargetTyps)
        )
      )
    ),
    tail = Cons(
      head = BaseCons(headGen = Init(input = Goals), headCoeff = 1.0, tail = Target(output = Goals)),
      tail = Cons(
        head = BaseCons(
          headGen = Map(f = Identity, input = Typs, output = IsleDomains),
          headCoeff = 1.0,
          tail = Target(output = IsleDomains)
        ),
        tail = Cons(
          head = BaseCons(
            headGen = BasePiOpt(nodesOpt = DomainForDefn, outputFamily = DomForInduc),
            headCoeff = 1.0,
            tail = Target(output = DomForInduc)
          ),
          tail = Cons(
            head = BaseCons(
              headGen = Init(input = InducDefns),
              headCoeff = 1.0,
              tail = Target(output = InducDefns)
            ),
            tail = Cons(
              head = BaseCons(
                headGen = BasePi(nodes = CodomainNode, outputFamily = FuncForCod),
                headCoeff = 1.0,
                tail = Target(output = FuncForCod)
              ),
              tail = Cons(
                head = BaseCons(
                  headGen = Init(input = Terms),
                  headCoeff = 0.45,
                  tail = BaseCons(
                    headGen = FiberProductMap(
                      quot = domOf,
                      fiberVar = TermsWithTyp,
                      f = Appln,
                      baseInput = Funcs,
                      output = Terms
                    ),
                    headCoeff = 0.1,
                    tail = BaseCons(
                      headGen = ZipMapOpt(
                        f = UnifApplnOpt,
                        input1 = Funcs,
                        input2 = Terms,
                        output = Terms
                      ),
                      headCoeff = 0.1,
                      tail = BaseCons(
                        headGen = FiberProductMap(
                          quot = typeOf(_),
                          fiberVar = FuncsWithDomainFn,
                          f = FlipAppln,
                          baseInput = Terms,
                          output = Terms
                        ),
                        headCoeff = 0.1,
                        tail = BaseCons(
                          headGen = FlatMap(
                            baseInput = Typs,
                            fiberNode = LambdaIsle,
                            output = Terms
                          ),
                          headCoeff = 0.1,
                          tail = BaseCons(
                            headGen = BaseThenCondition(
                              gen = FlatMap(baseInput = Typs, fiberNode = PiIsle, output = Typs),
                              output = Terms,
                              condition = Restrict(optMap = TypAsTermOpt)
                            ),
                            headCoeff = 0.05,
                            tail = BaseCons(
                              headGen = ZipFlatMap(
                                baseInput = TargetTyps,
                                fiberVar = TermsWithTyp,
                                f = Proj2,
                                output = Terms
                              ),
                              headCoeff = 0.05,
                              tail = BaseCons(
                                headGen = FlatMap(
                                  baseInput = InducDefns,
                                  fiberNode = RecFuncsFolded,
                                  output = Terms
                                ),
                                headCoeff = 0.05,
                                tail = BaseCons(
                                  headGen = FlatMap(
                                    baseInput = InducDefns,
                                    fiberNode = InducFuncsFolded,
                                    output = Terms
                                  ),
                                  headCoeff = 0.05,
                                  tail = Target(output = Terms)
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                ),
                tail = Cons(
                  head = BaseCons(
                    headGen = Init(input = Typs),
                    headCoeff = 0.49999999999999994,
                    tail = BaseCons(
                      headGen = BaseThenCondition(
                        gen = FiberProductMap(
                          quot = domOf,
                          fiberVar = TermsWithTyp,
                          f = Appln,
                          baseInput = TypFamilies,
                          output = Terms
                        ),
                        output = Typs,
                        condition = Restrict(optMap = TypOpt)
                      ),
                      headCoeff = 0.1,
                      tail = BaseCons(
                        headGen = BaseThenCondition(
                          gen = ZipMapOpt(
                            f = UnifApplnOpt,
                            input1 = TypFamilies,
                            input2 = Terms,
                            output = Terms
                          ),
                          output = Typs,
                          condition = Restrict(optMap = TypOpt)
                        ),
                        headCoeff = 0.1,
                        tail = BaseCons(
                          headGen = FlatMap(baseInput = Typs, fiberNode = PiIsle, output = Typs),
                          headCoeff = 0.1,
                          tail = BaseCons(
                            headGen = FlatMap(
                              baseInput = Typs,
                              fiberNode = SigmaIsle,
                              output = Typs
                            ),
                            headCoeff = 0.05,
                            tail = BaseCons(
                              headGen = FlatMap(
                                baseInput = TypFamilies,
                                fiberNode = FoldTypFamily,
                                output = Typs
                              ),
                              headCoeff = 0.05,
                              tail = BaseCons(
                                headGen = BaseThenCondition(
                                  gen = FlatMap(
                                    baseInput = InducDefns,
                                    fiberNode = RecFuncsFolded,
                                    output = Terms
                                  ),
                                  output = Typs,
                                  condition = Restrict(optMap = TypOpt)
                                ),
                                headCoeff = 0.05,
                                tail = BaseCons(
                                  headGen = BaseThenCondition(
                                    gen = FlatMap(
                                      baseInput = InducDefns,
                                      fiberNode = InducFuncsFolded,
                                      output = Terms
                                    ),
                                    output = Typs,
                                    condition = Restrict(optMap = TypOpt)
                                  ),
                                  headCoeff = 0.05,
                                  tail = Target(output = Typs)
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  ),
                  tail = Cons(
                    head = BaseCons(
                      headGen = Init(input = Funcs),
                      headCoeff = 0.45,
                      tail = BaseCons(
                        headGen = BaseThenCondition(
                          gen = FiberProductMap(
                            quot = domOf,
                            fiberVar = TermsWithTyp,
                            f = Appln,
                            baseInput = Funcs,
                            output = Terms
                          ),
                          output = Funcs,
                          condition = Restrict(optMap = FuncOpt)
                        ),
                        headCoeff = 0.1,
                        tail = BaseCons(
                          headGen = BaseThenCondition(
                            gen = ZipMapOpt(
                              f = UnifApplnOpt,
                              input1 = Funcs,
                              input2 = Terms,
                              output = Terms
                            ),
                            output = Funcs,
                            condition = Restrict(optMap = FuncOpt)
                          ),
                          headCoeff = 0.1,
                          tail = BaseCons(
                            headGen = BaseThenCondition(
                              gen = FiberProductMap(
                                quot = typeOf(_),
                                fiberVar = FuncsWithDomainFn,
                                f = FlipAppln,
                                baseInput = Terms,
                                output = Terms
                              ),
                              output = Funcs,
                              condition = Restrict(optMap = FuncOpt)
                            ),
                            headCoeff = 0.1,
                            tail = BaseCons(
                              headGen = BaseThenCondition(
                                gen = FlatMap(
                                  baseInput = Typs,
                                  fiberNode = LambdaIsle,
                                  output = Terms
                                ),
                                output = Funcs,
                                condition = Restrict(optMap = FuncOpt)
                              ),
                              headCoeff = 0.1,
                              tail = BaseCons(
                                headGen = BaseThenCondition(
                                  gen = ZipFlatMap(
                                    baseInput = TargetTyps,
                                    fiberVar = TermsWithTyp,
                                    f = Proj2,
                                    output = Terms
                                  ),
                                  output = Funcs,
                                  condition = Restrict(optMap = FuncOpt)
                                ),
                                headCoeff = 0.05,
                                tail = BaseCons(
                                  headGen = BaseThenCondition(
                                    gen = FlatMap(
                                      baseInput = InducDefns,
                                      fiberNode = RecFuncsFolded,
                                      output = Terms
                                    ),
                                    output = Funcs,
                                    condition = Restrict(optMap = FuncOpt)
                                  ),
                                  headCoeff = 0.05,
                                  tail = BaseCons(
                                    headGen = BaseThenCondition(
                                      gen = FlatMap(
                                        baseInput = InducDefns,
                                        fiberNode = InducFuncsFolded,
                                        output = Terms
                                      ),
                                      output = Funcs,
                                      condition = Restrict(optMap = FuncOpt)
                                    ),
                                    headCoeff = 0.05,
                                    tail = Target(output = Funcs)
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    ),
                    tail = Cons(
                      head = BaseCons(
                        headGen = Init(input = TypFamilies),
                        headCoeff = 0.45,
                        tail = BaseCons(
                          headGen = BaseThenCondition(
                            gen = FiberProductMap(
                              quot = domOf,
                              fiberVar = TermsWithTyp,
                              f = Appln,
                              baseInput = TypFamilies,
                              output = Terms
                            ),
                            output = TypFamilies,
                            condition = Restrict(optMap = TypFamilyOpt)
                          ),
                          headCoeff = 0.1,
                          tail = BaseCons(
                            headGen = BaseThenCondition(
                              gen = ZipMapOpt(
                                f = UnifApplnOpt,
                                input1 = TypFamilies,
                                input2 = Terms,
                                output = Terms
                              ),
                              output = TypFamilies,
                              condition = Restrict(optMap = TypFamilyOpt)
                            ),
                            headCoeff = 0.1,
                            tail = BaseCons(
                              headGen = BaseThenCondition(
                                gen = FiberProductMap(
                                  quot = typeOf(_),
                                  fiberVar = FuncsWithDomainFn,
                                  f = FlipAppln,
                                  baseInput = Terms,
                                  output = Terms
                                ),
                                output = TypFamilies,
                                condition = Restrict(optMap = TypFamilyOpt)
                              ),
                              headCoeff = 0.1,
                              tail = BaseCons(
                                headGen = FlatMap(
                                  baseInput = Typs,
                                  fiberNode = LambdaTypFamilyIsle,
                                  output = TypFamilies
                                ),
                                headCoeff = 0.1,
                                tail = BaseCons(
                                  headGen = BaseThenCondition(
                                    gen = ZipFlatMap(
                                      baseInput = TargetTyps,
                                      fiberVar = TermsWithTyp,
                                      f = Proj2,
                                      output = Terms
                                    ),
                                    output = TypFamilies,
                                    condition = Restrict(optMap = TypFamilyOpt)
                                  ),
                                  headCoeff = 0.05,
                                  tail = BaseCons(
                                    headGen = BaseThenCondition(
                                      gen = FlatMap(
                                        baseInput = InducDefns,
                                        fiberNode = RecFuncsFolded,
                                        output = Terms
                                      ),
                                      output = TypFamilies,
                                      condition = Restrict(optMap = TypFamilyOpt)
                                    ),
                                    headCoeff = 0.05,
                                    tail = BaseCons(
                                      headGen = BaseThenCondition(
                                        gen = FlatMap(
                                          baseInput = InducDefns,
                                          fiberNode = InducFuncsFolded,
                                          output = Terms
                                        ),
                                        output = TypFamilies,
                                        condition = Restrict(optMap = TypFamilyOpt)
                                      ),
                                      headCoeff = 0.05,
                                      tail = Target(output = TypFamilies)
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      ),
                      tail = Cons(
                        head = BaseCons(
                          headGen = Map(f = Identity, input = Typs, output = TypsAndFamilies),
                          headCoeff = 0.5,
                          tail = BaseCons(
                            headGen = Map(
                              f = GetFunc,
                              input = TypFamilies,
                              output = TypsAndFamilies
                            ),
                            headCoeff = 0.5,
                            tail = Target(output = TypsAndFamilies)
                          )
                        ),
                        tail = Cons(
                          head = BaseCons(
                            headGen = BasePi(
                              nodes = InitFunc(base = FuncsWithDomain),
                              outputFamily = FuncsWithDomain
                            ),
                            headCoeff = 0.45,
                            tail = BaseCons(
                              headGen = BasePi(
                                nodes = ConditionFunc(
                                  base = FiberProductMap(
                                    quot = domOf,
                                    fiberVar = TermsWithTyp,
                                    f = Appln,
                                    baseInput = Funcs,
                                    output = Terms
                                  ),
                                  conditionFamily = RestrictFuncWithDom,
                                  outputFamily = FuncsWithDomain
                                ),
                                outputFamily = FuncsWithDomain
                              ),
                              headCoeff = 0.1,
                              tail = BaseCons(
                                headGen = BasePi(
                                  nodes = ConditionFunc(
                                    base = ZipMapOpt(
                                      f = UnifApplnOpt,
                                      input1 = Funcs,
                                      input2 = Terms,
                                      output = Terms
                                    ),
                                    conditionFamily = RestrictFuncWithDom,
                                    outputFamily = FuncsWithDomain
                                  ),
                                  outputFamily = FuncsWithDomain
                                ),
                                headCoeff = 0.1,
                                tail = BaseCons(
                                  headGen = BasePi(
                                    nodes = ConditionFunc(
                                      base = FiberProductMap(
                                        quot = typeOf(_),
                                        fiberVar = FuncsWithDomainFn,
                                        f = FlipAppln,
                                        baseInput = Terms,
                                        output = Terms
                                      ),
                                      conditionFamily = RestrictFuncWithDom,
                                      outputFamily = FuncsWithDomain
                                    ),
                                    outputFamily = FuncsWithDomain
                                  ),
                                  headCoeff = 0.1,
                                  tail = BaseCons(
                                    headGen = BasePi(
                                      nodes = LambdaIsleForFuncWithDomain,
                                      outputFamily = FuncsWithDomain
                                    ),
                                    headCoeff = 0.1,
                                    tail = Target(output = FuncsWithDomain)
                                  )
                                )
                              )
                            )
                          ),
                          tail = Cons(
                            head = BaseCons(
                              headGen = BasePi(
                                nodes = InitFunc(base = TermsWithTyp),
                                outputFamily = TermsWithTyp
                              ),
                              headCoeff = 0.06750000000000003,
                              tail = BaseCons(
                                headGen = BasePi(
                                  nodes = ConditionFunc(
                                    base = FiberProductMap(
                                      quot = domOf,
                                      fiberVar = TermsWithTyp,
                                      f = Appln,
                                      baseInput = Funcs,
                                      output = Terms
                                    ),
                                    conditionFamily = WithTypSort,
                                    outputFamily = TermsWithTyp
                                  ),
                                  outputFamily = TermsWithTyp
                                ),
                                headCoeff = 0.1,
                                tail = BaseCons(
                                  headGen = BasePi(
                                    nodes = ConditionFunc(
                                      base = ZipMapOpt(
                                        f = UnifApplnOpt,
                                        input1 = Funcs,
                                        input2 = Terms,
                                        output = Terms
                                      ),
                                      conditionFamily = WithTypSort,
                                      outputFamily = TermsWithTyp
                                    ),
                                    outputFamily = TermsWithTyp
                                  ),
                                  headCoeff = 0.1,
                                  tail = BaseCons(
                                    headGen = BasePi(
                                      nodes = ConditionFunc(
                                        base = FiberProductMap(
                                          quot = typeOf(_),
                                          fiberVar = FuncsWithDomainFn,
                                          f = FlipAppln,
                                          baseInput = Terms,
                                          output = Terms
                                        ),
                                        conditionFamily = WithTypSort,
                                        outputFamily = TermsWithTyp
                                      ),
                                      outputFamily = TermsWithTyp
                                    ),
                                    headCoeff = 0.1,
                                    tail = BaseCons(
                                      headGen = BasePiOpt(
                                        nodesOpt = NodeForTyp,
                                        outputFamily = TermsWithTyp
                                      ),
                                      headCoeff = 0.41500000000000004,
                                      tail = BaseCons(
                                        headGen = BasePiOpt(
                                          nodesOpt = CurryForTyp,
                                          outputFamily = TermsWithTyp
                                        ),
                                        headCoeff = 0.41500000000000004,
                                        tail = BaseCons(
                                          headGen = BasePiOpt(
                                            nodesOpt = Incl1Node,
                                            outputFamily = TermsWithTyp
                                          ),
                                          headCoeff = 0.20750000000000002,
                                          tail = BaseCons(
                                            headGen = BasePiOpt(
                                              nodesOpt = Incl2Node,
                                              outputFamily = TermsWithTyp
                                            ),
                                            headCoeff = 0.20750000000000002,
                                            tail = BaseCons(
                                              headGen = BasePi(
                                                nodes = FoldedTargetFunctionNode,
                                                outputFamily = FuncForCod
                                              ),
                                              headCoeff = 0.05,
                                              tail = BaseCons(
                                                headGen = BasePi(
                                                  nodes = TargetInducNode,
                                                  outputFamily = TermsWithTyp
                                                ),
                                                headCoeff = 0.05,
                                                tail = BaseCons(
                                                  headGen = BasePiOpt(
                                                    nodesOpt = SolverTyp,
                                                    outputFamily = TermsWithTyp
                                                  ),
                                                  headCoeff = 0.05,
                                                  tail = BaseCons(
                                                    headGen = BasePiOpt(
                                                      nodesOpt = TypViaZeroNode,
                                                      outputFamily = TermsWithTyp
                                                    ),
                                                    headCoeff = 0.05,
                                                    tail = Target(output = TermsWithTyp)
                                                  )
                                                )
                                              )
                                            )
                                          )
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            ),
                            tail = Empty()
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
)
```
