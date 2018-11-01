package provingground.interface

import trepplein._, Name.Str, BinderInfo._, Level._

object LeanBug{
  import LeanRoutes._

  getMods("basic.lean")

  lazy val p: LeanParser = parser

  val expr =
    Lam(
      Binding(Str("", "n"), Const(Str("", "nat"), Vector()), Implicit),
      Lam(
        Binding(
          Str("" , "a"),
          App(
            App(App(Const(Str("", "ne"), Vector(Succ(Zero))), Const(Str("", "nat"), Vector())), Var(0)),
            App(
              App(Const(Str(Str("", "has_zero"), "zero"), Vector(Zero)), Const(Str("", "nat"), Vector())),
              Const(Str(Str("", "nat"), "has_zero"), Vector())
            )
          ),
          Default
        ),
        App(
          App(
            App(
              App(
                App(
                  Const(Str(Str("", "nat"), "cases_on"), Vector(Zero)),
                  Lam(
                    Binding(Str("", "n"), Const(Str("", "nat"), Vector()), Implicit),
                    Pi(
                      Binding(
                        Str("", "a"),
                        App(
                          App(
                            App(Const(Str("", "ne"), Vector(Succ(Zero))), Const(Str("", "nat"), Vector())),
                            Var(0)
                          ),
                          App(
                            App(
                              Const(Str(Str("", "has_zero"), "zero"), Vector(Zero)),
                              Const(Str("", "nat"), Vector())
                            ),
                            Const(Str(Str("", "nat"), "has_zero"), Vector())
                          )
                        ),
                        Default
                      ),
                      App(
                        App(
                          App(
                            App(
                              Const(Str(Str("", "has_lt"), "lt"), Vector(Zero)),
                              Const(Str("", "nat"), Vector())
                            ),
                            Const(Str(Str("", "nat"), "has_lt"), Vector())
                          ),
                          App(Const(Str(Str("", "nat"), "pred"), Vector()), Var(1))
                        ),
                        Var(1)
                      )
                    )
                  )
                ),
                Var(1)
              ),
              Lam(
                Binding(
                  Str("", "a"),
                  App(
                    App(
                      App(Const(Str("", "ne"), Vector(Succ(Zero))), Const(Str("", "nat"), Vector())),
                      Const(Str(Str("", "nat"), "zero"), Vector())
                    ),
                    App(
                      App(
                        Const(Str(Str("", "has_zero"), "zero"), Vector(Zero)),
                        Const(Str("", "nat"), Vector())
                      ),
                      Const(Str(Str("", "nat"), "has_zero"), Vector())
                    )
                  ),
                  Default
                ),
                App(
                  App(
                    Const(Str("", "id_rhs"), Vector(Zero)),
                    App(
                      App(
                        App(
                          App(
                            Const(Str(Str("", "has_lt"), "lt"), Vector(Zero)),
                            Const(Str("", "nat"), Vector())
                          ),
                          Const(Str(Str("", "nat"), "has_lt"), Vector())
                        ),
                        App(
                          Const(Str(Str("", "nat"), "pred"), Vector()),
                          App(
                            App(
                              Const(Str(Str("" , "has_zero"), "zero"), Vector(Zero)),
                              Const(Str("" , "nat"), Vector())
                            ),
                            Const(Str(Str("" , "nat"), "has_zero"), Vector())
                          )
                        )
                      ),
                      App(
                        App(
                          Const(Str(Str("" , "has_zero"), "zero"), Vector(Zero)),
                          Const(Str("" , "nat"), Vector())
                        ),
                        Const(Str(Str("" , "nat"), "has_zero"), Vector())
                      )
                    )
                  ),
                  App(
                    App(
                      App(
                        App(
                          Const(Str("" , "absurd"), Vector(Zero)),
                          App(
                            App(
                              App(
                                Const(Str("" , "eq"), Vector(Succ(Zero))),
                                Const(Str("" , "nat"), Vector())
                              ),
                              App(
                                App(
                                  Const(Str(Str("" , "has_zero"), "zero"), Vector(Zero)),
                                  Const(Str("" , "nat"), Vector())
                                ),
                                Const(Str(Str("" , "nat"), "has_zero"), Vector())
                              )
                            ),
                            App(
                              App(
                                Const(Str(Str("" , "has_zero"), "zero"), Vector(Zero)),
                                Const(Str("" , "nat"), Vector())
                              ),
                              Const(Str(Str("" , "nat"), "has_zero"), Vector())
                            )
                          )
                        ),
                        App(
                          App(
                            App(
                              App(
                                Const(Str(Str("" , "has_lt"), "lt"), Vector(Zero)),
                                Const(Str("" , "nat"), Vector())
                              ),
                              Const(Str(Str("" , "nat"), "has_lt"), Vector())
                            ),
                            App(
                              Const(Str(Str("" , "nat"), "pred"), Vector()),
                              App(
                                App(
                                  Const(Str(Str("" , "has_zero"), "zero"), Vector(Zero)),
                                  Const(Str("" , "nat"), Vector())
                                ),
                                Const(Str(Str("" , "nat"), "has_zero"), Vector())
                              )
                            )
                          ),
                          App(
                            App(
                              Const(Str(Str("" , "has_zero"), "zero"), Vector(Zero)),
                              Const(Str("" , "nat"), Vector())
                            ),
                            Const(Str(Str("" , "nat"), "has_zero"), Vector())
                          )
                        )
                      ),
                      App(
                        App(Const(Str("" , "rfl"), Vector(Succ(Zero))), Const(Str("" , "nat"), Vector())),
                        App(
                          App(
                            Const(Str(Str("" , "has_zero"), "zero"), Vector(Zero)),
                            Const(Str("" , "nat"), Vector())
                          ),
                          Const(Str(Str("" , "nat"), "has_zero"), Vector())
                        )
                      )
                    ),
                    Var(0)
                  )
                )
              )
            ),
            Lam(
              Binding(Str("" , "n"), Const(Str("" , "nat"), Vector()), Default),
              Lam(
                Binding(
                  Str("" , "a"),
                  App(
                    App(
                      App(Const(Str("" , "ne"), Vector(Succ(Zero))), Const(Str("" , "nat"), Vector())),
                      App(Const(Str(Str("" , "nat"), "succ"), Vector()), Var(0))
                    ),
                    App(
                      App(
                        Const(Str(Str("" , "has_zero"), "zero"), Vector(Zero)),
                        Const(Str("" , "nat"), Vector())
                      ),
                      Const(Str(Str("" , "nat"), "has_zero"), Vector())
                    )
                  ),
                  Default
                ),
                App(
                  App(
                    Const(Str("" , "id_rhs"), Vector(Zero)),
                    App(
                      App(
                        App(
                          App(
                            Const(Str(Str("" , "has_lt"), "lt"), Vector(Zero)),
                            Const(Str("" , "nat"), Vector())
                          ),
                          Const(Str(Str("" , "nat"), "has_lt"), Vector())
                        ),
                        App(
                          Const(Str(Str("" , "nat"), "pred"), Vector()),
                          App(Const(Str(Str("" , "nat"), "succ"), Vector()), Var(1))
                        )
                      ),
                      App(Const(Str(Str("" , "nat"), "succ"), Vector()), Var(1))
                    )
                  ),
                  App(
                    App(
                      App(
                        Const(Str(Str("" , "nat"), "lt_succ_of_le"), Vector()),
                        App(
                          Const(Str(Str("" , "nat"), "pred"), Vector()),
                          App(Const(Str(Str("" , "nat"), "succ"), Vector()), Var(1))
                        )
                      ),
                      Var(1)
                    ),
                    App(
                      Const(Str(Str(Str("" , "nat"), "less_than_or_equal"), "refl"), Vector()),
                      App(
                        Const(Str(Str("" , "nat"), "pred"), Vector()),
                        App(Const(Str(Str("" , "nat"), "succ"), Vector()), Var(1))
                      )
                    )
                  )
                )
              )
            )
          ),
          Var(0)
        )
      )

  )
}
