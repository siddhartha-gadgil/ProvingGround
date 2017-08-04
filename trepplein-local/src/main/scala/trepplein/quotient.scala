package trepplein

object quotient {
  val univParams = Vector(Level.Param(Name("u")))
  val A = LocalConst(
    Binding(Name("A"), Sort(univParams(0)), BinderInfo.Implicit))
  val R = LocalConst(
    Binding(Name("R"), A -->: A -->: Sort.Prop, BinderInfo.Default))

  val quot = Axiom(Name("quot"),
                   univParams,
                   Pis(A, R)(Sort(univParams(0))),
                   builtin = true)

  val quotMk = Axiom(
    Name.Str(quot.name, "mk"),
    univParams,
    Pis(A, R)(A -->: Apps(Const(quot.name, univParams), A, R)),
    builtin = true)

  val liftUnivParams = univParams :+ Level.Param(Name("v"))
  val B = LocalConst(
    Binding(Name("B"), Sort(liftUnivParams(1)), BinderInfo.Implicit))
  val f = LocalConst(Binding(Name("f"), A -->: B, BinderInfo.Default))
  val Seq(a, b) =
    Seq("a", "b").map(n => LocalConst(Binding(Name(n), A, BinderInfo.Default)))

  val quotLift = Axiom(
    Name.Str(quot.name, "lift"),
    liftUnivParams,
    Pis(A, R, B, f)(
      Pis(a, b)(
        Apps(R, a, b) -->: Apps(Const(Name("eq"), Vector(liftUnivParams(1))),
                                B,
                                App(f, a),
                                App(f, b))) -->:
        Apps(Const(quot.name, Vector(liftUnivParams(0))), A, R) -->: B),
    builtin = true
  )

  val B2 = LocalConst(
    Binding(Name("B"),
            Apps(Const(quot.name, Vector(univParams(0))), A, R) -->: Sort.Prop,
            BinderInfo.Implicit))
  val q = LocalConst(
    Binding(Name("q"),
            Apps(Const(quot.name, Vector(univParams(0))), A, R),
            BinderInfo.Default))
  val quotInd = Axiom(
    Name.Str(quot.name, "ind"),
    univParams,
    Pis(A, R, B2)(Pi(a,
                     Apps(B2,
                          Apps(Const(quotMk.name, Vector(univParams(0))),
                               A,
                               R,
                               a))) -->: Pi(q, Apps(B2, q))),
    builtin = true
  )

  val h = LocalConst(
    Binding(Name("h"),
            Apps(Const(Name("eq"), Vector(liftUnivParams(1))),
                 B,
                 App(f, a),
                 App(f, b)),
            BinderInfo.Default))
  val quotRed = ReductionRule(
    Vector(A, R, B, f, a, h),
    Apps(Const(quotLift.name, univParams),
         A,
         R,
         B,
         f,
         h,
         Apps(Const(quotMk.name, Vector(univParams(0))), A, R, a)),
    Apps(f, a),
    List())
}

case object QuotMod extends Modification {
  import quotient._
  def name: Name = quot.name
  def compile(env: PreEnvironment) = new CompiledModification {
    val decls = Seq(quot, quotMk, quotInd, quotLift)
    val rules = Seq(quotRed)
    override def check(): Unit =
      decls.foldLeft(env)((env, decl) => env.addNow(decl.asAxiom))
  }
}
