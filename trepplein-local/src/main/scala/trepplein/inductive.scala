package trepplein

import trepplein.Level.Param

final case class InductiveType(name: Name,
                               univParams: Vector[Level.Param],
                               ty: Expr) {
  val decl = Axiom(name, univParams, ty, builtin = true)
}

final case class CompiledIndMod(indMod: IndMod, env: PreEnvironment)
    extends CompiledModification {
  import indMod._
  val tc = new TypeChecker(env.addNow(inductiveType.decl))
  import tc.NormalizedPis

  def name: Name = inductiveType.name

  def univParams: Vector[Param] = inductiveType.univParams
  val indTy                     = Const(inductiveType.name, univParams)

  val ((params, indices), level) =
    inductiveType.ty match {
      case NormalizedPis(doms, Sort(lvl)) =>
        (doms.splitAt(numParams), lvl)
    }
  val indTyWParams = Apps(indTy, params)

  case class CompiledIntro(name: Name, ty: Expr) {
    val NormalizedPis(arguments, Apps(introType, introTyArgs)) =
      NormalizedPis.instantiate(ty, params)
    val introTyIndices: List[Expr] = introTyArgs.drop(numParams)

    type ArgInfo = Either[Expr, (List[LocalConst], List[Expr])]

    val argInfos: List[ArgInfo] = arguments.map {
      case LocalConst(
          Binding(_,
                  NormalizedPis(
                    eps,
                    Apps(recArgIndTy @ Const(inductiveType.name, _), recArgs)),
                  _),
          _,
          _) =>
        require(recArgs.size >= numParams)
        tc.requireDefEq(Apps(recArgIndTy, recArgs.take(numParams)),
                        indTyWParams)
        Right((eps, recArgs.drop(numParams)))
      case nonRecArg => Left(nonRecArg)
    }

    lazy val ihs: Traversable[LocalConst] =
      (arguments, argInfos).zipped.collect {
        case (recArg, Right((eps, recIndices))) =>
          LocalConst(
            Binding("ih",
                    Pis(eps)(mkMotiveApp(recIndices, Apps(recArg, eps))),
                    BinderInfo.Default))
      }

    lazy val minorPremise = LocalConst(
      Binding(
        "h",
        Pis(arguments ++ ihs)(
          mkMotiveApp(introTyIndices,
                      Apps(Const(name, univParams), params ++ arguments))),
        BinderInfo.Default))

    lazy val redRule: ReductionRule = {
      val recCalls = arguments.zip(argInfos).collect {
        case (recArg, Right((eps, recArgIndices))) =>
          Lams(eps)(
            Apps(
              Const(elimDecl.name, elimLevelParams),
              params ++ Seq(motive) ++ minorPremises ++ recArgIndices :+ Apps(
                recArg,
                eps)))
      }
      ReductionRule(
        Vector() ++ params ++ Seq(motive) ++ minorPremises ++ indices ++ arguments,
        Apps(Const(elimDecl.name, elimLevelParams),
             params ++ Seq(motive) ++ minorPremises ++ indices
               :+ Apps(Const(name, univParams), params ++ arguments)),
        Apps(minorPremise, arguments ++ recCalls),
        List()
      )
    }

    def check(): Unit = {
      require(introTyArgs.size >= numParams)
      tc.requireDefEq(Apps(introType, introTyArgs.take(numParams)),
                      Apps(indTy, params))

      val tc0 = new TypeChecker(env)
      arguments.zip(argInfos).foreach {
        case (_, Left(nonRecArg)) =>
          tc0.inferUniverseOfType(tc0.infer(nonRecArg))
        case (_, Right((eps, _))) =>
          for (e <- eps) tc0.inferUniverseOfType(tc0.infer(e))
      }

      if (level.maybeNonZero) for (arg <- arguments) {
        val argLevel = tc.inferUniverseOfType(tc.infer(arg))
        require(argLevel <== level)
      }
    }
  }

  val compiledIntros: Vector[CompiledIntro] = intros.map(CompiledIntro.tupled)

  val elimIntoProp: Boolean = level.maybeZero &&
    (intros.size > 1 || compiledIntros.exists { intro =>
      intro.arguments.exists { arg =>
        !tc.isProof(arg) && !intro.introTyArgs.contains(arg)
      }
    })
  val elimLevel: Level =
    if (elimIntoProp) Level.Zero
    else Level.Param(Name.fresh("l", univParams.map(_.param).toSet))
  val extraElimLevelParams: Vector[Param] =
    Vector(elimLevel).collect { case p: Level.Param => p }

  val useDepElim: Boolean = level.maybeNonZero
  val motiveType: Expr =
    if (useDepElim)
      Pis(
        indices :+ LocalConst(
          Binding("c", Apps(indTy, params ++ indices), BinderInfo.Default)))(
        Sort(elimLevel))
    else
      Pis(indices)(Sort(elimLevel))
  val motive = LocalConst(Binding("C", motiveType, BinderInfo.Implicit))
  def mkMotiveApp(indices: Seq[Expr], e: Expr): Expr =
    if (useDepElim) App(Apps(motive, indices), e) else Apps(motive, indices)

  val minorPremises: Vector[LocalConst] = compiledIntros.map { _.minorPremise }
  val majorPremise = LocalConst(
    Binding("x", Apps(indTy, params ++ indices), BinderInfo.Default))
  val elimType: Expr =
    Pis(params ++ Seq(motive) ++ minorPremises ++ indices :+ majorPremise)(
      mkMotiveApp(indices, majorPremise))
  val elimLevelParams: Vector[Param] = extraElimLevelParams ++ univParams
  val elimDecl =
    Axiom(Name.Str(name, "rec"), elimLevelParams, elimType, builtin = true)

  val kIntroRule: Option[ReductionRule] =
    compiledIntros match {
      case Vector(intro) if intro.arguments.isEmpty =>
        Some(
          ReductionRule(
            Vector() ++ params ++ Seq(motive) ++ minorPremises ++ indices ++ Seq(
              majorPremise),
            Apps(Const(elimDecl.name, elimLevelParams),
                 params ++ Seq(motive) ++ minorPremises ++ indices
                   ++ Seq(majorPremise)),
            minorPremises(0),
            (intro.introTyArgs zip (params ++ indices)).filter {
              case (a, b) => a != b
            }
          ))
      case _ => None
    }

  val introDecls: Vector[Axiom] =
    for (i <- compiledIntros)
      yield Axiom(i.name, univParams, i.ty, builtin = true)

  val decls
    : Vector[Declaration] = Axiom(name, univParams, inductiveType.ty) +: introDecls :+ elimDecl
  val rules: Vector[ReductionRule] =
    if (kIntroRule.isDefined)
      kIntroRule.toVector
    else if (elimIntoProp)
      Vector()
    else
      compiledIntros.map(_.redRule)

  def check(): Unit = {
    val withType = env.addNow(inductiveType.decl)
    val withIntros = introDecls.foldLeft(withType)((env, i) => {
      i.check(withType); env.addNow(i)
    })
    withIntros.addNow(elimDecl)

    for (i <- compiledIntros) i.check()
  }
}

final case class IndMod(inductiveType: InductiveType,
                        numParams: Int,
                        intros: Vector[(Name, Expr)])
    extends Modification {
  def name: Name = inductiveType.name

  def compile(env: PreEnvironment) = CompiledIndMod(this, env)
}
