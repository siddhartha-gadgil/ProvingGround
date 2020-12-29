package provingground.learning
import provingground._, HoTT._

import GeneratorVariables._, Expression._, TermRandomVars._, GeneratorNode._,
TermGeneratorNodes._
import provingground.learning.Sort.All
import provingground.learning.Sort.Filter
import provingground.learning.Sort.Restrict
import scala.collection.parallel.CollectionConverters._
import scala.collection.parallel.immutable._
import shapeless.HNil
import provingground.induction.ExstInducDefn
import provingground.induction.ExstInducStrucs

class DerivedEquations(
    tg: TermGeneratorNodes[TermState] = TermGeneratorNodes.Base
) {
  def finalProb[X](a: X, rv: RandomVar[X]): Expression = FinalVal(Elem(a, rv))

  def conditionedProb[O, Y](
      a: O,
      input: RandomVar[O],
      output: RandomVar[Y],
      condition: Sort[O, Y]
  ) =
    Coeff(conditionedVar(input, output, condition)) * finalProb(
      a,
      input
    )

  def asTarget(typ: Typ[Term]) =
    EquationNode(
      finalProb(typ, TargetTyps),
      Coeff(TargetTyps.fromTyp) * finalProb(typ, Typs)
    )

  def targets(typs: Set[Typ[Term]]): Set[EquationNode] = typs.map(asTarget(_))

  def recTargets(vals: Set[VarVal[_]]): Set[EquationNode] = {
    val base = vals.collect {
      case FinalVal(Elem(typ: Typ[u], Typs)) => asTarget(typ)
    }
    val inner = vals
      .collect {
        case FinalVal(InIsle(variable, boat, isle)) =>
          ((boat, isle), FinalVal(variable): VarVal[_])
      }
      .groupMap(_._1)(_._2)
      .toSet
    val innerEqs = inner.flatMap {
      case ((boat, isle), s) =>
        recTargets(s).map(_.mapVars(InIsle.variableMap(boat, isle)))
    }
    base union innerEqs
  }

  def expressionInIsle(
      exp: Expression,
      boat: Any,
      isle: Island[_, _, _, _]
  ): Option[Expression] = exp match {
    case FinalVal(InIsle(variable, b, isl)) if b == boat && isl == isle =>
      Some(FinalVal(variable))
    case Coeff(node) => Some(Coeff(node))
    case Product(x, y) =>
      for {
        a <- expressionInIsle(x, boat, isle)
        b <- expressionInIsle(y, boat, isle)
      } yield Product(a, b)
  }

  def recursiveDerived(
      init: Set[EquationNode],
      step: => (Set[EquationNode] => Set[EquationNode])
  ): Set[EquationNode] = {
    val base = step(init)
    val inner = init
      .collect {
        case EquationNode(FinalVal(InIsle(variable, boat, isle)), rhs) =>
          expressionInIsle(rhs, boat, isle).map(
            r => ((boat, isle), EquationNode(FinalVal(variable), r))
          )
      }
      .flatten
      .groupMap(_._1)(_._2)
      .toSet
    val innerEqs = inner.flatMap {
      case ((boat, isle), s) =>
        recursiveDerived(s, step).map(_.mapVars(InIsle.variableMap(boat, isle)))
    }
    base union innerEqs
  }

  def conditionWithTyp(t: Term): EquationNode =
    EquationNode(
      finalProb(t, termsWithTyp(t.typ)),
      conditionedProb(
        t,
        Terms,
        termsWithTyp(t.typ),
        Sort.Filter[Term](WithTyp(t.typ))
      )
    )

  def conditionAsFunction(t: Term): Set[EquationNode] =
    ExstFunc
      .opt(t)
      .map { f =>
        Set(
          EquationNode(
            finalProb(f, Funcs),
            conditionedProb(t, Terms, Funcs, funcSort)
          ),
          EquationNode(
            finalProb(f, funcsWithDomain(f.dom)),
            conditionedProb(
              t,
              Terms,
              funcsWithDomain(f.dom),
              Sort.Restrict(FuncWithDom(f.dom))
            )
          )
        )
      }
      .getOrElse(Set.empty[EquationNode])

  def conditionAsTypFamily(t: Term): Set[EquationNode] =
    if (isTypFamily(t))
      Set(
        EquationNode(
          finalProb(t, TypFamilies),
          conditionedProb(t, Terms, TypFamilies, typFamilySort)
        )
      )
    else Set.empty[EquationNode]

  import tg._

  def initEquationOpt[Y](
      x: Term,
      rv: RandomVar[Y],
      sort: Sort[Term, Y]
  ): Option[EquationNode] = sort match {
    case All() => None
    case Filter(pred) =>
      if (pred(x))
        Some(
          EquationNode(
            finalProb(x, rv),
            Coeff(Init(rv)) * finalProb(x, Terms)
          )
        )
      else None
    case Restrict(optMap) =>
      optMap(x).map(
        y =>
          EquationNode(
            finalProb(y, rv),
            Coeff(Init(rv)) * finalProb(x, Terms)
          )
      )
  }

  def allInitEquations(xs: Set[Term]): Set[EquationNode] =
    xs.flatMap(x => initEquationOpt(x, Funcs, Sort.Restrict(FuncOpt)))
      .union(
        xs.flatMap(
          x => initEquationOpt(x, TypFamilies, Sort.Restrict(TypFamilyOpt))
        )
      )
      .union(
        xs.flatMap(
          x =>
            initEquationOpt(
              x,
              termsWithTyp(x.typ),
              Sort.Filter[Term](WithTyp(x.typ))
            )
        )
      )
      .union(
        xs.flatMap(
          x =>
            ExstFunc
              .opt(x)
              .map { fn =>
                val rv = funcsWithDomain(fn.dom)
                EquationNode(
                  finalProb(fn, rv),
                  Coeff(Init(rv)) * finalProb(x, Terms)
                )
              }
        )
      )

  def applnFlip(eqn: EquationNode): Option[EquationNode] =
    (coeffFactor(eqn.rhs), varFactors(eqn.rhs)) match {
      case (
          Some(`applnNode`),
          Vector(Elem(Funcs, f: ExstFunc), Elem(_, a: Term))
          ) =>
        Some(
          EquationNode(
            eqn.lhs,
            Coeff(applnByArgNode) * finalProb(a, Terms) * finalProb(
              f,
              funcsWithDomain(a.typ)
            )
          )
        )
      case (
          Some(`applnByArgNode`),
          Vector(Elem(Terms, a: Term), Elem(_, f: ExstFunc))
          ) =>
        Some(
          EquationNode(
            eqn.lhs,
            Coeff(applnNode) * finalProb(f, Funcs) * finalProb(
              a,
              termsWithTyp(f.dom)
            )
          )
        )
      case _ => None
    }

  def funcFoldEqs(
      fn: Term,
      args: Vector[Term],
      accum: Set[EquationNode] = Set()
  ): Set[EquationNode] =
    args match {
      case Vector() => accum
      case a +: ys =>
        val tailFunc = fold(fn)(a)
        val f        = ExstFunc.opt(fn).get
        val lhs      = finalProb(tailFunc, Terms)
        val headEqs = Set(
          EquationNode(
            lhs,
            Coeff(applnNode) * finalProb(f, Funcs) * finalProb(
              a,
              termsWithTyp(f.dom)
            )
          ),
          EquationNode(
            lhs,
            Coeff(applnByArgNode) * finalProb(a, Terms) * finalProb(
              f,
              funcsWithDomain(a.typ)
            )
          )
        )
        funcFoldEqs(tailFunc, ys, headEqs union (accum))

    }

  def typFuncFoldEquations(
      fn: Term,
      args: Vector[Term],
      accum: Set[EquationNode] = Set()
  ): Set[EquationNode] =
    args match {
      case Vector() => accum
      case a +: Vector() =>
        val tailFunc = fold(fn)(a)
        val f        = ExstFunc.opt(fn).get
        val lhs      = finalProb(tailFunc, Typs)
        val headEq =
          EquationNode(
            lhs,
            Coeff(typApplnNode) * finalProb(f, Funcs) * finalProb(
              a,
              termsWithTyp(f.dom)
            )
          )
        accum + headEq
      case a +: ys =>
        val tailFunc = fold(fn)(a)
        val f        = ExstFunc.opt(fn).get
        val lhs      = finalProb(tailFunc, Terms)
        val headEqs = Set(
          EquationNode(
            lhs,
            Coeff(applnNode) * finalProb(f, Funcs) * finalProb(
              a,
              termsWithTyp(f.dom)
            )
          ),
          EquationNode(
            lhs,
            Coeff(applnByArgNode) * finalProb(a, Terms) * finalProb(
              f,
              funcsWithDomain(a.typ)
            )
          )
        )
        funcFoldEqs(tailFunc, ys, headEqs union (accum))

    }

  def scaleValue(boat: Term, el: Elem[_]): Expression =
    if (boat == el.element)
      (IsleScale("%boat" :: boat.typ) * -1) + Literal(1)
    else IsleScale("%boat" :: boat.typ) * FinalVal(el)

  def formalEquations(
      t: Term,
      ctx: Context = Context.Empty
  ): Set[EquationNode] = {
    val base: Set[EquationNode] = t match {
      case MiscAppln(fn: FuncLike[u, v], a) =>
        val f   = ExstFunc(fn)
        val lhs = finalProb(t, Terms)
        val funcSet: Set[EquationNode] = Set(
          EquationNode(
            lhs,
            Coeff(applnNode) * finalProb(f, Funcs) * finalProb(
              a,
              termsWithTyp(f.dom)
            )
          ),
          EquationNode(
            finalProb(a, termsWithTyp(f.dom)),
            finalProb(a, Terms) /
              FinalVal(Event(Terms, Sort.Filter[Term](WithTyp(f.dom))))
          ),
          EquationNode(
            FinalVal(Event(Terms, Sort.Filter[Term](WithTyp(f.dom)))),
            finalProb(a, Terms)
          ),
          EquationNode(
            finalProb(f, Funcs),
            finalProb(fn, Terms) /
              FinalVal(Event(Terms, Sort.Restrict(FuncOpt)))
          ),
          EquationNode(
            FinalVal(Event(Terms, Sort.Restrict(FuncOpt))),
            finalProb(fn, Terms)
          ),
          EquationNode(
            lhs,
            Coeff(applnByArgNode) * finalProb(a, Terms) * finalProb(
              f,
              funcsWithDomain(a.typ)
            )
          ),
          EquationNode(
            finalProb(f, funcsWithDomain(a.typ)),
            finalProb(fn, Terms) /
              FinalVal(Event(Terms, Sort.Restrict(FuncWithDom(a.typ))))
          ),
          EquationNode(
            FinalVal(Event(Terms, Sort.Restrict(FuncWithDom(a.typ)))),
            finalProb(fn, Terms)
          )
        )
        val typFamilySet: Set[EquationNode] = TypFamilyOpt(fn).toSet.flatMap {
          f: ExstFunc =>
            Set(
              EquationNode(
                lhs,
                Coeff(applnNode) * finalProb(f, TypFamilies) * finalProb(
                  a,
                  termsWithTyp(f.dom)
                )
              ),
              EquationNode(
                finalProb(f, TypFamilies),
                finalProb(fn, Terms) /
                  FinalVal(Event(Terms, Sort.Restrict(TypFamilyOpt)))
              ),
              EquationNode(
                FinalVal(Event(Terms, Sort.Restrict(TypFamilyOpt))),
                finalProb(fn, Terms)
              )
            )
        }
        funcSet union (typFamilySet) union (formalEquations(fn)) union (formalEquations(
          a
        ))
      case idt: IdentityTyp[u] =>
        funcFoldEqs(IdentityTyp.idFunc, Vector(idt.dom, idt.lhs, idt.rhs))
      case idt: Refl[u] =>
        funcFoldEqs(IdentityTyp.reflTerm, Vector(idt.dom, idt.value))
      case lt: LambdaLike[u, v] =>
        val coeff = Coeff(tg.lambdaNode)
        val boat  = lt.variable
        val isle  = tg.lambdaIsle(lt.dom)
        val eqs   = formalEquations(lt.value, ctx.addVariable(boat))
        val isleEqs =
          eqs.map(_.mapVars(InIsle.variableMap(boat, isle)))
        val outerEqs =
          eqs.filterNot(DerivedEquations.equationDepends(boat, _))
        val bridgeEq = EquationNode(
          FinalVal(Elem(lt, Terms)),
          coeff * finalProb(lt.dom, Typs) * FinalVal(
            InIsle(Elem(lt.value, isle.islandOutput(boat)), boat, isle)
          )
        )
        val initVarElems = eqs
          .flatMap { (eq) =>
            Expression.varVals(eq.rhs) union Expression.varVals(eq.lhs)
          }
          .collect {
            case FinalVal(Elem(el: Term, Terms))
                if !el.dependsOn(lt.variable) =>
              Elem(el, Terms): Elem[_]
            case FinalVal(Elem(el: Typ[Term], Typs))
                if !el.dependsOn(lt.variable) =>
              Elem(el, Typs): Elem[_]
            case FinalVal(Elem(el: ExstFunc, Funcs))
                if !el.func.dependsOn(lt.variable) =>
              Elem(el, Funcs): Elem[_]
          } union (Set(Elem(lt.value, Terms))
          .filter(_.element.indepOf(boat))
          .map(t => t: Elem[_])) union typOpt(boat)
          .map(typ => Elem(typ, Typs))
          .toSet union ExstFunc
          .opt(boat)
          .map { fn =>
            Elem(fn, Funcs)
          }
          .toSet + Elem(boat, Terms)
        val isleIn: Set[EquationNode] =
          initVarElems.map { el =>
            val rhs =
              scaleValue(boat, el)
            EquationNode(
              InitialVal(InIsle(el, boat, isle)),
              rhs
            )
          }
        val initInIsle = initEquations(initVarElems.map(FinalVal(_)))
          .map(_.mapVars(InIsle.variableMap(boat, isle)))
        (isleIn
          .union(isleEqs)
          .union(initInIsle) + bridgeEq) union formalEquations(lt.dom, ctx) union(outerEqs)
      case pd: PiDefn[u, v] =>
        val coeff = Coeff(tg.piNode)
        val boat  = pd.variable
        val isle  = tg.piIsle(pd.domain)
        val eqs   = formalTypEquations(pd.value, ctx.addVariable(boat))
        val isleEqs =
          eqs.map(_.mapVars(InIsle.variableMap(boat, isle)))
        val outerEqs =
          eqs.filterNot(DerivedEquations.equationDepends(boat, _))
        // pprint.log("outer equations")
        // outerEqs.foreach(println(_))
        val bridgeEq = EquationNode(
          FinalVal(Elem(pd, Typs)),
          coeff * finalProb(pd.domain, Typs) * FinalVal(
            InIsle(Elem(pd.value, isle.islandOutput(boat)), boat, isle)
          )
        )
        val initVarElems = eqs
          .flatMap { (eq) =>
            Expression.varVals(eq.rhs) union Expression.varVals(eq.lhs)
          }
          .collect {
            case FinalVal(Elem(el: Term, Terms))
                if !el.dependsOn(pd.variable) =>
              Elem(el, Terms): Elem[_]
            case FinalVal(Elem(el: Typ[Term], Typs))
                if !el.dependsOn(pd.variable) =>
              Elem(el, Typs): Elem[_]
            case FinalVal(Elem(el: ExstFunc, Funcs))
                if !el.func.dependsOn(pd.variable) =>
              Elem(el, Funcs): Elem[_]
            case FinalVal(Elem(el: ExstFunc, TypFamilies))
                if !el.func.dependsOn(boat) =>
              Elem(el, TypFamilies): Elem[_]
          } union (Set(Elem(pd.value, Typs))
          .filter(_.element.indepOf(boat))
          .map(t => t: Elem[_])) union typOpt(boat)
          .map(typ => Elem(typ, Typs))
          .toSet + Elem(boat, Terms)
        val isleIn: Set[EquationNode] =
          initVarElems.map { el =>
            val rhs =
              scaleValue(boat, el)
            EquationNode(
              InitialVal(InIsle(el, boat, isle)),
              rhs
            )
          }
        val initInIsle = initEquations(initVarElems.map(FinalVal(_)))
          .map(_.mapVars(InIsle.variableMap(boat, isle)))
        (isleIn
          .union(isleEqs)
          .union(initInIsle) + bridgeEq) union (formalTypEquations(
          pd.domain,
          ctx
        )).union(outerEqs)
      case pd: FuncTyp[u, v] =>
        val coeff = Coeff(tg.piNode)
        val boat  = nextVar(pd.dom, ctx.variables)
        val isle  = tg.piIsle(pd.domain)
        val eqs   = formalTypEquations(pd.codom, ctx.addVariable(boat))
        val isleEqs =
          eqs.map(_.mapVars(InIsle.variableMap(boat, isle)))
        val outerEqs =
          eqs.filterNot(DerivedEquations.equationDepends(boat, _))
        val bridgeEq = EquationNode(
          FinalVal(Elem(pd, Typs)),
          coeff * finalProb(pd.domain, Typs) * FinalVal(
            InIsle(Elem(pd.codom, isle.islandOutput(boat)), boat, isle)
          )
        )
        val initVarElems = eqs
          .flatMap { (eq) =>
            Expression.varVals(eq.rhs) union Expression.varVals(eq.lhs)
          }
          .collect {
            case FinalVal(Elem(el: Term, Terms)) if !el.dependsOn(boat) =>
              Elem(el, Terms): Elem[_]
            case FinalVal(Elem(el: Typ[Term], Typs)) if !el.dependsOn(boat) =>
              Elem(el, Typs): Elem[_]
            case FinalVal(Elem(el: ExstFunc, Funcs))
                if !el.func.dependsOn(boat) =>
              Elem(el, Funcs): Elem[_]
            case FinalVal(Elem(el: ExstFunc, TypFamilies))
                if !el.func.dependsOn(boat) =>
              Elem(el, TypFamilies): Elem[_]
          } union (Set(Elem(pd.codom, Typs))
          .filter(_.element.indepOf(boat))
          .map(t => t: Elem[_])) union typOpt(boat)
          .map(typ => Elem(typ, Typs))
          .toSet + Elem(boat, Terms)
        val isleIn: Set[EquationNode] =
          initVarElems.map { el =>
            val rhs =
              scaleValue(boat, el)
            EquationNode(
              InitialVal(InIsle(el, boat, isle)),
              rhs
            )
          }
        val initInIsle = initEquations(initVarElems.map(FinalVal(_)))
          .map(_.mapVars(InIsle.variableMap(boat, isle)))
        (isleIn
          .union(isleEqs)
          .union(initInIsle) + bridgeEq) union formalTypEquations(
          pd.domain,
          ctx
        ).union(outerEqs)

      case pd: SigmaTyp[u, v] =>
        val coeff = Coeff(tg.sigmaNode)
        val boat  = pd.fib.variable
        val isle  = tg.sigmaIsle(pd.fib.dom)
        val eqs   = formalEquations(pd.fib.value, ctx.addVariable(boat))
        val isleEqs =
          eqs.map(_.mapVars(InIsle.variableMap(boat, isle)))
        val outerEqs =
          eqs.filterNot(DerivedEquations.equationDepends(boat, _))
        val bridgeEq = EquationNode(
          FinalVal(Elem(pd, Typs)),
          coeff * finalProb(pd.fib.dom, Typs) * FinalVal(
            InIsle(Elem(pd.fib.value, isle.islandOutput(boat)), boat, isle)
          )
        )
        val initVarElems = eqs
          .flatMap { (eq) =>
            Expression.varVals(eq.rhs) union Expression.varVals(eq.lhs)
          }
          .collect {
            case FinalVal(Elem(el: Term, Terms)) if !el.dependsOn(boat) =>
              Elem(el, Terms): Elem[_]
            case FinalVal(Elem(el: Typ[Term], Typs)) if !el.dependsOn(boat) =>
              Elem(el, Typs): Elem[_]
            case FinalVal(Elem(el: ExstFunc, Funcs))
                if !el.func.dependsOn(boat) =>
              Elem(el, Funcs): Elem[_]
            case FinalVal(Elem(el: ExstFunc, TypFamilies))
                if !el.func.dependsOn(boat) =>
              Elem(el, TypFamilies): Elem[_]
          } union (Set(Elem(pd.fib.value, Typs))
          .filter(_.element.indepOf(boat))
          .map(t => t: Elem[_])) union typOpt(boat)
          .map(typ => Elem(typ, Typs))
          .toSet + Elem(boat, Terms)
        val isleIn: Set[EquationNode] =
          initVarElems.map { el =>
            val rhs =
              scaleValue(boat, el)
            EquationNode(
              InitialVal(InIsle(el, boat, isle)),
              rhs
            )
          }
        val initInIsle = initEquations(initVarElems.map(FinalVal(_)))
          .map(_.mapVars(InIsle.variableMap(boat, isle)))
        (isleIn
          .union(isleEqs)
          .union(initInIsle) + bridgeEq) union (formalEquations(
          pd.fib.dom,
          ctx
        )).union(outerEqs)
      case pd: ProdTyp[u, v] =>
        val coeff = Coeff(tg.sigmaIsle(pd.first))
        val x     = pd.first
        val isle  = tg.sigmaIsle(pd.first)
        Set(
          EquationNode(
            FinalVal(Elem(pd, Typs)),
            coeff * FinalVal(
              InIsle(Elem(pd.second, isle.islandOutput(x)), x, isle)
            )
          )
        )
      case rf: RecFunc[u, v] =>
        val direct = EquationNode(
          FinalVal(Elem(rf, Terms)),
          Coeff(tg.targetInducNode(rf.typ)) *
            finalProb(rf.typ, TargetTyps)
        )
        val offspring =
          (rf.defnData :+ rf.typ).toSet.flatMap(formalEquations(_, ctx))
        offspring + direct
      case rf: InducFuncLike[u, v] =>
        val direct = EquationNode(
          FinalVal(Elem(rf, Terms)),
          Coeff(tg.targetInducNode(rf.typ)) *
            finalProb(rf.typ, TargetTyps)
        )
        val offspring =
          (rf.defnData :+ rf.typ).toSet.flatMap(formalEquations(_, ctx))
        offspring + direct
      case i1: PlusTyp.FirstIncl[u, v] =>
        incl1Node(i1.typ).map { node =>
          EquationNode(
            finalProb(i1.value, Terms),
            Coeff(node) * finalProb(i1.value, termsWithTyp(i1.typ.first))
          )
        }.toSet
      case i2: PlusTyp.ScndIncl[u, v] =>
        incl2Node(i2.typ).map { node =>
          EquationNode(
            finalProb(i2.value, Terms),
            Coeff(node) * finalProb(i2.value, termsWithTyp(i2.typ.second))
          )
        }.toSet
      case pair @ PairTerm(first: Term, second: Term) =>
        nodeForTyp(pair.typ).map { node =>
          EquationNode(
            finalProb(pair, Terms),
            Coeff(node) * finalProb(first, termsWithTyp(first.typ)) * finalProb(
              second,
              termsWithTyp(second.typ)
            )
          )
        }.toSet
      case pair @ DepPair(first: Term, second: Term, _) =>
        nodeForTyp(pair.typ).map { node =>
          EquationNode(
            finalProb(pair, Terms),
            Coeff(node) * finalProb(first, termsWithTyp(first.typ)) * finalProb(
              second,
              termsWithTyp(second.typ)
            )
          )
        }.toSet
      case t: Term => Set()
    }
    base union initEquations(Set(FinalVal(Elem(t, Terms))))
  }

  def formalTypEquations(
      t: Typ[Term],
      ctx: Context = Context.Empty
  ): Set[EquationNode] = {
    val base: Set[EquationNode] = t match {
      case MiscAppln(fn: FuncLike[u, v], a) =>
        val f   = ExstFunc(fn)
        val lhs = finalProb(t, Typs)
        val funcSet: Set[EquationNode] = Set(
          EquationNode(
            lhs,
            Coeff(typApplnNode) * finalProb(f, Funcs) * finalProb(
              a,
              termsWithTyp(f.dom)
            )
          ),
          EquationNode(
            finalProb(a, termsWithTyp(f.dom)),
            finalProb(a, Terms) /
              FinalVal(Event(Terms, Sort.Filter[Term](WithTyp(f.dom))))
          ),
          EquationNode(
            FinalVal(Event(Terms, Sort.Filter[Term](WithTyp(f.dom)))),
            finalProb(a, Terms)
          ),
          EquationNode(
            finalProb(f, Funcs),
            finalProb(fn, Terms) /
              FinalVal(Event(Terms, Sort.Restrict(FuncOpt)))
          ),
          EquationNode(
            FinalVal(Event(Terms, Sort.Restrict(FuncOpt))),
            finalProb(fn, Terms)
          )
        )
        val typFamilySet: Set[EquationNode] = TypFamilyOpt(fn).toSet.flatMap {
          f: ExstFunc =>
            Set(
              EquationNode(
                lhs,
                Coeff(applnNode) * finalProb(f, TypFamilies) * finalProb(
                  a,
                  termsWithTyp(f.dom)
                )
              ),
              EquationNode(
                finalProb(f, TypFamilies),
                finalProb(fn, Terms) /
                  FinalVal(Event(Terms, Sort.Restrict(TypFamilyOpt)))
              ),
              EquationNode(
                FinalVal(Event(Terms, Sort.Restrict(TypFamilyOpt))),
                finalProb(fn, Terms)
              )
            )
        }
        funcSet union (typFamilySet) union (formalEquations(fn)) union (formalEquations(
          a
        ))
      case idt: IdentityTyp[u] =>
        typFuncFoldEquations(
          IdentityTyp.idFunc,
          Vector(idt.dom, idt.lhs, idt.rhs)
        )
      case idt: Refl[u] =>
        typFuncFoldEquations(IdentityTyp.reflTerm, Vector(idt.dom, idt.value))
      case pd: PiDefn[u, v] =>
        val coeff = Coeff(tg.piNode)
        val boat  = pd.variable
        val isle  = tg.piIsle(pd.domain)
        val eqs   = formalTypEquations(pd.value, ctx.addVariable(boat))
        val isleEqs =
          eqs.map(_.mapVars(InIsle.variableMap(boat, isle)))
        val outerEqs =
          eqs.filterNot(DerivedEquations.equationDepends(boat, _))
        // println(s"Outer equations size: ${outerEqs.size}")
        val bridgeEq = EquationNode(
          FinalVal(Elem(pd, Typs)),
          coeff * finalProb(pd.domain, Typs) * FinalVal(
            InIsle(Elem(pd.value, isle.islandOutput(boat)), boat, isle)
          )
        )
        val initVarElems = eqs
          .flatMap { (eq) =>
            Expression.varVals(eq.rhs) union Expression.varVals(eq.lhs)
          }
          .collect {
            case FinalVal(Elem(el: Term, Terms))
                if !el.dependsOn(pd.variable) =>
              Elem(el, Terms): Elem[_]
            case FinalVal(Elem(el: Typ[Term], Typs))
                if !el.dependsOn(pd.variable) =>
              Elem(el, Typs): Elem[_]
            case FinalVal(Elem(el: ExstFunc, Funcs))
                if !el.func.dependsOn(pd.variable) =>
              Elem(el, Funcs): Elem[_]
            case FinalVal(Elem(el: ExstFunc, TypFamilies))
                if !el.func.dependsOn(boat) =>
              Elem(el, TypFamilies): Elem[_]
          } union (Set(Elem(pd.value, Typs))
          .filter(_.element.indepOf(boat))
          .map(t => t: Elem[_])) union typOpt(boat)
          .map(typ => Elem(typ, Typs))
          .toSet + Elem(boat, Terms)
        val isleIn: Set[EquationNode] =
          initVarElems.map { el =>
            val rhs =
              scaleValue(boat, el)
            EquationNode(
              InitialVal(InIsle(el, boat, isle)),
              rhs
            )
          }
        val initInIsle = initEquations(initVarElems.map(FinalVal(_)))
          .map(_.mapVars(InIsle.variableMap(boat, isle)))
        (isleIn
          .union(isleEqs)
          .union(initInIsle) + bridgeEq) union (formalTypEquations(
          pd.domain,
          ctx
        )).union(outerEqs)
      case pd: FuncTyp[u, v] =>
        val coeff = Coeff(tg.piNode)
        val boat  = nextVar(pd.dom, ctx.variables)
        val isle  = tg.piIsle(pd.domain)
        val eqs   = formalTypEquations(pd.codom, ctx.addVariable(boat))
        val isleEqs =
          eqs.map(_.mapVars(InIsle.variableMap(boat, isle)))
        val outerEqs =
          eqs.filterNot(DerivedEquations.equationDepends(boat, _))
        val bridgeEq = EquationNode(
          FinalVal(Elem(pd, Typs)),
          coeff * finalProb(pd.domain, Typs) * FinalVal(
            InIsle(Elem(pd.codom, isle.islandOutput(boat)), boat, isle)
          )
        )
        val initVarElems = eqs
          .flatMap { (eq) =>
            Expression.varVals(eq.rhs) union Expression.varVals(eq.lhs)
          }
          .collect {
            case FinalVal(Elem(el: Term, Terms)) if !el.dependsOn(boat) =>
              Elem(el, Terms): Elem[_]
            case FinalVal(Elem(el: Typ[Term], Typs)) if !el.dependsOn(boat) =>
              Elem(el, Typs): Elem[_]
            case FinalVal(Elem(el: ExstFunc, Funcs))
                if !el.func.dependsOn(boat) =>
              Elem(el, Funcs): Elem[_]
            case FinalVal(Elem(el: ExstFunc, TypFamilies))
                if !el.func.dependsOn(boat) =>
              Elem(el, TypFamilies): Elem[_]
          } union (Set(Elem(pd.codom, Typs))
          .filter(_.element.indepOf(boat))
          .map(t => t: Elem[_])) union typOpt(boat)
          .map(typ => Elem(typ, Typs))
          .toSet + Elem(boat, Terms)
        val isleIn: Set[EquationNode] =
          initVarElems.map { el =>
            val rhs =
              scaleValue(boat, el)
            EquationNode(
              InitialVal(InIsle(el, boat, isle)),
              rhs
            )
          }
        val initInIsle = initEquations(initVarElems.map(FinalVal(_)))
          .map(_.mapVars(InIsle.variableMap(boat, isle)))
        (isleIn
          .union(isleEqs)
          .union(initInIsle) + bridgeEq) union formalTypEquations(
          pd.domain,
          ctx
        ).union(outerEqs)

      case pd: SigmaTyp[u, v] =>
        val coeff = Coeff(tg.sigmaNode)
        val boat  = pd.fib.variable
        val isle  = tg.sigmaIsle(pd.fib.dom)
        val eqs   = formalEquations(pd.fib.value, ctx.addVariable(boat))
        val isleEqs =
          eqs.map(_.mapVars(InIsle.variableMap(boat, isle)))
        val outerEqs =
          eqs.filterNot(DerivedEquations.equationDepends(boat, _))
        val bridgeEq = EquationNode(
          FinalVal(Elem(pd, Typs)),
          coeff * finalProb(pd.fib.dom, Typs) * FinalVal(
            InIsle(Elem(pd.fib.value, isle.islandOutput(boat)), boat, isle)
          )
        )
        val initVarElems = eqs
          .flatMap { (eq) =>
            Expression.varVals(eq.rhs) union Expression.varVals(eq.lhs)
          }
          .collect {
            case FinalVal(Elem(el: Term, Terms)) if !el.dependsOn(boat) =>
              Elem(el, Terms): Elem[_]
            case FinalVal(Elem(el: Typ[Term], Typs)) if !el.dependsOn(boat) =>
              Elem(el, Typs): Elem[_]
            case FinalVal(Elem(el: ExstFunc, Funcs))
                if !el.func.dependsOn(boat) =>
              Elem(el, Funcs): Elem[_]
            case FinalVal(Elem(el: ExstFunc, TypFamilies))
                if !el.func.dependsOn(boat) =>
              Elem(el, TypFamilies): Elem[_]
          } union (Set(Elem(pd.fib.value, Typs))
          .filter(_.element.indepOf(boat))
          .map(t => t: Elem[_])) union typOpt(boat)
          .map(typ => Elem(typ, Typs))
          .toSet + Elem(boat, Terms)
        val isleIn: Set[EquationNode] =
          initVarElems.map { el =>
            val rhs =
              scaleValue(boat, el)
            EquationNode(
              InitialVal(InIsle(el, boat, isle)),
              rhs
            )
          }
        val initInIsle = initEquations(initVarElems.map(FinalVal(_)))
          .map(_.mapVars(InIsle.variableMap(boat, isle)))
        (isleIn
          .union(isleEqs)
          .union(initInIsle) + bridgeEq) union (formalEquations(
          pd.fib.dom,
          ctx
        )).union(outerEqs)
      case pd: ProdTyp[u, v] =>
        val coeff = Coeff(tg.sigmaIsle(pd.first))
        val x     = pd.first
        val isle  = tg.sigmaIsle(pd.first)
        Set(
          EquationNode(
            FinalVal(Elem(pd, Typs)),
            coeff * FinalVal(
              InIsle(Elem(pd.second, isle.islandOutput(x)), x, isle)
            )
          )
        )
      case rf: RecFunc[u, v] =>
        val direct = EquationNode(
          FinalVal(Elem(rf, Typs)),
          Coeff(tg.targetInducNode(rf.typ)) *
            finalProb(rf.typ, TargetTyps)
        )
        val offspring =
          (rf.defnData :+ rf.typ).toSet.flatMap(formalEquations(_, ctx))
        offspring + direct
      case rf: InducFuncLike[u, v] =>
        val direct = EquationNode(
          FinalVal(Elem(rf, Typs)),
          Coeff(tg.targetInducNode(rf.typ)) *
            finalProb(rf.typ, TargetTyps)
        )
        val offspring =
          (rf.defnData :+ rf.typ).toSet.flatMap(formalEquations(_, ctx))
        offspring + direct
      case t: Term => Set()
    }
    base union initEquations(Set(FinalVal(Elem(t, Typs))))
  }

  def initEquations(s: Set[Expression]): Set[EquationNode] =
    s.collect {
      case FinalVal(Elem(t, rv)) =>
        EquationNode(
          finalProb(t, rv),
          Coeff(Init(rv)) * InitialVal(Elem(t, rv))
        )
    }

  def initCheck(exp: Expression) =
    Expression.atoms(exp).exists {
      case InitialVal(Elem(_, rv)) =>
        Set[RandomVar[_]](Terms, Typs, InducDefns, Goals).contains(rv)
      case _ => true
    }

  def initPurge(s: Set[EquationNode]) =
    s.filterNot(eq => initCheck(eq.rhs))

  def termStateElems(ts: TermState): Set[Elem[_]] =
    ts.terms.support.map { x =>
      Elem(x, Terms): Elem[_]
    } union
      ts.typs.support.map { x =>
        Elem(x, Typs): Elem[_]
      } union
      ts.typs.support.map { x =>
        Elem(x, TargetTyps): Elem[_]
      } union
      ts.terms.support.flatMap(ExstFunc.opt).map { x =>
        Elem(x, Funcs): Elem[_]
      } union
      ts.terms.condMap(TypFamilyOpt).support.map { x =>
        Elem(x, TypFamilies): Elem[_]
      } union
      ts.terms.support.map { x =>
        Elem(x, termsWithTyp(x.typ)): Elem[_]
      } union
      ts.terms.support.flatMap(ExstFunc.opt).map { x =>
        Elem(x, funcsWithDomain(x.dom)): Elem[_]
      }

  def termStateInit(ts: TermState): Set[EquationNode] =
    termStateElems(ts).par.map {
      case Elem(t, rv) =>
        EquationNode(
          finalProb(t, rv),
          Coeff(Init(rv)) * InitialVal(Elem(t, rv))
        )
    }.seq

  def termStateElemVec(ts: TermState): Vector[Elem[_]] =
    ts.terms.supp.distinct.map { x =>
      Elem(x, Terms): Elem[_]
    } ++
      ts.typs.supp.distinct.map { x =>
        Elem(x, Typs): Elem[_]
      } ++
      ts.typs.supp.distinct.map { x =>
        Elem(x, TargetTyps): Elem[_]
      } ++
      ts.terms.supp.distinct.flatMap(ExstFunc.opt).map { x =>
        Elem(x, Funcs): Elem[_]
      } ++
      ts.terms.condMap(TypFamilyOpt).supp.distinct.map { x =>
        Elem(x, TypFamilies): Elem[_]
      } ++
      ts.terms.supp.distinct.map { x =>
        Elem(x, termsWithTyp(x.typ)): Elem[_]
      } ++
      ts.terms.supp.distinct.flatMap(ExstFunc.opt).map { x =>
        Elem(x, funcsWithDomain(x.dom)): Elem[_]
      }

  def termStateElemPar(
      ts: TermState
  ): scala.collection.parallel.ParIterable[GeneratorVariables.Elem[_]] =
    ts.termDistMap.keys.map { x =>
      Elem(x, Terms): Elem[_]
    } ++
      ts.typDistMap.keys.map { x =>
        Elem(x, Typs): Elem[_]
      } ++
      ts.typDistMap.keys.map { x =>
        Elem(x, TargetTyps): Elem[_]
      } ++
      ts.funcDistMap.keys.map { x =>
        Elem(x, Funcs): Elem[_]
      } ++
      ts.typFamilyDistMap.keys.map { x =>
        Elem(x, TypFamilies): Elem[_]
      } ++
      ts.termDistMap.keys.map { x =>
        Elem(x, termsWithTyp(x.typ)): Elem[_]
      } ++
      ts.termDistMap.keys.flatMap(ExstFunc.opt).map { x =>
        Elem(x, funcsWithDomain(x.dom)): Elem[_]
      }

  def termStateInitMap(ts: TermState): ParMap[Expression, EquationNode] =
    termStateElemPar(ts).map {
      case Elem(t, rv) =>
        finalProb(t, rv) -> EquationNode(
          finalProb(t, rv),
          Coeff(Init(rv)) * InitialVal(Elem(t, rv))
        )
    }.toMap
}

object DE extends DerivedEquations()

object DerivedEquations {
  import shapeless._
  def elemDepends(x: Term, el: Elem[_]): Boolean = el match {
    case Elem(a: Term, TermsWithTyp(typ: Typ[u])) =>
      a.dependsOn(x) || typ.dependsOn(x)
    case Elem(a: Term, _) => a.dependsOn(x)
    case Elem(f: ExstFunc, RandomVar.AtCoord(_, (dom: Typ[Term]) :: HNil)) =>
      f.func.dependsOn(x) || dom.dependsOn(x)
    case Elem(f: ExstFunc, _) => f.func.dependsOn(x)
    case Elem(ind: ExstInducDefn, _) =>
      ind.typFamily.dependsOn(x) || ind.intros
        .exists(_.dependsOn(x)) || ind.ind.constants.exists(_.dependsOn(x))
    case Elem(ind: ExstInducStrucs, _) => ind.constants.exists(_.dependsOn(x))
  }

  def sortDepends(x: Term, sort: Sort[_, _]) = sort match {
    case All() => false
    case filter: Filter[u] =>
      filter.pred match {
        case WithTyp(typ) =>
          typ.dependsOn(x)
        case FuncWithDomFilter(dom) =>
          dom.dependsOn(x)
        case _ => false
      }
    case restrict: Restrict[u, v] =>
      restrict.optMap match {
        case FuncWithDom(dom) => dom.dependsOn(x)
        case _                => false
      }
  }

  def varDepends(x: Term, v: GeneratorVariables.Variable[_]): Boolean =
    v match {
      case Event(base, sort) => sortDepends(x, sort)
      case InIsle(isleVar, boat: Term, isle) =>
        boat.dependsOn(x) || varDepends(x, isleVar)
      case InIsle(isleVar, boat, isle)   => varDepends(x, isleVar)
      case PairEvent(base1, base2, sort) => sortDepends(x, sort)
      case el @ Elem(element, randomVar) => elemDepends(x, el)
    }

  def equationDepends(x: Term, eqn: EquationNode): Boolean =
    (varVals(eqn.lhs) union (varVals(eqn.rhs)))
      .map(_.variable)
      .exists(varDepends(x, _))
}
