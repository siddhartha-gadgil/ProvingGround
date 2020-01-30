package provingground.learning
import provingground._, HoTT._
import provingground.induction.ExstInducDefn

/**
  * Messages to be posted for autonomous/interactive running. For now a draft/strawman. Most classes will be refined.
  * The principle followed is to factorize a lot, so there can be alternative responses, including by interactions
  * and deep learning
  */
object HoTTMessages {

  case class InitState(
      ts: TermState,
      termsForProof: Option[FiniteDistribution[Term]]
  )

  case class FinalState(ts: TermState)

  case class TunedLocalProver(lp: LocalProver)

  case class GeneratedEquationNodes(eqn: Set[EquationNode])

  case class IsleNormalizedEquationNodes(eqn: Set[EquationNode])

  case class ProcessedEquationNodes(eqn: Set[EquationNode])

  case class Lemmas(lemmas: Vector[(Typ[Term], Double)])

  case class ChompResult(
      successes: Vector[StrategicProvers.Successes],
      failures: Vector[Typ[Term]],
      eqns: Set[EquationNode]
  )

  case class RepresenationMap(
      rep: Map[GeneratorVariables.Variable[_], Vector[Double]]
  )

  case class TimeLimit(duration: scala.concurrent.duration.FiniteDuration)

  case class HaltIf(condition: Unit => Boolean)

  case class Refine(scale: Double)

  case class SeekLemmas(weightPower: Double, scale: Double)

  case object TangentWithLemmas

  case object MixinLemmas

  case class OptimizeGenerators(damping: Double)

  case object GenerateTypes

  case object GenerateTerms

  case class SeekGoals(goals: FiniteDistribution[Typ[Term]])

  case class SeekGoal(goal: Typ[Term], forConsequences: Option[Set[Typ[Term]]])

  case class Consequence(premise: Typ[Term], conclusion: Typ[Term], proofMap: Term => Term){
    assert(proofMap("hyp" :: premise).typ == conclusion)
  }

  case class Contradicts(premise: Typ[Term], conclusion: Typ[Term], contraMap: Term => Term => Term){
    assert(contraMap("assume" :: premise)("also" :: conclusion).typ == Zero)
  }

  case class Proved(statement: Typ[Term], proof: Term){
    assert(proof.typ == statement)
  }

  case class Contradicted(statement: Typ[Term], disProof: Term, contra: Term => Term => Term){
    assert(contra(disProof)("assume":: statement).typ == Zero)
  }

  case class SeekInstances[U<: Term with Subs[U], V<: Term with Subs[V]](typ: Typ[U], toShow: U => Typ[V])

  case class Instance[U<: Term with Subs[U]](term: U, typ: Typ[U]){
    assert(term.typ == typ)
  }

  case class FromAll(typs: Vector[Typ[Term]], conclusion: Typ[Term])

  case class FromAny(typs: Vector[Typ[Term]], conclusion: Typ[Term], exhaustive: Boolean)

  case class AddVariable(typ: Typ[Term])

  case class ConsiderTerms(terms: FiniteDistribution[Term])

  case class ConsiderInductiveTypes(inds: FiniteDistribution[ExstInducDefn])

  case class SplitFocus(number: Int, scale: Double)
}
