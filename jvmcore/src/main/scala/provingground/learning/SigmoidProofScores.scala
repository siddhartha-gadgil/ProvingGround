package provingground.learning

import org.tensorflow.Graph

class SigmoidProofScores[
    ParamSizes,
    TermRepSizes,
    StatRepSizes,
    TopSizes,
    ParamVars,
    TermRepVars,
    StatRepVars,
    TopVars
](
    val graph: Graph,
    ps: ParamSizes,
    trs: TermRepSizes,
    sts: StatRepSizes,
    topSizes: TopSizes,
    val batchSize: Int = 1
)(
    implicit psS: SigmoidStacker[ParamSizes, ParamVars],
    trsS: SigmoidStacker[TermRepSizes, TermRepVars],
    stsS: SigmoidStacker[StatRepSizes, StatRepVars],
    topSizS: SigmoidStacker[TopSizes, TopVars]
) extends ProofScoreLearner[ParamVars, TermRepVars, StatRepVars, TopVars] {

  lazy val fanInit: TermStatementVariables =
    TermStatementVariables.random(paramLayerDimension)
  lazy val representationDimension: Int = termRepLayers.inDim

  lazy val paramLayerDimension: Int = paramLayers.inDim

  lazy val paramLayers: TFLayers[ParamVars] = psS.build(ps)

  lazy val termRepLayers: TFLayers[TermRepVars] = trsS.build(trs)

  lazy val statementRefLayers: TFLayers[StatRepVars] = stsS.build(sts)

  lazy val preTopLayers: TFLayers[TopVars] = topSizS.build(topSizes)

}
