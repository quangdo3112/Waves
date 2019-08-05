package com.wavesplatform.features

import com.wavesplatform.state.Blockchain
import BlockchainFeatures._
import FeatureProvider._
import com.wavesplatform.lang.ScriptEstimator
import com.wavesplatform.lang.v1.ScriptEstimatorV1
import com.wavesplatform.lang.v2.estimator.ScriptEstimatorV2

object EstimatorProvider {
  implicit class EstimatorBlockchainExt(b: Blockchain) {
    def estimator(): ScriptEstimator =
      if (b.isFeatureActivated(NewScriptEstimator)) ScriptEstimatorV2.apply
      else ScriptEstimatorV1.apply
  }
}
