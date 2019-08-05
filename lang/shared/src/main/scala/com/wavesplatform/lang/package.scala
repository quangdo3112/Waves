package com.wavesplatform

import cats.Eval
import cats.data.EitherT
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms.EXPR
import monix.eval.Coeval

package object lang {

  type ExecutionError           = String
  type ExecutionLog             = String
  type TrampolinedExecResult[T] = EitherT[Eval, ExecutionError, T]

  type ScriptEstimator = (Set[String], Map[FunctionHeader, Coeval[Long]], EXPR) => Either[String, Long]
}
