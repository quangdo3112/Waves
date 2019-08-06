package com.wavesplatform.network.message

import com.wavesplatform.lang.ScriptEstimator

import scala.reflect.ClassTag
import scala.util.Try

abstract class MessageSpec[Content <: AnyRef](implicit contentCt: ClassTag[Content]) {
  val contentClass: Class[_] = contentCt.runtimeClass
  val messageCode: Message.MessageCode
  final val messageName: String = """Spec\$$""".r.replaceAllIn(getClass.getSimpleName, "")

  def maxLength: Int

  def deserializeData(bytes: Array[Byte], estimator: ScriptEstimator): Try[Content]

  def serializeData(data: Content): Array[Byte]

  override def toString: String = s"MessageSpec($messageCode: $messageName)"
}
