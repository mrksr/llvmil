package llvmil

import ILInstructions._
import Types._
import Prefixes._
import scala.collection.mutable.HashMap

trait ConstantPool {
  def string(const: String): Identifier
}

private[llvmil] class StringPool extends ConstantPool {
  private val strings: HashMap[String, Identifier] = HashMap()
  private val prefix = Prefixes.string
  private var nextIndex: Int = 0

  def string(const: String): Identifier = strings.getOrElse(const, {
    val vlue = Global(TString, "%s%d".format(prefix,nextIndex))
    nextIndex += 1
    strings += (const -> vlue)

    vlue
  })
}
