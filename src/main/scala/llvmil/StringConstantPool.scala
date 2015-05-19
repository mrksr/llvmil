package llvmil

import scala.collection.mutable.HashMap

class StringConstantPool {
  import ILInstructions._
  import Types._

  private val strings: HashMap[String, Identifier] = HashMap()
  private val prefix = "$SC$"
  private var nextIndex: Int = 0

  def apply(const: String): Identifier = strings.getOrElse(const, {
    val vlue = Global(TString, "%s%d".format(prefix,nextIndex))
    nextIndex += 1
    strings += (const -> vlue)

    vlue
  })
}
