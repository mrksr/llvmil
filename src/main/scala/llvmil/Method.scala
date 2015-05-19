package llvmil

import Types._
import ILInstructions._

import scala.collection.mutable

class Method private[llvmil]( val name: String,
                              val args: List[(Type, String)],
                              val retTpe: Type,
                              val scp: StringConstantPool
                            ) {
  var instructions: mutable.ListBuffer[ILInstruction] = mutable.ListBuffer.empty

  def <<(ilGen: ILInstructionGenerator): Method = {
    ilGen(this)
  }

  def <<(il: ILInstruction): Method = {
    instructions += il

    this
  }

  private var labelCounts = new mutable.HashMap[String, Int]
  def getFreshLabel(prefix: String): String = {
    val postfix: Int = labelCounts.getOrElse(prefix, {
      labelCounts(prefix) = 0
      0
    })
    val name = "%s.%d".format(prefix, postfix)
    labelCounts(prefix) = postfix + 1
    name
  }

  private var nameCounts = new mutable.HashMap[String, Int]
  def getFreshLocal(prefix: String): Local = {
    val postfix: Int = nameCounts.getOrElse(prefix, {
      nameCounts(prefix) = 0
      0
    })
    val name = "%s.%d".format(prefix, postfix)
    nameCounts(prefix) = postfix + 1
    Local(name)
  }
}
