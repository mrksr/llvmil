package llvmil

import Types._
import AbstractILInstructions._
import ILInstructions._

import scala.collection.mutable

class Function private[llvmil]( val name: String,
                                val args: List[(Type, String)],
                                val retTpe: Type,
                                sp: StringPool
                              ) extends ConstantPool {
  def string(const: String) = sp.string(const)

  var abstractILs: mutable.ListBuffer[AbstractILInstruction] = mutable.ListBuffer.empty
  def append(ilGen: ILOperationPipeline): Option[Identifier] = {
    val (ils, id) = ilGen(() => getFreshName())
    abstractILs ++= ils
    id
  }
  def append(il: AbstractILInstruction): Option[Identifier] = {
    abstractILs += il
    None
  }

  val functionType = TFunction(args.map(_._1), retTpe)
  def resolve(prog: Program): List[ILInstruction] =
    abstractILs.toList.map(ail => ail(prog, () => getFreshName())).flatten

  private var labelCounts = new mutable.HashMap[String, Int]
  def getFreshLabel(prefix: String): String = {
    val postfix: Int = labelCounts.getOrElse(prefix, {
      labelCounts(prefix) = 0
      0
    })
    val name = "%s%d".format(prefix, postfix)
    labelCounts(prefix) = postfix + 1
    name
  }

  private var nameCounts = new mutable.HashMap[String, Int]
  def getFreshName(prefix: String = ""): String = {
    val postfix: Int = nameCounts.getOrElse(prefix, {
      nameCounts(prefix) = 0
      0
    })
    val name = "%s%d".format(prefix, postfix)
    nameCounts(prefix) = postfix + 1
    name
  }
}
