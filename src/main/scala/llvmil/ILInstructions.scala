package llvmil

import Types._
import scala.language.implicitConversions

object ILInstructions {
  sealed trait ILInstruction

  case class Label(name: String) extends ILInstruction
  case class Assign(to: Identifier, rhs: ILOperation) extends ILInstruction
  case class Store(value: Identifier, to: Identifier) extends ILInstruction

  // Flow Control
  case object RetVoid extends ILInstruction
  case class Ret(id: Identifier) extends ILInstruction
  case class BrCond(cond: Identifier, iftrue: String, iffalse: String) extends ILInstruction
  case class Br(dest: String) extends ILInstruction

  sealed class ILOperation(val retType: Type)

  type ILOperationPipeline = (Method => Option[Identifier])
  case class ILOperationChain private[ILInstructions](pipe: ILOperationPipeline) {
    def >>(rhs: Identifier => ILInstruction) =
      ILOperationChain(((mtd: Method) => {
        pipe(mtd) match {
          case Some(id) => {
            mtd append rhs(id)
            None
          }
          case None => ???
        }
      }))

    def >|(rhs: Identifier => ILOperation) =
      ILOperationChain(((mtd: Method) => {
        pipe(mtd) match {
          case Some(id) => {
            val op = rhs(id)

            val next = Local(op.retType, mtd.getFreshName())
            mtd append Assign(id, op)
            Some(id)
          }
          case None => ???
        }
      }))

    def |:(mtd: Method) = mtd append pipe
    def |:(op: ILOperation) = op ::: this

    def :::(lhs: ILOperationChain) = ILOperationChain((mtd: Method) => {
      mtd append lhs.pipe
      mtd append this.pipe
    })
  }
  implicit def singleOpToChain(op: ILOperation): ILOperationChain =
    ILOperationChain(((mtd: Method) => {
      val id = Local(op.retType, mtd.getFreshName())
      mtd append Assign(id, op)
      Some(id)
    }))
  implicit def chainToPipeline(pipe: ILOperationChain): ILOperationPipeline = pipe.pipe

  object Const {
    def apply(s: String)(implicit mtd: Method) = mtd.scp(s)
    def apply(i: Int) = IConst(TInt, i)
  }

  // Values identified by Strings of some sort.
  sealed class Identifier(tpe: Type, val name: String) extends ILOperation(tpe)
  case class Local(tpe: Type, nme: String) extends Identifier(tpe, '%' + nme)
  case class Global(tpe: Type, nme: String) extends Identifier(tpe, '@' + nme)
  case class IConst(tpe: TInteger, i: Int) extends Identifier(tpe, i.toString)
  case class Bitcast(to: Type, id: Identifier) extends Identifier(to, id.name)

  // Arithmetics
  case class Add(lhs: Identifier, rhs: Identifier) extends ILOperation(lhs.retType)
  case class Sub(lhs: Identifier, rhs: Identifier) extends ILOperation(lhs.retType)
  case class Mul(lhs: Identifier, rhs: Identifier) extends ILOperation(lhs.retType)
  case class UDiv(lhs: Identifier, rhs: Identifier) extends ILOperation(lhs.retType)
  case class SDiv(lhs: Identifier, rhs: Identifier) extends ILOperation(lhs.retType)

  // Memory
  case class Alloca(tpe: Type) extends ILOperation(TPointer(tpe))
  case class Load(ptr: Identifier) extends ILOperation({
    val TPointer(inner) = ptr.retType
    inner
  })

  //TODO(mrksr): Implement the Type derivation
  // case class GetElementPtr(ptr: Identifier, idxs: List[Int]) extends ILOperation(TVoid)

  // OOP
  case class VirtualResolve(obj: Identifier, name: String, args: List[Type], retTpe: Type)
    extends ILOperation(TFunction(args, retTpe))
  case class AccessField(obj: Identifier, name: String, tpe: Type) extends ILOperation(tpe)
  case class Call(func: Identifier, args: List[Identifier]) extends ILOperation({
    val TFunction(_, ret) = func.retType
    ret
  })
}
