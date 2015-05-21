package llvmil

import Types._
import scala.language.implicitConversions

object ILInstructions {
  sealed trait ILInstruction

  // Variables
  case class Assign(to: Identifier, rhs: ILOperation) extends ILInstruction
  case class Store(value: Identifier, to: Identifier) extends ILInstruction

  // Flow Control
  case class Label(name: String) extends ILInstruction
  case object RetVoid extends ILInstruction
  case class Ret(id: Identifier) extends ILInstruction
  case class BrCond(cond: Identifier, iftrue: String, iffalse: String) extends ILInstruction
  case class Br(dest: String) extends ILInstruction

  sealed abstract class ILOperation(val retType: Type)
  // Values identified by Strings of some sort.
  sealed abstract class Identifier(tpe: Type, val name: String) extends ILOperation(tpe)
  case class Local(tpe: Type, nme: String) extends Identifier(tpe, '%' + nme)
  case class Global(tpe: Type, nme: String) extends Identifier(tpe, '@' + nme)
  case class IConst(tpe: TInteger, i: Int) extends Identifier(tpe, i.toString)

  case class Bitcast(to: Type, id: Identifier) extends Identifier(to, id.name)
  case class PtrToInt(to: TInteger, id: Identifier) extends Identifier(to, id.name)

  object Const {
    def apply(s: String)(implicit pool: ConstantPool) = pool.string(s)
    def apply(i: Int) = IConst(TInt, i)
  }

  // Arithmetics
  case class Add(lhs: Identifier, rhs: Identifier) extends ILOperation(lhs.retType)
  case class Sub(lhs: Identifier, rhs: Identifier) extends ILOperation(lhs.retType)
  case class Mul(lhs: Identifier, rhs: Identifier) extends ILOperation(lhs.retType)
  case class Div(lhs: Identifier, rhs: Identifier) extends ILOperation(lhs.retType)

  // Comparisons
  sealed trait Comparison
  case object eq extends Comparison
  case object ne extends Comparison
  case object sgt extends Comparison
  case object sge extends Comparison
  case object slt extends Comparison
  case object sle extends Comparison
  case class Icmp(op: Comparison, lhs: Identifier, rhs: Identifier) extends ILOperation(TBool)

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

  type ILOperationPipeline = (Function => Option[Identifier])
  sealed case class ILOperationChain private[ILInstructions](pipe: ILOperationPipeline) {
    def +>(rhs: Identifier => ILInstruction): ILOperationPipeline =
      ((fnc: Function) => {
        pipe(fnc) match {
          case Some(id) => {
            fnc append rhs(id)
            None
          }
          case None => ???
        }
      })

    def ++(rhs: Identifier => ILOperation) =
      ILOperationChain(((fnc: Function) => {
        pipe(fnc) match {
          case Some(id) => {
            val op = rhs(id)

            val next = Local(op.retType, fnc.getFreshName())
            fnc append Assign(id, op)
            Some(id)
          }
          case None => ???
        }
      }))

    def ::(op: ILOperation) = op ::: this
    def ::(lhs: ILOperationChain) = lhs ::: this
    def ::(lhs: ILOperationPipeline) = ILOperationChain(lhs) ::: this

    private def :::(lhs: ILOperationChain) = ILOperationChain((fnc: Function) => {
      fnc append lhs.pipe
      fnc append this.pipe
    })
  }

  implicit def singleOpToChain(op: ILOperation): ILOperationChain =
    ILOperationChain(((fnc: Function) => {
      val id = Local(op.retType, fnc.getFreshName())
      fnc append Assign(id, op)
      Some(id)
    }))
  implicit def chainToPipeline(pipe: ILOperationChain): ILOperationPipeline =
    pipe.pipe
  implicit def singleOpToPipeline(op: ILOperation): ILOperationPipeline =
    chainToPipeline(singleOpToChain(op))
}
