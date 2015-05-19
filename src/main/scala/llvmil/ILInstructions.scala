package llvmil

import Types._

object ILInstructions {
  sealed trait ILInstruction
  type ILInstructionGenerator = (Method => Method)

  object Const {
    def apply(s: String): ILInstructionGenerator = ((mtd: Method) => {
      mtd << mtd.scp(s)
      mtd
    })

    def apply(i: Int) = IConst(i)
  }

  // Internals
  case class Label(name: String) extends ILInstruction
  case class Assign(to: Identifier, rhs: ILOperation) extends ILInstruction

  // Flow Control
  case object RetVoid extends ILInstruction
  case class Ret(id: Identifier) extends ILInstruction
  case class BrCond(cond: Identifier, iftrue: String, iffalse: String) extends ILInstruction
  case class Br(dest: String) extends ILInstruction

  sealed trait ILOperation
  // Values identified by Strings of some sort.
  sealed trait Identifier(tpe: Type, name: String) extends ILOperation
  case class Local(tpe: Type, name: String) extends Value(tpe, '%' + name)
  case class Global(tpe: Type, name: String) extends Value(tpe, '@' + name)
  case class IConst(i: Int) extends Value(TInt, i.toString)
  case class Bitcast(to: Type, id: Identifier) extends Value(to, id.name)

  // Arithmetics
  case class Add(lhs: Identifier, rhs: Identifier) extends ILOperation
  case class Sub(lhs: Identifier, rhs: Identifier) extends ILOperation
  case class Mul(lhs: Identifier, rhs: Identifier) extends ILOperation
  case class UDiv(lhs: Identifier, rhs: Identifier) extends ILOperation
  case class SDiv(lhs: Identifier, rhs: Identifier) extends ILOperation

  // Memory
  case class Alloca(tpe: Type) extends ILOperation
  case class Load(ptr: Identifier) extends ILOperation
  case class Store(value: Identifier, to: Identifier) extends ILOperation
  case class GetElementPtr(ptr: Identifier, idxs: List[Int]) extends ILOperation

  // OOP
  case class VirtualResolve(obj: Identifier, name: String, args: List[Type], retTpe: Type) extends ILOperation
  case class AccessField(obj: Identifier, name: String) ILOperation
  case class Call(func: Identifier, args: List[Identifier]) extends ILInstruction

}
