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

  case class Label(name: String) extends ILInstruction

  sealed trait Identifier extends ILInstruction
  case class Local(name: String) extends Identifier
  case class Global(name: String) extends Identifier
  case class IConst(i: Int) extends Identifier

  // NOTE(mrksr): Is this sensible?
  case class Assign(id: Identifier, rhs: ILInstruction)

  case class Add(tpe: Type, lhs: Identifier, rhs: Identifier)
  case class Sub(tpe: Type, lhs: Identifier, rhs: Identifier)
  case class Mul(tpe: Type, lhs: Identifier, rhs: Identifier)
  case class UDiv(tpe: Type, lhs: Identifier, rhs: Identifier)
  case class SDiv(tpe: Type, lhs: Identifier, rhs: Identifier)

  case object RetVoid extends ILInstruction
  case class Ret(tpe: Type, id: Identifier) extends ILInstruction
  case class BrCond(cond: Identifier, iftrue: String, iffalse: String) extends ILInstruction
  case class Br(dest: String) extends ILInstruction

  // NOTE(mrksr): Need Identifier for assigment here?
  case class VirtualResolve(obj: (Type, Identifier), name: String, args: List[Type], retTpe: Type) extends ILInstruction
  case class AccessField(obj: (Type, Identifier), name: String)

  case class Call(tpe: Type, fnptr: Identifier, args: List[(Type, Identifier)]) extends ILInstruction

  // FIXME(mrksr): More Stuff
}
