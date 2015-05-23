package llvmil

import Types._
import AbstractILInstructions._

object ILInstructions {
  sealed trait ILInstruction extends AbstractILInstruction {
    def apply(ctx: Context): List[ILInstruction] = List(this)
  }

  // Variables
  case class Assign(to: Identifier, rhs: ILOperation) extends ILInstruction
  case class Store(value: Identifier, to: Identifier) extends ILInstruction

  // Flow Control
  case class Label(name: String) extends ILInstruction
  case object RetVoid extends ILInstruction
  case class Ret(id: Identifier) extends ILInstruction
  case class BrCond(cond: Identifier, iftrue: String, iffalse: String) extends ILInstruction
  case class Br(dest: String) extends ILInstruction

  sealed abstract class ILOperation(retType: Type) extends AbstractILOperation(retType) {
    def apply(id: Identifier): AbstractILInstruction = Assign(id, this)
  }

  // Values identified by Strings of some sort.
  sealed abstract class Identifier(tpe: Type, val name: String) extends ILOperation(tpe)
  case class Local(tpe: Type, nme: String) extends Identifier(tpe, '%' + nme)
  case class Global(tpe: Type, nme: String) extends Identifier(tpe, '@' + nme)
  case class IConst(tpe: TInteger, i: Int) extends Identifier(tpe, i.toString)

  case object Null extends Identifier(TPointer(TInteger(8)), "null")
  case object This extends Identifier(TThis, "this")
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

  case class GetElementPtr(ptr: Identifier, idxs: List[Identifier]) extends ILOperation({
    val TPointer(struct) = ptr.retType
    val inner = (struct /: idxs.tail)({
      case (struct, idx) => struct match {
        case TStruct(_, inners) =>
          val IConst(_, i) = idx
          inners(i)

        case TArray(_, tpe) => tpe
        case TVTable(_, TArray(_, tpe), _) => tpe

        case _ => ???
      }
    })

    TPointer(inner)
  })

  // Functions
  case class Call(func: Identifier, args: List[Identifier]) extends ILOperation({
    val TFunction(_, ret) = func.retType
    ret
  })
}
