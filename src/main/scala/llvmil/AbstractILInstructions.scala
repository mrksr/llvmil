package llvmil

import Types._
import ILInstructions._
import scala.language.implicitConversions

object AbstractILInstructions {
  trait AbstractILInstruction {
    def apply(prog: Program, nameGen: () => String): List[ILInstruction]
  }
  case class AbstractAssign(to: Identifier, rhs: AbstractILOperation) extends AbstractILInstruction {
    def apply(prog: Program, nameGen: () => String): List[ILInstruction] = rhs(to)(prog, nameGen)
  }

  abstract class AbstractILOperation(val retType: Type) {
    def apply(id: Identifier): AbstractILInstruction
  }
  // case class VirtualResolve(obj: Identifier, name: String, args: List[Type], retTpe: Type)
  //   extends AbstractILOperation(TFunction(args, retTpe))
  case class AccessField(obj: Identifier, name: String, tpe: Type) extends AbstractILOperation(tpe) {
    def apply(id: Identifier): AbstractILInstruction = (prog: Program, nameGen: () => String) => {
      val TReference(className) = obj.retType
      val cls = prog.classes(className)
      val ptrType = TPointer(cls.classType)
      val withType = id match {
        case Local(_, nme) => Local(ptrType, nme)
        case Global(_, nme) => Global(ptrType, nme)

        case _ => ???
      }
      val index = cls.allFields.indexWhere(_._2 == name) + 1

      val lookup =
        GetElementPtr(withType, List(index)) +>
        (ptr => Assign(id, Load(ptr)))

      lookup(nameGen)._1.map(_(prog, nameGen)).flatten
    }
  }

  type ILOperationPipeline = ((() => String) => (List[AbstractILInstruction], Option[Identifier]))
  sealed case class ILOperationChain private[AbstractILInstructions](pipe: ILOperationPipeline) {
    def +>(rhs: Identifier => AbstractILInstruction) =
      ILOperationChain((nameGen: () => String) => {
        pipe(nameGen) match {
          case (li, Some(id)) => {
            (li :+ rhs(id), None)
          }
          case (_, None) => ???
        }
      })

    def ++(rhs: Identifier => AbstractILOperation) =
      ILOperationChain((nameGen: () => String) => {
        pipe(nameGen) match {
          case (li, Some(id)) => {
            val op = rhs(id)

            val next = Local(op.retType, nameGen())
            (li :+ AbstractAssign(next, rhs(id)), Some(next))
          }
          case (_, None) => ???
        }
      })

    def ::(op: AbstractILOperation) = op ::: this
    def ::(lhs: ILOperationChain) = lhs ::: this
    def ::(lhs: ILOperationPipeline) = ILOperationChain(lhs) ::: this

    private def :::(lhs: ILOperationChain) = ILOperationChain((nameGen: () => String) => {
      val (ls, _) = lhs.pipe(nameGen)
      val (rs, rid) = this.pipe(nameGen)
      (ls ::: rs, rid)
    })
  }

  implicit def singleAbsOpToChain(op: AbstractILOperation): ILOperationChain =
    ILOperationChain(((nameGen: () => String) => {
      val id = Local(op.retType, nameGen())
      (List(AbstractAssign(id, op)), Some(id))
    }))
  implicit def chainToPipeline(pipe: ILOperationChain): ILOperationPipeline =
    pipe.pipe
  implicit def singleOpToPipeline(op: AbstractILOperation): ILOperationPipeline =
    chainToPipeline(singleAbsOpToChain(op))
  implicit def funcToAbstractILInstruction(
    fn: (Program, () => String) => List[ILInstruction]
  ): AbstractILInstruction =
    new AbstractILInstruction() {
      def apply(prog: Program, nameGen: () => String): List[ILInstruction] = fn(prog, nameGen)
    }
}
