package llvmil

import Types._
import ILInstructions._
import OperationChains._

object AbstractILInstructions {
  case class Context(prog: Program, cls: Option[Class], fnc: Function, nameGen: () => String)

  trait AbstractILInstruction {
    def apply(ctx: Context): List[ILInstruction]
  }
  abstract class AbstractILOperation(val retType: Type) {
    def apply(to: Identifier): AbstractILInstruction
  }

  case class AbstractAssign(
    to: Identifier,
    rhs: AbstractILOperation
  ) extends AbstractILInstruction {
    def apply(ctx: Context): List[ILInstruction] = rhs(to)(ctx)
  }

  object OOP {
    case class SetVptr(of: Identifier) extends AbstractILInstruction {
      def apply(ctx: Context): List[ILInstruction] = {
        val (cls, castLis, withType) = resolveReferenceThis(of, ctx)
        val vtGlobal = "%s%s".format(Prefixes.vtable, cls.className)

        val vtStore =
          GetElementPtr(withType, List(Const(0), Const(0))) +>
        ( Store(Global(TPointer(cls.vTableType), vtGlobal), _) )

        castLis.map(_(ctx)).flatten ::: vtStore(ctx.nameGen).map(_(ctx)).flatten
      }
    }

    case class AccessField(
      obj: Identifier, name: String, tpe: Type
    ) extends AbstractILOperation(TPointer(tpe)) {
      def apply(to: Identifier): AbstractILInstruction = (ctx: Context) => {
        val (cls, castLis, withType) = resolveReferenceThis(obj, ctx)

        val index = cls.allFields.indexWhere(_._2 == name) + 1
        val lookup =
          Assign(to, GetElementPtr(withType, List(Const(0), Const(index))))

        castLis.map(_(ctx)).flatten ::: lookup(ctx)
      }
    }

    case class AccessArray(
      arr: Identifier, index: Identifier
    ) extends AbstractILOperation({
      val TPointer(TArray(_, tpe)) = arr.retType
      tpe
    }) {
      def apply(to: Identifier): AbstractILInstruction = (ctx: Context) => {
        val access =
          Assign(to, GetElementPtr(arr, List(Const(0), index)))

        access(ctx)
      }
    }

    case class VirtualResolve(
      obj: Identifier, name: String, args: List[Type], retTpe: Type
    ) extends AbstractILOperation(TPointer(TFunction(TThis :: args, retTpe))) {
      def apply(to: Identifier): AbstractILInstruction = (ctx: Context) => {
        val (cls, castLis, withType) = resolveReferenceThis(obj, ctx)

        val functionIndex = cls.vTable.lastIndexWhere({ case (_, fnc) =>
          fnc.name == name && fnc.retTpe == retTpe && fnc.args.map(_._1).tail == args
        })
        val functionType = cls.vTable(functionIndex)._2.functionType

        val lookup =
          GetElementPtr(withType, List(Const(0), Const(0))) ++
        ( Load(_) ) ++
        ( GetElementPtr(_, List(Const(0), Const(functionIndex))) ) ++
        ( Load(_) ) +>
        ( fnc => Assign(to, Bitcast(TPointer(functionType), fnc)) )

        castLis.map(_(ctx)).flatten ::: lookup(ctx.nameGen).map(_(ctx)).flatten
      }
    }

    case class SizeOf(
      obj: TReference
    ) extends AbstractILOperation(TInt) {
      def apply(to: Identifier): AbstractILInstruction = (ctx: Context) => {
        val TReference(className) = obj
        val cls = ctx.prog.classes(className)
        val ptrType = TPointer(cls.classType)

        val lookup =
          Bitcast(ptrType, Null) ++
        ( GetElementPtr(_, List(Const(1))) ) +>
        ( size =>  Assign(to, PtrToInt(TInt, size)) )

        lookup(ctx.nameGen).map(_(ctx)).flatten
      }
    }

    private def resolveReferenceThis(obj: Identifier, ctx: Context) = {
      val className = obj match {
        case This => ctx.cls.get.className
        case _ => obj.retType match {
          case TReference(cn) => cn
          case _ => sys.error("Can only derive class from References and This.")
        }
      }

      val cls = ctx.prog.classes(className)
      val ptrType = TPointer(cls.classType)

      val (castLis, withType) = obj match {
        case Local(_, nme) => (Nil, Local(ptrType, nme))
        case Global(_, nme) => (Nil, Global(ptrType, nme))
        case This => Bitcast(ptrType, Local(TThis, "this"))(ctx.nameGen)

        case _ => sys.error("Can only access Fields of References and This.")
      }

      (cls, castLis, withType)
    }
  }
}
