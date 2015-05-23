package llvmil

import Types._
import ILInstructions._
import scala.language.implicitConversions

object AbstractILInstructions {
  case class Context(prog: Program, cls: Option[Class], fnc: Function, nameGen: () => String)
  trait AbstractILInstruction {
    def apply(ctx: Context): List[ILInstruction]
  }
  case class AbstractAssign(to: Identifier, rhs: AbstractILOperation) extends AbstractILInstruction {
    def apply(ctx: Context): List[ILInstruction] = rhs(to)(ctx)
  }

  abstract class AbstractILOperation(val retType: Type) {
    def apply(id: Identifier): AbstractILInstruction
  }
  case class AccessField(
    obj: Identifier, name: String, tpe: Type
  ) extends AbstractILOperation(tpe) {
    def apply(id: Identifier): AbstractILInstruction = (ctx: Context) => {
      val className = obj.retType match {
        case TReference(cn) => cn
        case TThis => ctx.cls.get.className

        case _ => ???
      }
      val cls = ctx.prog.classes(className)
      val ptrType = TPointer(cls.classType)
      val withType = id match {
        case Local(_, nme) => Local(ptrType, nme)
        case Global(_, nme) => Global(ptrType, nme)

        case _ => ???
      }
      val index = cls.allFields.indexWhere(_._2 == name) + 1

      val lookup =
        Assign(id, GetElementPtr(withType, List(Const(0), Const(index))))

      lookup(ctx)
    }
  }

  case class AccessArray(
    arr: Identifier, index: Identifier
  ) extends AbstractILOperation({
    val TPointer(TArray(_, tpe)) = arr.retType
    tpe
  }) {
    def apply(id: Identifier): AbstractILInstruction = (ctx: Context) => {
      val access =
        Assign(id, GetElementPtr(arr, List(Const(0), index)))

      access(ctx)
    }
  }

  case class VirtualResolve(
    obj: Identifier, name: String, args: List[Type], retTpe: Type
  ) extends AbstractILOperation(TFunction(TThis :: args, retTpe)) {
    def apply(id: Identifier): AbstractILInstruction = (ctx: Context) => {
      val className = obj.retType match {
        case TReference(cn) => cn
        case TThis => ctx.cls.get.className

        case _ => ???
      }
      val cls = ctx.prog.classes(className)
      val ptrType = TPointer(cls.classType)
      val withType = id match {
        case Local(_, nme) => Local(ptrType, nme)
        case Global(_, nme) => Global(ptrType, nme)

        case _ => ???
      }

      val functionIndex = cls.vTable.lastIndexWhere({ fnc =>
        fnc.name == name && fnc.retTpe == retTpe && fnc.args.map(_._1).tail == args
      })
      val functionType = cls.vTable(functionIndex).functionType

      val lookup =
        GetElementPtr(withType, List(Const(0), Const(0))) ++
        ( Load(_) ) ++
        ( GetElementPtr(_, List(Const(0), Const(functionIndex))) ) ++
        ( Load(_) ) +>
        ( fnc => Assign(id, Bitcast(TPointer(functionType), fnc)) )

      lookup(ctx.nameGen).map(_(ctx)).flatten
    }
  }

  // TODO(mrksr): Constructor
  case class SizeOf(
    obj: TReference
  ) extends AbstractILOperation(TInt) {
    def apply(id: Identifier): AbstractILInstruction = (ctx: Context) => {
      val TReference(className) = obj
      val cls = ctx.prog.classes(className)
      val ptrType = TPointer(cls.classType)

      val lookup =
        GetElementPtr(Bitcast(ptrType, Null), List(Const(1))) +>
        ( size =>  Assign(id, PtrToInt(TInt, size)) )

      lookup(ctx.nameGen).map(_(ctx)).flatten
    }
  }

  type OpenILOperationPipeline = (() => String) => (List[AbstractILInstruction], Identifier)
  type ClosedILOperationPipeline = (() => String) => List[AbstractILInstruction]
  sealed case class OpenILOperationChain private[AbstractILInstructions](
    pipe: OpenILOperationPipeline
  ) {
    def +>(rhs: Identifier => AbstractILInstruction) =
      ClosedILOperationChain((nameGen: () => String) => {
        val (li, id) = pipe(nameGen)
        li :+ rhs(id)
      })

    def ++(rhs: Identifier => AbstractILOperation) =
      OpenILOperationChain((nameGen: () => String) => {
        val (li, id) = pipe(nameGen)
        val op = rhs(id)

        val next = Local(op.retType, nameGen())
        (li :+ AbstractAssign(next, rhs(id)), next)
      })

    def ::(op: AbstractILOperation) = op ::: this
    def ::(lhs: OpenILOperationChain) = lhs ::: this
    def ::(lhs: ClosedILOperationChain) = lhs ::: this
    def ::(lhs: OpenILOperationPipeline) = OpenILOperationChain(lhs) ::: this
    def ::(lhs: ClosedILOperationPipeline)(implicit dummy: DummyImplicit) =
      ClosedILOperationChain(lhs) ::: this

    private def :::(lhs: OpenILOperationChain) =
      OpenILOperationChain((nameGen: () => String) => {
        val (ls, _) = lhs.pipe(nameGen)
        val (rs, rid) = this.pipe(nameGen)
        (ls ::: rs, rid)
      })

    private def :::(lhs: ClosedILOperationChain) =
      OpenILOperationChain((nameGen: () => String) => {
        val ls = lhs.pipe(nameGen)
        val (rs, rid) = this.pipe(nameGen)
        (ls ::: rs, rid)
      })
  }

  sealed case class ClosedILOperationChain private[AbstractILInstructions](
    pipe: ClosedILOperationPipeline
  ) {
    def ::(op: AbstractILOperation) = op ::: this
    def ::(lhs: OpenILOperationChain) = lhs ::: this
    def ::(lhs: ClosedILOperationChain) = lhs ::: this
    def ::(lhs: OpenILOperationPipeline) = OpenILOperationChain(lhs) ::: this
    def ::(lhs: ClosedILOperationPipeline)(implicit dummy: DummyImplicit) =
      ClosedILOperationChain(lhs) ::: this

    private def :::(lhs: OpenILOperationChain) =
      ClosedILOperationChain((nameGen: () => String) => {
        val (ls, _) = lhs.pipe(nameGen)
        val rs = this.pipe(nameGen)
        ls ::: rs
      })

    private def :::(lhs: ClosedILOperationChain) =
      ClosedILOperationChain((nameGen: () => String) => {
        val ls = lhs.pipe(nameGen)
        val rs = this.pipe(nameGen)
        ls ::: rs
      })
  }

  implicit def singleAbsOpToChain(op: AbstractILOperation): OpenILOperationChain =
    OpenILOperationChain(((nameGen: () => String) => {
      val id = Local(op.retType, nameGen())
      (List(AbstractAssign(id, op)), id)
    }))
  implicit def chainToPipeline(pipe: OpenILOperationChain): OpenILOperationPipeline =
    pipe.pipe
  implicit def chainToPipeline(pipe: ClosedILOperationChain): ClosedILOperationPipeline =
    pipe.pipe
  implicit def singleOpToPipeline(op: AbstractILOperation): OpenILOperationPipeline =
    chainToPipeline(singleAbsOpToChain(op))
  implicit def funcToAbstractILInstruction(
    fn: Context => List[ILInstruction]
  ): AbstractILInstruction =
    new AbstractILInstruction() {
      def apply(ctx: Context): List[ILInstruction] = fn(ctx)
    }
}
