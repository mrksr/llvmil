package llvmil

import Types._
import ILInstructions._
import scala.language.implicitConversions

object AbstractILInstructions {
  private def resolveReferenceThis(obj: Identifier, ctx: Context) = {
      val className = obj.retType match {
        case TReference(cn) => cn
        case TThis => ctx.cls.get.className

        case _ => sys.error("Can only derive class from References and This.")
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

  case class Context(prog: Program, cls: Option[Class], fnc: Function, nameGen: () => String)
  trait AbstractILInstruction {
    def apply(ctx: Context): List[ILInstruction]
  }
  case class AbstractAssign(to: Identifier, rhs: AbstractILOperation) extends AbstractILInstruction {
    def apply(ctx: Context): List[ILInstruction] = rhs(to)(ctx)
  }
  case class SetVptr(of: Identifier) extends AbstractILInstruction {
    def apply(ctx: Context): List[ILInstruction] = {
      val (cls, castLis, withType) = resolveReferenceThis(of, ctx)
      val vtGlobal = "%s%s".format(Prefixes.vtable, cls.className)

      val vtStore =
        Bitcast(TPointer(cls.classType), Local(TThis, "this")) ++
        ( GetElementPtr( _, List(Const(0), Const(0))) ) +>
        ( Store(Global(TPointer(cls.vTableType), vtGlobal), _) )

      castLis.map(_(ctx)).flatten ::: vtStore(ctx.nameGen).map(_(ctx)).flatten
    }
  }

  abstract class AbstractILOperation(val retType: Type) {
    def apply(to: Identifier): AbstractILInstruction
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

  implicit def singleAbsInsToChain(op: AbstractILInstruction): ClosedILOperationChain =
    ClosedILOperationChain((nameGen: () => String) => {
      List(op)
    })
  implicit def singleAbsOpToChain(op: AbstractILOperation): OpenILOperationChain =
    OpenILOperationChain((nameGen: () => String) => {
      val id = Local(op.retType, nameGen())
      (List(AbstractAssign(id, op)), id)
    })
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
