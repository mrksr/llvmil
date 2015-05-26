package llvmil

import ILInstructions._
import AbstractILInstructions._
import scala.language.implicitConversions

object OperationChains {
  type OpenILOperationPipeline = (() => String) => (List[AbstractILInstruction], Identifier)
  type ClosedILOperationPipeline = (() => String) => List[AbstractILInstruction]
  sealed case class OpenILOperationChain private[llvmil](
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

  sealed case class ClosedILOperationChain private[llvmil](
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

  // NOTE(mrksr): These conversions should probablystay here to be visible to everyone
  // using IL instructions.
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
