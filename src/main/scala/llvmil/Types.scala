package llvmil

import Prefixes._
import ILInstructions._

object Types {
  sealed trait Type {
    def toIL: String
  }

  case object TVoid extends Type {
    def toIL = "void"
  }
  case class TInteger(size: Int) extends Type {
    def toIL = "i%d".format(size)
  }
  case class TStruct(name: String, fields: List[Type]) extends Type {
    def toIL = "%%%s%s".format(Prefixes.struct, name)
    def declaration = "%s = { %s }".format(toIL, fields.map(_.toIL).mkString(", "))
  }
  case class TFunction(args: List[Type], retTpe: Type) extends Type {
    def toIL = "%s (%s)".format(retTpe.toIL, args.map(_.toIL).mkString(", "))
  }
  case class TArray(length: Int, tpe: Type) extends Type {
    def toIL = "[%d x %s]".format(length, tpe.toIL)
  }
  case class TVTable(name: String, tpe: TArray, funcs: List[Identifier]) extends Type {
    def toIL = "%%%s%s".format(Prefixes.vtable, name)
    def declaration =
      "%s = %s [ %s ]".format(toIL, tpe.toIL, funcs.map(ILPrinter.identifier).mkString(", "))
  }
  case class TPointer(target: Type) extends Type {
    def toIL = "%s *".format(target.toIL)
  }
  case class TReference(className: String) extends Type {
    def toIL = "%%%s%s%s *".format(Prefixes.struct, Prefixes.classType, className)
  }
  case object TThis extends Type {
    def toIL = TPointer(TInteger(8)).toIL
  }
  case object TVariadic extends Type {
    def toIL = "..."
  }

  val TBool = TInteger(1)
  val TInt = TInteger(32)
  val TString = TPointer(TStruct("String", List(TInt, TArray(0, TInteger(8)))))
  val TIntArray = TPointer(TStruct("IntArray", List(TInt, TArray(0, TInt))))
}
