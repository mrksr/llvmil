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
  case class TStruct(name: Option[String], fields: List[Type]) extends Type {
    def toIL = name match {
      case Some(global) => "%%%s%s".format(Prefixes.struct, global)
      case None => "{ %s }".format(fields.map(_.toIL).mkString(", "))
    }
    def declaration = "%s = type { %s }".format(toIL, fields.map(_.toIL).mkString(", "))
  }
  case class TFunction(args: List[Type], retTpe: Type) extends Type {
    private lazy val argList = args.map(_.toIL).mkString(", ")

    def toIL = "%s (%s)".format(retTpe.toIL, argList)
    def declaration(name: String) = "declare %s @%s(%s)".format(retTpe.toIL, name, argList)
  }
  case class TArray(length: Int, tpe: Type) extends Type {
    def toIL = "[%d x %s]".format(length, tpe.toIL)
  }
  case class TVTable(name: String, tpe: TArray, funcs: List[Identifier]) extends Type {
    def toIL = tpe.toIL
    def instantiation =
      "@%s%s = constant %s [ %s ]".format(
        Prefixes.vtable, name,
        toIL,
        funcs.map(ILPrinter.identifier).mkString(", ")
      )
  }
  case class TPointer(target: Type) extends Type {
    def toIL = "%s*".format(target.toIL)
  }
  case class TReference(className: String) extends Type {
    def toIL = "%%%s%s%s*".format(Prefixes.struct, Prefixes.classType, className)
  }
  case object TVariadic extends Type {
    def toIL = "..."
  }

  val TBool = TInteger(1)
  val TChar = TInteger(8)
  val TInt = TInteger(32)
  val TThis = TPointer(TChar)
  val TString = TPointer(TStruct(Some("String"), List(TInt, TArray(0, TChar))))
  val TIntArray = TPointer(TStruct(Some("IntArray"), List(TInt, TArray(0, TInt))))
}
