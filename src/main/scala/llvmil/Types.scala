package llvmil

import Prefixes._

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
    def declaration() = "%s = { %s }".format(toIL, fields.map(_.toIL).mkString(", "))
  }
  case class TFunction(args: List[Type], retTpe: Type) extends Type {
    def toIL = "%s (%s)".format(retTpe.toIL, args.map(_.toIL).mkString(", "))
  }
  case class TArray(length: Int, tpe: Type) extends Type {
    def toIL = "[%d x %s]".format(length, tpe.toIL)
  }
  case class TPointer(target: Type) extends Type {
    def toIL = "%s *".format(target.toIL)
  }
  case object TVariadic extends Type {
    def toIL = "..."
  }

  val TBool = TInteger(1)
  val TInt = TInteger(32)
  val TString = TStruct("String", List(TInt, TArray(0, TInteger(8))))
  val TIntArray = TStruct("IntArray", List(TInt, TArray(0, TInt)))

  def TVTable(size: Int) = TArray(size, TPointer(TFunction(List(TVariadic), TVoid)))
}
