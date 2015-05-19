package llvmil

object Types {
  sealed trait Type

  case object TVoid extends Type
  case class TInteger(size: Int) extends Type
  case class TStruct(name: String, fields: List[Type]) extends Type
  case class TFunction(args: List[Type], retTpe: Type) extends Type
  case class TArray(length: Int, tpe: Type) extends Type
  case class TPointer(target: Type) extends Type

  val TBool = TInteger(1)
  val TInt = TInteger(32)
  val TString = TStruct("String", List(TInt, TArray(0, TInteger(8))))
  val TIntArray = TStruct("IntArray", List(TInt, TArray(0, TInt)))
}
