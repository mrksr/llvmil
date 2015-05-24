package llvmil

import Types._
import ILInstructions._

object Runtime {
  val string_alloc =
    Global(TPointer(TFunction(List(TInt), TString)), "string_alloc")

  val string_create =
    Global(TPointer(TFunction(List(TPointer(TInteger(8))), TString)), "string_create")

  val string_concat =
    Global(TPointer(TFunction(List(TString, TString), TString)), "string_concat")

  val string_equals =
    Global(TPointer(TFunction(List(TString, TString), TBool)), "string_equals")

  val array_alloc =
    Global(TPointer(TFunction(List(TInt), TIntArray)), "array_alloc")

  val println_int =
    Global(TPointer(TFunction(List(TInt), TVoid)), "println_int")

  val println_string =
    Global(TPointer(TFunction(List(TString), TVoid)), "println_string")

  val println_bool =
    Global(TPointer(TFunction(List(TBool), TVoid)), "println_bool")
}
