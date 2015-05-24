import llvmil._
import llvmil.Types._
import llvmil.ILInstructions._
import llvmil.AbstractILInstructions._

object MyList {
  def main(args: Array[String]): Unit = {
    val prog = new Program()

    val cls = prog.addClass("MyIntList", None)
    cls.addField("Value", TInt)
    cls.addField("Prev", TReference("MyIntList"))
    cls.addField("Next", TReference("MyIntList"))
    cls.addDefaultConstructor() append (
      RetVoid
    )

    locally {
      implicit val mtd = cls.addMethod("setVal", List((TInt, "to")), TVoid, None)

      mtd append (
        AccessField(This, "Value", TInt) +>
        ( Store(Local(TInt, "to"), _ ) ) ::
        RetVoid
      )
    }

    locally {
      implicit val mtd = cls.addMethod("getVal", List(), TInt, None)

      mtd append (
        AccessField(This, "Value", TInt) ++
        ( Load(_) ) +>
        ( Ret(_) )
      )
    }

    locally {
      implicit val mtd = cls.addMethod("nextVal", List(), TInt, None)

      mtd append (
        AccessField(This, "Next", TReference("MyIntList")) ++
        ( Load(_) ) ++
        ( VirtualResolve(_, "getVal", List(), TInt) ) ++
        ( Call(_, List(This)) ) +>
        ( Ret(_) )
      )
    }

    locally {
      val string_create =
        Global(TPointer(TFunction(List(TPointer(TInteger(8))), TString)), "string_create")
      val string_equals =
        Global(TPointer(TFunction(List(TString, TString), TBool)), "string_equals")
      implicit val main = prog.addMain()

      val lhs = main.append(Call(string_create, List(Bitcast(TPointer(TChar), Const("Hallo")))))
      val rhs = main.append(Call(string_create, List(Bitcast(TPointer(TChar), Const("Mallo")))))
      main append (
        Call(string_equals, List(lhs, rhs)) ::
        Sub(Const(1), Const(2)) ::
        Ret(Const(1))
      )
    }

    prog.writeToFile("myList.ll")
  }
}
