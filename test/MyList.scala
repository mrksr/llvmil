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

    prog.writeToFile("myList.ll")
  }
}
