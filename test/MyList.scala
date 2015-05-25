import llvmil._
import llvmil.Types._
import llvmil.ILInstructions._
import llvmil.AbstractILInstructions._

object MyList {
  def main(args: Array[String]): Unit = {
    val prog = new Program()

    val parent = prog.addClass("Object", None)
    parent.addField("id", TInt)

    locally {
      implicit val mtd = parent.addMethod("setID", List((TInt, "to")), TVoid, None)

      mtd append (
        AccessField(This, "id", TInt) +>
        ( Store(Local(TInt, "to"), _ ) ) ::
        RetVoid
      )
    }

    locally {
      implicit val mtd = parent.addMethod("getID", Nil, TInt, None)

      mtd append (
        AccessField(This, "id", TInt) ++
        ( Load(_) ) +>
        ( Ret(_) )
      )
    }


    val cls = prog.addClass("MyIntList", Some("Object"))
    cls.addField("Value", TInt)
    cls.addField("Prev", TReference("MyIntList"))
    cls.addField("Next", TReference("MyIntList"))

    locally {
      implicit val mtd = cls.addMethod("getID", Nil, TInt, Some(("getID", Nil, TInt)))

      mtd append (
        Ret(Const(42))
      )
    }

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
      import Runtime._
      implicit val main = prog.addMain()

      val lhs = main.append(Call(string_create, List(Bitcast(TPointer(TChar), Const("Hallo")))))
      val rhs = main.append(Call(string_create, List(Bitcast(TPointer(TChar), Const("Mallo")))))
      main append (
        Call(string_equals, List(lhs, rhs)) +>
        (id => CallVoid(println_bool, List(id) ) ) ::
        Ret(Const(1))
      )
    }

    prog.writeToFile("myList.ll")
  }
}
