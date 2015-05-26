import llvmil._
import llvmil.Types._
import llvmil.ILInstructions._
import llvmil.AbstractILInstructions.OOP._
import llvmil.OperationChains._

object Test {
  def main(args: Array[String]): Unit = {
    val prog = new Program()

    val cls2 = prog.addClass("Wurst", None)
    cls2.addField("ThisNum", TInt)

    val cls = prog.addClass("Hallo", None)
    cls.addField("MyNum", TInt)
    cls.addField("MyBool", TBool)

    implicit val mtd = cls.addMethod("Test", List((TInt, "x"), (TInt, "y")), TBool, None)
    mtd append (
      Add(Const(3), Const("Hallo!")) ::
      Sub(Const("Wurst"), Const(123)) ::
      AccessField(Local(TReference("Wurst"), "wurst"), "ThisNum", TInt) +>
      (Store(Const(5), _))
    )

    mtd append (
      Alloca(TArray(5, TBool)) ++
      ( AccessArray(_, Const(3)) ) +>
      ( arr => Assign(Local(TBool, "arrayElement"), Add(arr, Const(2))) )
    )

    val rslv = mtd append (
      VirtualResolve(Local(TReference("Hallo"), "hallo"), "Wurst", List(TInt, TInt), TInt) ++
      ( Call(_, List(This, Const(1), Const(17))) )
    )

    mtd.append (
      SizeOf(TReference("Wurst")) ::
      VirtualResolve(This, "Test", List(TInt, TInt), TBool) ++
      ( Call (_, List(This, Const(1), Const(4))) )
    )

    val mtd2 = cls.addMethod("Wurst", List((TInt, "a"), (TInt, "b")), TInt, None)
    mtd2 append (
      Icmp(Comparisons.ne, Const(1), Const(3)) +>
      ( BrCond(_, "left", "right") ) ::
      Label("left") ::
      Ret(Const("äßüöµ@»«¢")(mtd2)) ::
      Label("right") ::
      Ret(Const(0))
    )

    prog.writeToFile("test.ll")
  }
}
