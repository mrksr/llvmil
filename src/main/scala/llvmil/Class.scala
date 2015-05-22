package llvmil

import Types._
import Prefixes._
import ILInstructions._

class Class private[llvmil]( val className: String,
                             val parentName: Option[String],
                             prog: Program
                           ) {
  private type MethodInfo = (String, List[Type], Type)

  var fields: List[(Type, String)] = Nil
  var methods: List[(Function, Option[MethodInfo])] = Nil

  def addField(name: String, tpe: Type): Unit = {
    fields = fields ::: ((tpe, name) :: Nil)
  }

  def addMethod( name: String,
                 args: List[(Type, String)],
                 retTpe: Type,
                 overrides: Option[MethodInfo]
               ): Function = {
    val mtd = new Function(name, (TReference(className), "this") :: args, retTpe, prog.sp)
    methods = methods ::: ((mtd, overrides) :: Nil)

    mtd
  }

  // The Lazys MUST be evaluated after all classes exist!
  lazy val vTable: List[Function] = {
    val parentMethods = parentName.map(prog.classes).map(_.vTable).getOrElse(Nil)
    parentMethods ::: methods.filter(_._2.isEmpty).map(_._1)
  }

  lazy val vTableType: TVTable = {
    def tpe(size: Int) = TArray(size, TPointer(TFunction(List(TVariadic), TVoid)))
    def mangled(functionName: String) =
      "%s%s.%s".format(Prefixes.method, className, functionName)

    TVTable(
      className,
      tpe(vTable.length),
      vTable.map(fnc => Global(fnc.functionType, mangled(fnc.name)))
    )
  }

  lazy val allFields: List[(Type, String)] = {
    val parentFields =
      parentName.map(prog.classes).map(_.allFields).getOrElse(Nil)

    parentFields ::: fields
  }

  lazy val classType: TStruct = {
    val vTableField = TPointer(vTableType.tpe)

    TStruct(
      "%s%s".format(Prefixes.classType, className),
      vTableField :: allFields.map(_._1)
    )
  }
}
