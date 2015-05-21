package llvmil

import Types._
import Prefixes._

class Class private[llvmil]( val className: String,
                             val parentName: Option[String],
                             prog: Program
                           ) {
  private type MethodInfo = (String, List[Type], Type)
  lazy val vTable: List[Function] = {
    val parentMethods = parentName.map(prog.classes).map(_.vTable).getOrElse(Nil)
    parentMethods ::: methods.filter(_._2.isEmpty).map(_._1)
  }

  lazy val allFieldTypes: List[Type] = {
    val ownFields = fields.map(_._1)
    val parentFields =
      parentName.map(prog.classes).map(_.allFieldTypes).getOrElse(Nil)

    parentFields ::: ownFields
  }

  lazy val classType: TStruct = {
    val vTableField = TPointer(TVTable(vTable.length))

    TStruct(
      "%s%s".format(Prefixes.classType, className),
      vTableField :: allFieldTypes
    )
  }

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
    val mtd = new Function(name, args, retTpe, prog.sp)
    methods = methods ::: ((mtd, overrides) :: Nil)

    mtd
  }
}
