package llvmil

import Types._
import Prefixes._

class Class private[llvmil]( val className: String,
                             val parentName: Option[String],
                             prog: Program
                           ) {
  private type MethodInfo = (String, List[Type], Type)
  lazy val vTable: List[Method] = {
    val parentMethods = parentName.map(prog.classes).map(_.vTable).getOrElse(Nil)
    parentMethods ::: methods.filter(_._2.isEmpty).map(_._1)
  }

  lazy val classType: TStruct = {
    val vTableField = TVTable(vTable.length)
    val ownFields = fields.map(_._1)
    val parentFields = parentName.map(prog.classes).map(_.classType) match {
      case None => Nil
      case Some(TStruct(_, fields)) => fields
    }

    TStruct(
      "%s%s".format(Prefixes.classType, className),
      vTableField :: parentFields ::: ownFields
    )
  }

  var fields: List[(Type, String)] = Nil
  var methods: List[(Method, Option[MethodInfo])] = Nil

  def addField(name: String, tpe: Type): Unit = {
    fields = fields ::: ((tpe, name) :: Nil)
  }

  def addMethod( name: String,
                 args: List[(Type, String)],
                 retTpe: Type,
                 overrides: Option[MethodInfo]
               ): Method = {
    val mtd = new Method(name, args, retTpe, prog.sp)
    methods = methods ::: ((mtd, overrides) :: Nil)

    mtd
  }
  def addMainMethod: Method = ???
}
