package llvmil

import Types._
import Prefixes._
import ILInstructions._
import AbstractILInstructions._

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
    val mtd = new Function(name, (TThis, "this") :: args, retTpe, prog.sp)
    methods = methods ::: ((mtd, overrides) :: Nil)

    mtd
  }

  // The Lazys MUST be evaluated after all classes exist!
  lazy val vTable: List[(String, Function)] = {
    val (original, overriding) = methods.partition(_._2.isEmpty)

    val parentMethods = parentName.map(prog.classes).map(_.vTable).getOrElse(Nil)

    // NOTE(mrksr): This abomination finds out where in the vTable to put overriding functions.
    val (withOverrides, leftovers) =
      (parentMethods :\ (List[(String, Function)](), overriding.toSet)) {
        case ((cls, mtd), (acc, ovrs)) =>
          ovrs.find( ovr => {
            val (cld, Some((name, args, retTpe))) = ovr
            mtd.name == name && mtd.retTpe == retTpe && mtd.args.map(_._1).tail == args
          }) match {
            case None => ((cls, mtd) :: acc, ovrs)
            case Some(ovr) => ((className, ovr._1) :: acc, ovrs - ovr)
          }
      }

    if (!leftovers.isEmpty)
      sys.error("Error in vTable for %s, the following methods have nothing to override: %s".format(className, leftovers))

    withOverrides ::: original.map(_._1).map((className, _))
  }

  lazy val vTableType: TVTable = {
    def tpe(size: Int) = TArray(size, TPointer(TFunction(List(TVariadic), TVoid)))
    def mangled(className: String, functionName: String) =
      "%s%s.%s".format(Prefixes.method, className, functionName)

    TVTable(
      className,
      tpe(vTable.length),
      vTable.map({ case(cls, fnc) =>
          Bitcast(
            TPointer(TFunction(List(TVariadic), TVoid)),
            Global(TPointer(fnc.functionType), mangled(cls, fnc.name)))
      })
    )
  }

  lazy val allFields: List[(Type, String)] = {
    val parentFields =
      parentName.map(prog.classes).map(_.allFields).getOrElse(Nil)

    parentFields ::: fields
  }

  lazy val classType: TStruct = {
    val vTableField = TPointer(vTableType)

    TStruct(
      Some("%s%s".format(Prefixes.classType, className)),
      vTableField :: allFields.map(_._1)
    )
  }
}
