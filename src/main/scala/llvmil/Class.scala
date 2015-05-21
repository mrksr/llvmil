package llvmil

import Types._

class Class private[llvmil]( val className: String,
                             val parentName: Option[String],
                             sp: StringPool
                           ) {
  var fields: List[(Type, String)] = Nil
  var methods: List[Method] = Nil

  def addField(name: String, tpe: Type): Unit = {
    fields = fields ::: ((tpe, name) :: Nil)
  }

  def addMethod(name: String, args: List[(Type, String)], retTpe: Type): Method = {
    val mtd = new Method(name, args, retTpe, sp)
    methods = methods ::: (mtd :: Nil)

    mtd
  }
  def addMainMethod: Method = ???
}
