package llvmil

import Types._

class Program extends ConstantPool {
  private[llvmil] val sp: StringPool = new StringPool()
  def string(const: String) = sp.string(const)

  private[llvmil] var classes: Map[String, Class] = Map()
  private[llvmil] var statics: List[Function] = Nil

  def addClass(className: String, parentName: Option[String]): Class = {
    val cls = new Class(className, parentName, this)
    classes += (className -> cls)

    cls
  }

  def addStatic( name: String,
                 args: List[(Type, String)],
                 retTpe: Type
               ): Function = {
    val fnc = new Function(name, args, retTpe, sp)
    statics = statics ::: (fnc :: Nil)

    fnc
  }

  def writeToFile(fileName: String) = {
    /*
     * FIXME(mrksr): Do the magic
     * Here we need to:
     *  - Resolve the Constant Pool
     *  - Create Class types (for fields)
     *  - Create VTables
     *  - Resolve Virtual Lookups
     * 
     * This is a Map from AIL to IL
     */
    ??? 
  }
}

