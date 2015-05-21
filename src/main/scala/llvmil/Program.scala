package llvmil

class Program extends ConstantPool {
  private val sp: StringPool = new StringPool()
  def string(const: String) = sp.string(const)

  private var classes: Map[String, Class] = Map()

  def addClass(className: String, parentName: Option[String]): Class = {
    val cls = new Class(className, parentName, sp)
    classes += (className -> cls)

    cls
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

