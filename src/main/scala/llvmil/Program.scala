package llvmil

class Program {
  private val scp: StringConstantPool = new StringConstantPool()
  private var classes: Map[String, Class] = Map()

  def addClass(className: String, parentName: Option[String]): Class = {
    val cls = new Class(className, parentName, scp)
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

