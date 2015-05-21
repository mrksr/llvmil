package llvmil

import Prefixes._

object ILPrinter {
  def apply(prog: Program): Stream[String] = {
    val br = Stream("")

    strings(prog) append
    br append
    types(prog) append
    br
  }

  def strings(implicit prog: Program) = {
    prog.sp.allStrings.map({
      case (string, identifier) =>
        // TODO(mrksr): Non-Ascii Unicode characters
        "%s = internal constant [%d x i8] c\"%s\\00\"".format(
          identifier.name, string.length + 1, string
        )
    }).toStream
  }

  def types(implicit prog: Program) = {
    prog.classes.map({
      case (name, cls) =>
        cls.classType.declaration()
    }).toStream
  }
}

