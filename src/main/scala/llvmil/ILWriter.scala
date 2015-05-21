package llvmil

object ILPrinter {
  def apply(prog: Program): Stream[String] = {
    strings(prog)
  }

  def strings(implicit prog: Program) = {
    prog.sp.allStrings.map({
      case (string, identifier) =>
        "%s = constant [%d x i8] c\"%s\"".format(
          identifier.name, string.length, string
        )
    }).toStream
  }
}

