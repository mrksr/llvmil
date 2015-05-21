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
    import java.text.Normalizer

    prog.sp.allStrings.map({
      case (string, identifier) =>
        val normalized = Normalizer.normalize(string, Normalizer.Form.NFD)
        val ascii = normalized.replaceAll("[^\\p{ASCII}]", "")
        "%s = internal constant [%d x i8] c\"%s\\00\"".format(
          identifier.name, ascii.length + 1, ascii
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

