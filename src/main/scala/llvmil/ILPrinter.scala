package llvmil

import Prefixes._
import ILInstructions._
import Types._

object ILPrinter {
  private val br = Stream("")

  def apply(prog: Program): Stream[String] = {
    types(prog) append
    br append
    strings(prog) append
    br append
    functions(prog)
  }

  def mangledFunctionName(className: Option[String], functionName: String) =
    className match {
      case None => "%s%s".format(Prefixes.static, functionName)
      case Some(cls) => "%s%s.%s".format(Prefixes.method, cls, functionName)
    }

  def strings(prog: Program) = {
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

  def types(prog: Program): Stream[String] = {
    val internals = Stream(TString.declaration, TIntArray.declaration)

    val classTypes =
      prog.classes.map({
        case (name, cls) =>
          cls.classType.declaration
      }).toStream

    internals append br append classTypes
  }

  def functions(prog: Program): Stream[String] = {
    def function(className: Option[String])(fnc: Function): Stream[String] = {
      val name = "@%s".format(mangledFunctionName(className, fnc.name))
      val args = fnc.args.map({ case (t, n) => "%s %%%s".format(t.toIL, n) }).mkString(", ")

      val head = "define %s %s (%s) {".format(fnc.retTpe.toIL, name, args)
      val foot = "}"

      (Stream(head) /: fnc.instructions.map(instruction))(_ append _) append Stream(foot)
    }

    def flatten(xs: Iterable[Stream[String]]): Stream[String] =
      xs.reduceOption(_ append br append _).getOrElse(Stream.empty)

    val statics: Stream[String] = flatten(prog.statics.map(function(None)))
    val classFunctions: Stream[String] =
      flatten(
        prog.classes.values.map(c =>
            flatten(c.methods.map(_._1).map(function(Some(c.className))))
            )
          )

    statics append classFunctions
  }

  def instruction(il: ILInstruction) = br
}

