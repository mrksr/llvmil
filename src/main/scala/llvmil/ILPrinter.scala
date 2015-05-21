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

  def strings(prog: Program): Stream[String] = {
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
    def flattenWith(
                     func: (Stream[String], Stream[String]) => Stream[String]
                   )(
                     xs: Iterable[Stream[String]]
                   ): Stream[String] =
      xs.reduceOption(func).getOrElse(Stream.empty)

    val flattenBreak = flattenWith(_ append br append _) _

    def function(className: Option[String])(fnc: Function): Stream[String] = {
      val name = "@%s".format(mangledFunctionName(className, fnc.name))
      val args = fnc.args.map({ case (t, n) => "%s %%%s".format(t.toIL, n) }).mkString(", ")

      val head = "define %s %s(%s) {".format(fnc.retTpe.toIL, name, args)
      val foot = "}"

      val instructions = flattenWith(_ append _)(fnc.instructions.map(instruction))

      Stream(head) append instructions.map(line => "  %s".format(line)) append Stream(foot)
    }

    val statics = flattenBreak(prog.statics.map(function(None)))
    val classFunctions =
      flattenBreak(
        prog.classes.values.map(c =>
            flattenBreak(c.methods.map(_._1).map(function(Some(c.className))))
            )
          )

    statics append classFunctions
  }

  def instruction(il: ILInstruction): Stream[String] = il match {
    case Label(name) =>
      Stream("%s:".format(name))
    case RetVoid =>
      Stream("return void")
    case Ret(id) => 
      Stream("return %s".format(identifier(id)))
    case Br(dest) =>
      Stream("br label %s".format(dest))
    case BrCond(cond, iftrue, iffalse) =>
      Stream("br %s, label %s, label %s".format(identifier(cond), iftrue, iffalse))

    case Store(value, to) =>
      Stream("store %s, %s".format(identifier(value), identifier(to)))
    case Assign(to, rhs) =>
      operation(to, rhs)
  }

  def identifier(id: Identifier): String = id match {
    case Local(_ , _) | Global(_, _) | IConst(_, _) =>
      "%s %s".format(id.retType.toIL, id.name)

    case Bitcast(to, inner) => "bitcast %s to %s".format(identifier(inner), to.toIL)
    case PtrToInt(to, inner) => "ptrtoint %s to %s".format(identifier(inner), to.toIL)
  }

  def operation(to: Identifier, op: ILOperation): Stream[String] = {
    def assign(rhs: String): String = "%s = %s".format(to.name, rhs)

    op match {
      case id: Identifier =>
        Stream(assign(
          identifier(id)
        ))

      case Add(lhs, rhs) =>
        Stream(assign(
          "add %s %s, %s".format(to.retType, identifier(lhs), identifier(rhs))
        ))

      case Sub(lhs, rhs) =>
        Stream(assign(
          "sub %s %s, %s".format(to.retType, identifier(lhs), identifier(rhs))
        ))

      case Mul(lhs, rhs) =>
        Stream(assign(
          "mul %s %s, %s".format(to.retType, identifier(lhs), identifier(rhs))
        ))

      case Div(lhs, rhs) =>
        Stream(assign(
          "sdiv %s %s, %s".format(to.retType, identifier(lhs), identifier(rhs))
        ))

      case Icmp(op, lhs, rhs) =>
        Stream(assign(
          "icmp %s %s %s, %s".format(op, to.retType, identifier(lhs), identifier(rhs))
        ))

      case Alloca(tpe) =>
        Stream(assign(
          "alloca %s".format(tpe.toIL)
        ))

      case Load(ptr) =>
        Stream(assign(
          "load %s, %s".format(to.retType, identifier(ptr))
        ))

      case Call(func, args) =>
        val argList = args.map(identifier).mkString(", ")
        Stream(assign(
          "call %s %s(%s)".format(to.retType, identifier(func), argList)
        ))

      case AccessField(obj, name, tpe) => ???
      case VirtualResolve(obj, name, args, retTpe) => ???
    }
  }
}

