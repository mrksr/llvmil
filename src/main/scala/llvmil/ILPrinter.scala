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
    def flatten(xs: Iterable[Stream[String]]): Stream[String] =
      xs.reduceOption(_ append br append _).getOrElse(Stream.empty)

    def function(className: Option[String])(fnc: Function): Stream[String] = {
      val name = "@%s".format(mangledFunctionName(className, fnc.name))
      val args = fnc.args.map({ case (t, n) => "%s %%%s".format(t.toIL, n) }).mkString(", ")

      val head = "define %s %s(%s) {".format(fnc.retTpe.toIL, name, args)
      val foot = "}"

      val instructions = fnc.resolve(prog).map(instruction)
      val withIndent = instructions.map(line => "  %s".format(line)).toStream

      Stream(head) append withIndent append Stream(foot)
    }

    val statics = flatten(prog.statics.map(function(None)))
    val classFunctions =
      flatten(
        prog.classes.values.map(c =>
            flatten(c.methods.map(_._1).map(function(Some(c.className))))
            )
          )

    statics append classFunctions
  }

  def instruction(il: ILInstruction): String = il match {
    case Label(name) =>
      "%s:".format(name)
    case RetVoid =>
      "return void"
    case Ret(id) => 
      "return %s".format(identifier(id))
    case Br(dest) =>
      "br label %s".format(dest)
    case BrCond(cond, iftrue, iffalse) =>
      "br %s, label %s, label %s".format(identifier(cond), iftrue, iffalse)

    case Store(value, to) =>
      "store %s, %s".format(identifier(value), identifier(to))
    case Assign(to, rhs) =>
      operation(to, rhs)
  }

  def identifier(id: Identifier): String = id match {
    case Local(_ , _) | Global(_, _) | IConst(_, _) =>
      "%s %s".format(id.retType.toIL, id.name)

    case Bitcast(to, inner) => "bitcast %s to %s".format(identifier(inner), to.toIL)
    case PtrToInt(to, inner) => "ptrtoint %s to %s".format(identifier(inner), to.toIL)
  }

  def operation(to: Identifier, op: ILOperation): String = {
    val toAssign = op match {
      case id: Identifier =>
        identifier(id)

      case Add(lhs, rhs) =>
        "add %s %s, %s".format(to.retType.toIL, identifier(lhs), identifier(rhs))

      case Sub(lhs, rhs) =>
        "sub %s %s, %s".format(to.retType.toIL, identifier(lhs), identifier(rhs))

      case Mul(lhs, rhs) =>
        "mul %s %s, %s".format(to.retType.toIL, identifier(lhs), identifier(rhs))

      case Div(lhs, rhs) =>
        "sdiv %s %s, %s".format(to.retType.toIL, identifier(lhs), identifier(rhs))

      case Icmp(op, lhs, rhs) =>
        "icmp %s %s %s, %s".format(op, to.retType.toIL, identifier(lhs), identifier(rhs))

      case Alloca(tpe) =>
        "alloca %s".format(tpe.toIL)

      case Load(ptr) =>
        "load %s, %s".format(to.retType.toIL, identifier(ptr))

      case Call(func, args) =>
        val argList = args.map(identifier).mkString(", ")
        "call %s %s(%s)".format(to.retType.toIL, identifier(func), argList)

      case GetElementPtr(ptr, idxs) =>
        val indexes = idxs.map(IConst(TInt, _)).map(i => ", %s".format(identifier(i)))
        "getelementptr %s, %s%s".format(to.retType.toIL, identifier(ptr), indexes.mkString(""))
    }

    "%s = %s".format(to.name, toAssign)
  }
}

