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

  def strings(prog: Program): Stream[String] = {
    prog.sp.allStrings.map({
      case (string, identifier) =>
        val ascii = StringPool.normalize(string)
        "%s = internal constant [%d x i8] c\"%s\\00\"".format(
          identifier.name, ascii.length + 1, ascii
        )
    }).toStream
  }

  def types(prog: Program): Stream[String] = {
    val runtime = {
      import Runtime._

      val internals = TString :: TIntArray :: allRuntimeFunctions
      internals.map({
        case TPointer(tpe: TStruct) =>
          tpe.declaration
        case Global(TPointer(fnc: TFunction), name) =>
          fnc.declaration(name)
      }).toStream
    }

    val classTypes =
      prog.classes.map({
        case (name, cls) =>
          cls.classType.declaration
      }).toStream

    val vTables =
      prog.classes.map({
        case (name, cls) =>
          cls.vTableType.instantiation
      }).toStream

    runtime append
    br append classTypes append br append vTables
  }

  def functions(prog: Program): Stream[String] = {
    def mangledStaticName(functionName: String) =
      // NOTE(mrksr): We do not mangle them to allow for magic names like "main"
      functionName
    def mangledClassName(className: String, functionName: String) =
      "%s%s.%s".format(Prefixes.method, className, functionName)

    def flatten(xs: Iterable[Stream[String]]): Stream[String] =
      xs.reduceOption(_ append br append _).getOrElse(Stream.empty)

    def function(functionName: String)(cls: Option[Class], fnc: Function): Stream[String] = {
      val name = "@%s".format(functionName)
      val args = fnc.args.map({ case (t, n) => "%s %%%s".format(t.toIL, n) }).mkString(", ")

      val head = "define %s %s(%s) {".format(fnc.retTpe.toIL, name, args)
      val foot = "}"

      val instructions = fnc.resolve(prog, cls).map(instruction)
      val withIndent = instructions.map(line => "  %s".format(line)).toStream

      Stream(head) append withIndent append Stream(foot)
    }

    val statics =
      flatten(
        prog.statics.map(fnc => function(mangledStaticName(fnc.name))(None, fnc)
      ))
    val classFunctions =
      flatten(
        prog.classes.values.map(c =>
          flatten(c.methods.map(_._1).map(fnc =>
              function(mangledClassName(c.className, fnc.name))(Some(c), fnc)
          ))
        )
      )

    statics append br append classFunctions
  }

  def instruction(il: ILInstruction): String = il match {
    case Label(name) =>
      "%s:".format(name)
    case RetVoid =>
      "ret void"
    case Ret(id) =>
      "ret %s".format(identifier(id))
    case Br(dest) =>
      "br label %s".format(dest)
    case BrCond(cond, iftrue, iffalse) =>
      "br %s, label %s, label %s".format(identifier(cond), iftrue, iffalse)

    case Store(value, to) =>
      "store %s, %s".format(identifier(value), identifier(to))
    case Assign(to, rhs) =>
      operation(to, rhs)

    case CallVoid(func, args) =>
      val argList = args.map(identifier).mkString(", ")
      "call void %s(%s)".format(func.name, argList)
  }

  def identifier(id: Identifier): String = id match {
    case Null | This | Local(_ , _) | Global(_, _) | IConst(_, _) =>
      "%s %s".format(id.retType.toIL, id.name)

    case Bitcast(to, inner) => "%s bitcast(%s to %s)".format(to.toIL, identifier(inner), to.toIL)
    case PtrToInt(to, inner) => "%s ptrtoint(%s to %s)".format(to.toIL, identifier(inner), to.toIL)
  }

  def operation(to: Identifier, op: ILOperation): String = {
    val toAssign = op match {
      case id: Identifier => id match {
        // NOTE(mrksr): We have to repeat those here since if they are to be
        // interpreted as operations, they do not need a type.
        case Null | This | Local(_ , _) | Global(_, _) =>
          sys.error("A raw identifier is no valid operation.")

        case IConst(tpe, i) =>
          "%s %s".format(tpe.toIL, i.toString)

        case Bitcast(to, inner) =>
          "bitcast %s to %s".format(identifier(inner), to.toIL)
        case PtrToInt(to, inner) =>
          "ptrtoint %s to %s".format(identifier(inner), to.toIL)
      }

      case Add(lhs, rhs) =>
        "add %s %s, %s".format(to.retType.toIL, lhs.name, rhs.name)

      case Sub(lhs, rhs) =>
        "sub %s %s, %s".format(to.retType.toIL, lhs.name, rhs.name)

      case Mul(lhs, rhs) =>
        "mul %s %s, %s".format(to.retType.toIL, lhs.name, rhs.name)

      case SDiv(lhs, rhs) =>
        "sdiv %s %s, %s".format(to.retType.toIL, lhs.name, rhs.name)

      case Icmp(op, lhs, rhs) =>
        "icmp %s %s %s, %s".format(op, lhs.retType.toIL, lhs.name, rhs.name)

      case Alloca(tpe) =>
        "alloca %s".format(tpe.toIL)

      case Load(ptr) =>
        "load %s".format(identifier(ptr))

      case Call(func, args) =>
        val argList = args.map(identifier).mkString(", ")
        "call %s %s(%s)".format(to.retType.toIL, func.name, argList)

      case GetElementPtr(ptr, idxs) =>
        val indexes = idxs.map(i => ", %s".format(identifier(i)))
        val TPointer(inner) = ptr.retType
        "getelementptr %s%s".format(identifier(ptr), indexes.mkString(""))
    }

    "%s = %s".format(to.name, toAssign)
  }
}
